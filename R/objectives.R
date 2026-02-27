#' Retrieve Cognitive3D objective results
#'
#' Queries the Cognitive3D objectives API and returns a tidy tibble of
#' objective success/failure results, enriched with objective names from
#' project metadata. Optionally includes step-level detail.
#'
#' This function coordinates three API endpoints:
#' 1. GET objectives metadata (names, versions, step definitions)
#' 2. POST objective result queries (success/failure counts)
#' 3. POST objective step result queries (per-step detail, if requested)
#'
#' @param project_id Integer. Project ID. Uses the default from
#'   `c3d_project()` if `NULL`.
#' @param objective_id Integer or NULL. Filter to a single objective.
#' @param objective_version_id Integer or NULL. Filter to a single version.
#' @param slice_by Character. `"objective_version"` (default) or `"objective"`.
#'   Controls result granularity. Version mode includes a `version_number`
#'   column matching the dashboard's "Version N" label.
#' @param include_steps Logical. If `TRUE`, also fetch step-level results
#'   for each objective version. Returns a list of two tibbles instead of
#'   a single tibble.
#' @param exclude_test Logical. If `TRUE` (default), filter out sessions
#'   tagged as test.
#' @param exclude_idle Logical. If `TRUE` (default), filter out sessions
#'   tagged as junk/idle.
#' @param start_date Date, POSIXct, or `"YYYY-MM-DD"` string. Start of date range.
#'   If neither `start_date` nor `end_date` is provided, defaults to 30 days ago.
#' @param end_date Date, POSIXct, or `"YYYY-MM-DD"` string. End of date range.
#'   Defaults to now if `start_date` is set but `end_date` is not.
#'
#' @return A tibble. If `include_steps = FALSE`, objective-level results. If
#'   `include_steps = TRUE`, step-level results.
#' @export
#'
#' @examples
#' \dontrun{
#' c3d_auth()
#' c3d_project(4460)
#'
#' # Get objective results (per version by default)
#' results <- c3d_objective_results()
#'
#' # Get step-level detail
#' steps <- c3d_objective_results(include_steps = TRUE)
#'
#' # Filter to a specific objective
#' results <- c3d_objective_results(objective_id = 268)
#' }
c3d_objective_results <- function(
  project_id = NULL,
  objective_id = NULL,
  objective_version_id = NULL,
  slice_by = c("objective_version", "objective"),
  include_steps = FALSE,
  exclude_test = TRUE,
  exclude_idle = TRUE,
  start_date = NULL,
  end_date = NULL
) {
  # --- Input validation ---
  if (is.null(project_id)) {
    project_id <- get_project_id()
  }
  project_id <- as.integer(project_id)
  if (is.na(project_id)) {
    cli::cli_abort("{.arg project_id} must be a valid integer.")
  }

  slice_by <- match.arg(slice_by)

  if (!is.null(objective_id)) {
    objective_id <- as.integer(objective_id)
    if (is.na(objective_id)) {
      cli::cli_abort("{.arg objective_id} must be a valid integer.")
    }
  }
  if (!is.null(objective_version_id)) {
    objective_version_id <- as.integer(objective_version_id)
    if (is.na(objective_version_id)) {
      cli::cli_abort("{.arg objective_version_id} must be a valid integer.")
    }
  }

  # --- Step 1: Fetch objectives metadata ---
  cli::cli_progress_step("Fetching objective metadata")
  metadata <- fetch_objectives_metadata(project_id)

  if (length(metadata$objectives) == 0) {
    return(empty_objective_results(slice_by, include_steps))
  }

  # --- Step 2: Fetch objective results ---
  # Default to last 30 days when no date range specified
  if (is.null(start_date) && is.null(end_date)) {
    start_date <- Sys.time() - as.difftime(30, units = "days")
    end_date <- Sys.time()
    cli::cli_alert_info("No date range specified. Defaulting to last 30 days ({.val {format(start_date, '%Y-%m-%d')}} to {.val {format(end_date, '%Y-%m-%d')}}).")
  }

  session_filters <- build_session_filters(
    exclude_test = exclude_test,
    exclude_idle = exclude_idle,
    start_date = start_date,
    end_date = end_date
  )

  results_body <- list(
    projectId = project_id,
    sliceByObjective = (slice_by == "objective"),
    sliceByObjectiveVersion = (slice_by == "objective_version"),
    sessionFilters = session_filters,
    eventFilters = list(),
    separableEventFilters = list(),
    userFilters = list()
  )
  if (!is.null(objective_id)) {
    results_body$objectiveId <- objective_id
  }
  if (!is.null(objective_version_id)) {
    results_body$objectiveVersionId <- objective_version_id
  }

  cli::cli_progress_step("Querying objective results")
  raw_results <- c3d_request(
    "/v0/datasets/objectives/objectiveResultQueries",
    results_body
  )

  objectives_tbl <- parse_objective_results(raw_results, slice_by, metadata)

  # --- Step 3: Optionally fetch step results ---
  if (!isTRUE(include_steps)) {
    return(objectives_tbl)
  }

  # Resolve dynamic object names in step details
  cli::cli_progress_step("Resolving dynamic object names")
  objects_lookup <- fetch_objects_lookup(project_id)
  metadata$components <- resolve_step_details(metadata$components, objects_lookup)

  # Determine which version IDs to fetch steps for
  if (slice_by == "objective_version") {
    version_ids <- objectives_tbl$objective_version_id
  } else {
    # When sliced by objective, resolve all version IDs from metadata
    obj_ids_in_results <- objectives_tbl$objective_id
    version_ids <- metadata$versions$objective_version_id[
      metadata$versions$objective_id %in% obj_ids_in_results
    ]
  }

  # Filter by objective_version_id param if provided
  if (!is.null(objective_version_id)) {
    version_ids <- intersect(version_ids, objective_version_id)
  }

  steps_tbl <- fetch_all_step_results(
    project_id = project_id,
    version_ids = version_ids,
    session_filters = session_filters,
    metadata = metadata
  )

  steps_tbl
}


# --- Internal helpers ---

#' Fetch and parse objectives metadata for a project
#' @keywords internal
fetch_objectives_metadata <- function(project_id) {
  endpoint <- paste0("/v0/projects/", project_id, "/objectives")
  raw <- c3d_get(endpoint)

  objectives <- tibble::tibble(
    objective_id = integer(0),
    objective_name = character(0),
    objective_enabled = logical(0)
  )

  versions <- tibble::tibble(
    objective_version_id = integer(0),
    objective_id = integer(0),
    version_is_active = logical(0),
    version_number = integer(0)
  )

  components <- tibble::tibble(
    objective_version_id = integer(0),
    step_number = integer(0),
    step_type = character(0),
    step_detail = character(0),
    step_name = character(0),
    is_step = logical(0)
  )

  if (length(raw) == 0) {
    return(list(objectives = objectives, versions = versions,
                components = components))
  }

  obj_rows <- list()
  ver_rows <- list()
  comp_rows <- list()

  for (obj in raw) {
    obj_rows <- c(obj_rows, list(list(
      objective_id = as.integer(obj$id),
      objective_name = as.character(obj$name),
      objective_enabled = as.logical(obj$enabled %||% TRUE)
    )))

    for (ver in (obj$objectiveVersions %||% list())) {
      ver_rows <- c(ver_rows, list(list(
        objective_version_id = as.integer(ver$id),
        objective_id = as.integer(obj$id),
        version_is_active = as.logical(ver$isActive %||% FALSE)
      )))

      for (comp in (ver$objectiveComponents %||% list())) {
        comp_rows <- c(comp_rows, list(list(
          objective_version_id = as.integer(ver$id),
          step_number = as.integer(comp$sequenceNumber),
          step_type = as.character(comp$type %||% NA_character_),
          step_detail = derive_step_detail(comp),
          step_name = as.character(comp$name %||% NA_character_),
          is_step = as.logical(comp$isStep %||% FALSE)
        )))
      }
    }
  }

  if (length(obj_rows) > 0) {
    objectives <- tibble::as_tibble(do.call(rbind, lapply(obj_rows, as.data.frame,
                                                          stringsAsFactors = FALSE)))
    objectives$objective_id <- as.integer(objectives$objective_id)
    objectives$objective_enabled <- as.logical(objectives$objective_enabled)
  }

  if (length(ver_rows) > 0) {
    versions <- tibble::as_tibble(do.call(rbind, lapply(ver_rows, as.data.frame,
                                                        stringsAsFactors = FALSE)))
    versions$objective_version_id <- as.integer(versions$objective_version_id)
    versions$objective_id <- as.integer(versions$objective_id)
    versions$version_is_active <- as.logical(versions$version_is_active)

    # Derive version_number from ascending objective_version_id within each objective
    versions$version_number <- ave(
      versions$objective_version_id,
      versions$objective_id,
      FUN = function(x) rank(x, ties.method = "first")
    )
    versions$version_number <- as.integer(versions$version_number)
  }

  if (length(comp_rows) > 0) {
    components <- tibble::as_tibble(do.call(rbind, lapply(comp_rows, as.data.frame,
                                                          stringsAsFactors = FALSE)))
    components$objective_version_id <- as.integer(components$objective_version_id)
    components$step_number <- as.integer(components$step_number)
    components$is_step <- as.logical(components$is_step)
  }

  list(objectives = objectives, versions = versions, components = components)
}


#' Parse objective results response and join with metadata
#' @keywords internal
parse_objective_results <- function(raw_results, slice_by, metadata) {
  if (length(raw_results) == 0) {
    return(empty_objective_results(slice_by, include_steps = FALSE))
  }

  rows <- list()
  for (r in raw_results) {
    rows <- c(rows, list(list(
      objective_id = as.integer(r$objectiveId %||% NA_integer_),
      objective_version_id = as.integer(r$objectiveVersionId %||% NA_integer_),
      succeeded = as.integer(r$succeeded %||% 0L),
      failed = as.integer(r$failed %||% 0L)
    )))
  }
  results <- tibble::as_tibble(do.call(rbind, lapply(rows, as.data.frame,
                                                     stringsAsFactors = FALSE)))
  results$objective_id <- as.integer(results$objective_id)
  results$objective_version_id <- as.integer(results$objective_version_id)
  results$succeeded <- as.integer(results$succeeded)
  results$failed <- as.integer(results$failed)

  # Compute completion rate
  total <- results$succeeded + results$failed
  results$completion_rate <- ifelse(total == 0, NA_real_,
                                    results$succeeded / total)

  if (slice_by == "objective") {
    # Join objective_name from metadata
    obj_meta <- metadata$objectives[, c("objective_id", "objective_name")]
    results <- merge(results, obj_meta, by = "objective_id", all.x = TRUE)

    results[, c("objective_id", "objective_name", "succeeded", "failed",
                "completion_rate")]
  } else {
    # slice_by == "objective_version"
    # Join objective_id, version_is_active, version_number from versions metadata
    ver_meta <- metadata$versions[, c("objective_version_id", "objective_id",
                                      "version_is_active", "version_number")]
    results <- merge(results, ver_meta, by = "objective_version_id", all.x = TRUE)

    # Prefer objective_id from versions metadata over the response
    if ("objective_id.y" %in% names(results)) {
      results$objective_id <- ifelse(is.na(results$objective_id.y),
                                     results$objective_id.x,
                                     results$objective_id.y)
      results$objective_id.x <- NULL
      results$objective_id.y <- NULL
    }

    # Join objective_name from objectives metadata
    obj_meta <- metadata$objectives[, c("objective_id", "objective_name")]
    results <- merge(results, obj_meta, by = "objective_id", all.x = TRUE)

    results[, c("objective_id", "objective_name", "objective_version_id",
                "version_number", "version_is_active", "succeeded", "failed",
                "completion_rate")]
  }
}


#' Fetch step results for multiple objective versions
#' @keywords internal
fetch_all_step_results <- function(project_id, version_ids, session_filters,
                                   metadata) {
  if (length(version_ids) == 0) {
    return(empty_steps_tibble())
  }

  all_steps <- list()

  cli::cli_progress_bar(
    "Fetching step results",
    total = length(version_ids),
    clear = TRUE
  )

  for (vid in version_ids) {
    body <- list(
      projectId = project_id,
      objectiveVersionId = as.integer(vid),
      sessionFilters = session_filters,
      eventFilters = list(),
      separableEventFilters = list(),
      userFilters = list()
    )

    raw_steps <- c3d_request(
      "/v0/datasets/objectives/objectiveStepResultQueries",
      body
    )

    cli::cli_progress_update()

    if (length(raw_steps) == 0) next

    for (s in raw_steps) {
      all_steps <- c(all_steps, list(list(
        objective_version_id = as.integer(vid),
        step_number = as.integer(s$step),
        succeeded = as.integer(s$succeeded %||% 0L),
        failed = as.integer(s$failed %||% 0L),
        avg_completion_time_s = as.double(s$averageStepCompletionTime %||% NA_real_) / 1000,
        avg_step_duration_s = as.double(s$averageStepDuration %||% NA_real_) / 1000
      )))
    }
  }

  cli::cli_progress_done()

  if (length(all_steps) == 0) {
    return(empty_steps_tibble())
  }

  steps <- tibble::as_tibble(do.call(rbind, lapply(all_steps, as.data.frame,
                                                   stringsAsFactors = FALSE)))
  steps$objective_version_id <- as.integer(steps$objective_version_id)
  steps$step_number <- as.integer(steps$step_number)
  steps$succeeded <- as.integer(steps$succeeded)
  steps$failed <- as.integer(steps$failed)

  # Compute step completion rate
  total <- steps$succeeded + steps$failed
  steps$step_completion_rate <- ifelse(total == 0, NA_real_,
                                       steps$succeeded / total)

  # Join version → objective_id, version_number from metadata
  ver_meta <- metadata$versions[, c("objective_version_id", "objective_id",
                                    "version_number")]
  steps <- merge(steps, ver_meta, by = "objective_version_id", all.x = TRUE)

  # Join objective_name from metadata
  obj_meta <- metadata$objectives[, c("objective_id", "objective_name")]
  steps <- merge(steps, obj_meta, by = "objective_id", all.x = TRUE)

  # Join step component metadata (only isStep == TRUE components)
  step_comps <- metadata$components[metadata$components$is_step == TRUE,
                                    c("objective_version_id", "step_number",
                                      "step_type", "step_detail", "step_name")]
  steps <- merge(steps, step_comps,
                 by = c("objective_version_id", "step_number"),
                 all.x = TRUE)

  # Select, order columns, and sort by objective then step
  steps <- steps[, c("objective_id", "objective_name", "objective_version_id",
                      "version_number", "step_number", "step_type",
                      "step_detail", "step_name",
                      "succeeded", "failed", "step_completion_rate",
                      "avg_completion_time_s", "avg_step_duration_s")]
  steps[order(steps$objective_id, steps$step_number), ]
}


#' Return empty tibbles with correct column structure
#' @keywords internal
empty_objective_results <- function(slice_by, include_steps) {
  obj_tbl <- if (slice_by == "objective") {
    tibble::tibble(
      objective_id = integer(0),
      objective_name = character(0),
      succeeded = integer(0),
      failed = integer(0),
      completion_rate = double(0)
    )
  } else {
    tibble::tibble(
      objective_id = integer(0),
      objective_name = character(0),
      objective_version_id = integer(0),
      version_number = integer(0),
      version_is_active = logical(0),
      succeeded = integer(0),
      failed = integer(0),
      completion_rate = double(0)
    )
  }

  if (isTRUE(include_steps)) {
    return(empty_steps_tibble())
  }
  obj_tbl
}


#' @keywords internal
empty_steps_tibble <- function() {
  tibble::tibble(
    objective_id = integer(0),
    objective_name = character(0),
    objective_version_id = integer(0),
    version_number = integer(0),
    step_number = integer(0),
    step_type = character(0),
    step_detail = character(0),
    step_name = character(0),
    succeeded = integer(0),
    failed = integer(0),
    step_completion_rate = double(0),
    avg_completion_time_s = double(0),
    avg_step_duration_s = double(0)
  )
}


#' Replace dynamic object sdkIds in step_detail with friendly names
#'
#' For gazestep, fixationstep, and mediapointstep rows, splits the
#' comma-separated sdkIds and replaces each with the friendly name from
#' the objects lookup. Unmatched sdkIds are kept as-is.
#'
#' @param components A tibble of objective components (from metadata).
#' @param objects_lookup Named character vector (sdkId -> name).
#' @return The components tibble with resolved step_detail values.
#' @keywords internal
resolve_step_details <- function(components, objects_lookup) {
  if (nrow(components) == 0 || length(objects_lookup) == 0) {
    return(components)
  }

  object_types <- c("gazestep", "fixationstep", "mediapointstep")
  mask <- components$step_type %in% object_types & !is.na(components$step_detail)

  if (!any(mask)) {
    return(components)
  }

  components$step_detail[mask] <- vapply(
    components$step_detail[mask],
    function(detail) {
      ids <- trimws(strsplit(detail, ",")[[1]])
      resolved <- ifelse(
        ids %in% names(objects_lookup),
        unname(objects_lookup[ids]),
        ids
      )
      paste(resolved, collapse = ", ")
    },
    character(1)
  )

  components
}


#' Derive step_detail from a component based on its type
#' @param comp A component list from the API response.
#' @return Character scalar: the type-appropriate identifier, or NA.
#' @keywords internal
derive_step_detail <- function(comp) {
  type <- comp$type %||% NA_character_
  if (is.na(type)) return(NA_character_)

  switch(type,
    eventstep = as.character(comp$eventName %||% NA_character_),
    exitpollstep = as.character(comp$exitpollQuestionSetId %||% NA_character_),
    gazestep = ,
    fixationstep = ,
    mediapointstep = {
      ids <- comp$dynamicObjectIds
      if (is.null(ids) || length(ids) == 0) {
        NA_character_
      } else {
        paste(vapply(ids, as.character, character(1)), collapse = ", ")
      }
    },
    NA_character_
  )
}
