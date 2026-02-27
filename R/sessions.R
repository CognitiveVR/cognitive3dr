#' Retrieve Cognitive3D session data
#'
#' Queries the Cognitive3D sessions API and returns a tidy tibble of session
#' records with metrics and properties.
#'
#' @param project_id Integer. Project ID. Uses the default from
#'   `c3d_project()` if `NULL`.
#' @param n Integer. Maximum 500. For `session_type = "project"`, the number
#'   of rows returned. For `session_type = "scene"`, the cap is applied
#'   **per scene** so every scene is represented—the output may have more
#'   than `n` rows because the same session appears once per scene visited.
#' @param session_type Character. `"project"` (default) returns one row per
#'   session. `"scene"` returns one row per session-scene combination, with
#'   metrics scoped to each scene. Scene mode defaults to querying only the
#'   **latest version** of each scene; use `scene_version_id` to target a
#'   specific version.
#' @param scene_id Character or NULL. UUID of a specific scene to filter to.
#'   Only used when `session_type = "scene"`.
#' @param scene_version_id Integer or NULL. Specific scene version ID to
#'   filter to. Only used when `session_type = "scene"`. When provided,
#'   overrides the default latest-version-per-scene behaviour.
#' @param start_date Date, POSIXct, or `"YYYY-MM-DD"` string. Start of date range.
#'   If neither `start_date` nor `end_date` is provided, defaults to 30 days ago.
#' @param end_date Date, POSIXct, or `"YYYY-MM-DD"` string. End of date range.
#'   Defaults to now if `start_date` is set but `end_date` is not.
#'
#' @details
#' ## Column naming
#' Top-level API fields use snake_case (e.g. `session_id`, `device_id`).
#' Properties collected by Cognitive3D retain the `c3d_` prefix
#' (e.g. `c3d_app_name`, `c3d_metrics_fps_score`).
#'
#' ## Scene sessions
#' When `session_type = "scene"`, additional columns `scene_id`,
#' `scene_version_id`, and `scene_name` are included. The same `session_id`
#' may appear multiple times (once per scene visited). By default, only the
#' latest version of each scene is queried to avoid excessive API calls for
#' projects with many scene versions.
#' @param exclude_test Logical. If `TRUE` (default), filter out sessions tagged
#'   as test.
#' @param exclude_idle Logical. If `TRUE` (default), filter out sessions tagged
#'   as junk/idle.
#' @param min_duration Numeric. Minimum session duration in seconds. Converted
#'   to milliseconds for the API.
#' @param compact Logical. If `TRUE` (default), return ~40 key columns. If
#'   `FALSE`, return all columns.
#'
#' @return A tibble of session data.
#' @export
#'
#' @examples
#' \dontrun{
#' c3d_auth()
#' c3d_project(4460)
#'
#' # Get latest 100 sessions
#' sessions <- c3d_sessions(n = 100)
#'
#' # Filter by date range
#' sessions <- c3d_sessions(start_date = "2026-01-01", end_date = "2026-02-01")
#'
#' # Include test sessions, minimum 60 second duration
#' sessions <- c3d_sessions(exclude_test = FALSE, min_duration = 60)
#'
#' # Get all columns
#' sessions <- c3d_sessions(compact = FALSE)
#'
#' # Scene sessions (latest version per scene)
#' scene_data <- c3d_sessions(session_type = "scene")
#'
#' # Scene sessions for a specific scene
#' scene_data <- c3d_sessions(
#'   session_type = "scene",
#'   scene_id = "de704574-b03f-424e-be87-4985f85ed2e8"
#' )
#' }
c3d_sessions <- function(
  project_id = NULL,
  n = 500,
  session_type = c("project", "scene"),
  scene_id = NULL,
  scene_version_id = NULL,
  start_date = NULL,
  end_date = NULL,
  exclude_test = TRUE,
  exclude_idle = TRUE,
  min_duration = NULL,
  compact = TRUE
) {
  # --- Input validation ---
  if (is.null(project_id)) {
    project_id <- get_project_id()
  }
  project_id <- as.integer(project_id)
  if (is.na(project_id)) {
    cli::cli_abort("{.arg project_id} must be a valid integer.")
  }

  n <- as.integer(n)
  if (is.na(n) || n < 1) {
    cli::cli_abort("{.arg n} must be a positive integer.")
  }
  if (n > 500L) {
    cli::cli_abort("{.arg n} must be <= 500. Got {.val {n}}.")
  }

  session_type <- match.arg(session_type)

  if (!is.null(scene_id)) {
    if (!is.character(scene_id) || length(scene_id) != 1) {
      cli::cli_abort("{.arg scene_id} must be a single character string (UUID).")
    }
  }
  if (!is.null(scene_version_id)) {
    scene_version_id <- as.integer(scene_version_id)
    if (is.na(scene_version_id)) {
      cli::cli_abort("{.arg scene_version_id} must be a valid integer.")
    }
  }
  if (session_type == "project" && (!is.null(scene_id) || !is.null(scene_version_id))) {
    cli::cli_abort(
      "{.arg scene_id} and {.arg scene_version_id} can only be used with {.code session_type = \"scene\"}."
    )
  }

  # --- Default to last 30 days when no date range specified ---
  if (is.null(start_date) && is.null(end_date)) {
    start_date <- Sys.time() - as.difftime(30, units = "days")
    end_date <- Sys.time()
    cli::cli_alert_info("No date range specified. Defaulting to last 30 days ({.val {format(start_date, '%Y-%m-%d')}} to {.val {format(end_date, '%Y-%m-%d')}}).")
  }

  # --- Build session filters ---
  session_filters <- build_session_filters(
    exclude_test = exclude_test,
    exclude_idle = exclude_idle,
    start_date = start_date,
    end_date = end_date
  )

  # Duration filter (sessions-only, not in shared helper)
  if (!is.null(min_duration)) {
    min_duration <- as.numeric(min_duration)
    if (is.na(min_duration) || min_duration < 0) {
      cli::cli_abort("{.arg min_duration} must be a non-negative number (seconds).")
    }
    session_filters <- c(session_filters, list(list(
      field = list(fieldName = "duration", fieldParent = "session"),
      op = "gte",
      value = min_duration * 1000
    )))
  }

  # --- Fetch sessions ---
  if (session_type == "project") {
    all_results <- paginate_sessions(
      project_id = project_id,
      session_filters = session_filters,
      max_sessions = n,
      page_limit = 100L,
      include_events = FALSE
    )

    if (length(all_results) == 0) {
      cli::cli_alert_info("No sessions found.")
      return(tibble::tibble())
    }

    parse_sessions(all_results, compact = compact)

  } else {
    # Scene sessions
    result <- fetch_scene_sessions(
      project_id = project_id,
      session_filters = session_filters,
      max_sessions = n,
      scene_id = scene_id,
      scene_version_id = scene_version_id
    )

    if (length(result$sessions) == 0) {
      cli::cli_alert_info("No scene sessions found.")
      return(tibble::tibble())
    }

    df <- parse_sessions(result$sessions, compact = compact)
    join_scene_names(df, result$scenes_meta$lookup)
  }
}

# Internal: shared pagination loop for the sessions endpoint
#
# Used by c3d_sessions() and c3d_events(). Fetches paginated session results
# from the API, optionally including events.
#
# @param project_id Integer. Project ID.
# @param session_filters List. Built by build_session_filters() + min_duration.
# @param max_sessions Integer. Hard cap on total sessions.
# @param page_limit Integer. Number of sessions per API page.
# @param include_events Logical. If TRUE, include events in the response.
# @return A list of raw session objects from the API.
# @keywords internal
paginate_sessions <- function(project_id, session_filters, max_sessions,
                              page_limit, include_events = FALSE,
                              session_type = "project",
                              scene_id = NULL, version_id = NULL) {
  endpoint <- "/v0/datasets/sessions/paginatedListQueries"
  all_results <- list()
  remaining <- max_sessions
  page <- 0L

  cli::cli_progress_bar(
    "Fetching sessions",
    total = max_sessions,
    clear = TRUE
  )

  entity_filters <- list(projectId = project_id)
  if (!is.null(scene_id)) {
    entity_filters$sceneId <- scene_id
  }
  if (!is.null(version_id)) {
    entity_filters$versionId <- version_id
  }

  repeat {
    page_size <- min(remaining, page_limit)

    body <- list(
      page = page,
      limit = page_size,
      sort = "desc",
      orderBy = list(fieldName = "date", fieldParent = "session"),
      entityFilters = entity_filters,
      sessionFilters = session_filters,
      eventFilters = list(),
      objectiveFilters = list(),
      userFilters = list(),
      sessionType = session_type
    )

    if (isTRUE(include_events)) {
      body$includeEvents <- TRUE
    }

    resp <- c3d_request(endpoint, body)

    results <- resp[["results"]]
    if (is.null(results) || length(results) == 0) {
      break
    }

    all_results <- c(all_results, results)
    remaining <- remaining - length(results)
    cli::cli_progress_update(set = length(all_results))

    # Stop if we have enough or no more pages
    total_pages <- resp[["pages"]]
    current_page <- resp[["currentPage"]]
    if (remaining <= 0 || is.null(total_pages) || (current_page + 1) >= total_pages) {
      break
    }

    page <- page + 1L
  }

  cli::cli_progress_done()

  # Trim to exactly max_sessions
  if (length(all_results) > max_sessions) {
    all_results <- all_results[seq_len(max_sessions)]
  }

  all_results
}

# Internal: fetch scene sessions across all (or filtered) scene versions
#
# Fetches scenes metadata, then iterates over scene versions calling
# paginate_sessions() for each. Each scene version gets the full `n` budget
# so that all scenes are represented in the output—the combined result is
# the project session picture split by scene. By default, only the latest
# version per scene is queried.
#
# @param project_id Integer. Project ID.
# @param session_filters List. Built by build_session_filters().
# @param max_sessions Integer. Per-scene-version session cap (same as `n`).
# @param scene_id Character or NULL. Filter to a specific scene UUID.
# @param scene_version_id Integer or NULL. Filter to a specific version.
# @return List with $sessions (list of raw session objects) and
#   $scenes_meta (list from fetch_scenes_metadata()).
# @keywords internal
fetch_scene_sessions <- function(project_id, session_filters, max_sessions,
                                 scene_id = NULL, scene_version_id = NULL) {
  cli::cli_progress_step("Fetching scenes metadata")
  scenes_meta <- fetch_scenes_metadata(project_id)

  if (nrow(scenes_meta$versions) == 0) {
    cli::cli_alert_warning("No scenes found for project {.val {project_id}}.")
    return(list(sessions = list(), scenes_meta = scenes_meta))
  }

  # Determine target scene versions
  target_versions <- scenes_meta$versions

  if (!is.null(scene_id)) {
    target_versions <- target_versions[target_versions$scene_id == scene_id, ]
  }
  if (!is.null(scene_version_id)) {
    target_versions <- target_versions[target_versions$version_id == scene_version_id, ]
  }

  # Default: latest version per scene (highest version_number)
  if (is.null(scene_version_id)) {
    target_versions <- do.call(rbind, lapply(
      split(target_versions, target_versions$scene_id),
      function(sv) sv[which.max(sv$version_number), ]
    ))
    target_versions <- tibble::as_tibble(target_versions)
  }

  if (nrow(target_versions) == 0) {
    cli::cli_alert_warning("No matching scene versions found.")
    return(list(sessions = list(), scenes_meta = scenes_meta))
  }

  # Iterate over scene versions — each gets the full n budget so all
  # scenes are represented. The output may have more rows than n because
  # the same session can appear under multiple scenes.
  all_results <- list()

  cli::cli_progress_bar(
    "Fetching scene sessions",
    total = nrow(target_versions),
    clear = TRUE
  )

  for (i in seq_len(nrow(target_versions))) {
    sv <- target_versions[i, ]

    version_results <- paginate_sessions(
      project_id = project_id,
      session_filters = session_filters,
      max_sessions = max_sessions,
      page_limit = 100L,
      include_events = FALSE,
      session_type = "scene",
      scene_id = sv$scene_id,
      version_id = sv$version_id
    )

    all_results <- c(all_results, version_results)
    cli::cli_progress_update()
  }

  cli::cli_progress_done()

  list(sessions = all_results, scenes_meta = scenes_meta)
}

# Convert a date input to epoch milliseconds (whole number)
to_epoch_ms <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(round(as.numeric(x) * 1000))
  }
  if (inherits(x, "Date")) {
    return(round(as.numeric(as.POSIXct(x, tz = "UTC")) * 1000))
  }
  if (is.character(x)) {
    parsed <- tryCatch(
      as.POSIXct(x, tz = "UTC"),
      error = function(e) NULL
    )
    if (is.null(parsed)) {
      cli::cli_abort("Cannot parse date string {.val {x}}. Use YYYY-MM-DD format.")
    }
    return(round(as.numeric(parsed) * 1000))
  }
  cli::cli_abort("Date argument must be a Date, POSIXct, or YYYY-MM-DD string.")
}


# --- Session response parsing ---

# Column list for compact mode (~40 columns)
compact_columns <- c(
  # Core session fields (top-level API fields)
  "session_id", "session_date", "end_date", "duration_s", "hmd", "device_id",
  "participant_id", "user_key", "friendly_name", "tags",
  # Scene fields (populated for session_type = "scene")
  "scene_id", "scene_version_id", "scene_name",
  # C3D properties: participant info
  "c3d_participant_name", "c3d_participant_oculus_id",
  # C3D properties: app & device info
  "c3d_app_name", "c3d_app_version", "c3d_version", "c3d_device_type",
  "c3d_device_os", "c3d_device_model",
  # C3D properties: location & environment
  "c3d_geo_country", "c3d_geo_subdivision", "c3d_geo_city",
  "c3d_roomsize_meters",
  # C3D properties: key metrics (top-level scores)
  "c3d_metrics_fps_score", "c3d_metrics_app_performance",
  "c3d_metrics_average_fps", "c3d_metrics_presence_score",
  "c3d_metrics_immersion_score", "c3d_metrics_orientation_score",
  "c3d_metrics_comfort_score", "c3d_metrics_ergonomics_score",
  "c3d_metrics_battery_efficiency", "c3d_metrics_boundary_score",
  "c3d_metrics_controller_events_score",
  "c3d_metrics_controller_engagement_score",
  "c3d_metrics_dynamic_engagement_score", "c3d_metrics_standing_percentage",
  # C3D properties: metric components (sub-scores)
  "c3d_metric_components_fps_score_degree_app_performance",
  "c3d_metric_components_fps_score_consistency_app_performance",
  "c3d_metric_components_fps_score_fluctuation_app_performance",
  "c3d_metric_components_fps_score_session_percentage",
  "c3d_metric_components_comfort_score_head_orientation_score",
  "c3d_metric_components_comfort_score_head_orientation_score_pitch_score",
  "c3d_metric_components_comfort_score_head_orientation_score_roll_score",
  "c3d_metric_components_comfort_score_controller_ergonomic_score",
  "c3d_metric_components_comfort_score_controller_ergonomic_score_forward_reach_score",
  "c3d_metric_components_comfort_score_controller_ergonomic_score_horizontal_reach_score",
  "c3d_metric_components_comfort_score_controller_ergonomic_score_vertical_reach_score",
  "c3d_metric_components_presence_score_gaze_exploration_score",
  "c3d_metric_components_presence_score_interruption_score",
  "c3d_metric_components_presence_score_controller_movement_score",
  "c3d_metric_components_cyberwellness_visual_continuity",
  # C3D properties: session flags
  "c3d_session_tag_junk", "c3d_session_tag_test"
)

# Convert a camelCase top-level field name to snake_case
to_snake_case <- function(x) {
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  tolower(x)
}

# Convert a c3d property key to a clean column name
# Replace "." with "_", apply snake_case
clean_property_name <- function(x) {
  x <- gsub("\\.", "_", x)
  to_snake_case(x)
}

# Parse a single session object from the API response into a named list
parse_one_session <- function(session) {
  # Top-level scalar fields
  top_level_fields <- c(
    "sessionId", "projectId", "organizationId", "participantId",
    "date", "endDate", "duration", "gazeInterval", "hmd", "user",
    "deviceId", "userKey", "friendlyName", "sdkSessionId",
    "createdAt", "startTime",
    "sceneId", "sceneVersionId",
    "hasGaze", "hasFixation", "hasEvent", "hasDynamic",
    "hasSensor", "hasBoundary", "positionLimited", "gazeLimited",
    "fixationsLimited", "eventsLimited"
  )

  row <- list()

  # Extract top-level fields with snake_case naming

  for (field in top_level_fields) {
    val <- session[[field]]
    col_name <- to_snake_case(field)
    row[[col_name]] <- if (is.null(val)) NA else val
  }

  # Handle tags (list -> comma-separated string or NA)
  tags <- session[["tags"]]
  row[["tags"]] <- if (is.null(tags) || length(tags) == 0) {
    NA_character_
  } else {
    paste(tags, collapse = ", ")
  }

  # Flatten properties
  props <- session[["properties"]]
  if (!is.null(props) && length(props) > 0) {
    for (key in names(props)) {
      col_name <- clean_property_name(key)
      val <- props[[key]]
      row[[col_name]] <- if (is.null(val)) NA else val
    }
  }

  row
}

# Parse an API response (list of session objects) into a tibble
#
# @param results List of session objects from the API response.
# @param compact Logical. If TRUE, select only compact columns.
# @return A tibble.
parse_sessions <- function(results, compact = TRUE) {
  if (length(results) == 0) {
    return(tibble::tibble())
  }

  # Parse each session into a flat named list
  rows <- lapply(results, parse_one_session)

  # Collect all column names across all rows (ragged properties)
  all_names <- unique(unlist(lapply(rows, names)))

  # Build a data.frame column-by-column, filling NAs for missing keys
  cols <- lapply(all_names, function(nm) {
    vals <- lapply(rows, function(row) {
      v <- row[[nm]]
      if (is.null(v)) NA else v
    })
    unlist(vals)
  })
  names(cols) <- all_names
  df <- tibble::as_tibble(cols)

  # Type coercion
  df <- coerce_session_types(df)

  # Rename duration -> duration_s (already converted to seconds in coerce step)
  if ("duration" %in% names(df)) {
    df[["duration_s"]] <- df[["duration"]]
    df[["duration"]] <- NULL
  }

  # Rename date -> session_date
  if ("date" %in% names(df)) {
    df[["session_date"]] <- df[["date"]]
    df[["date"]] <- NULL
  }

  # Rename user -> device_id
  if ("user" %in% names(df)) {
    df[["device_id"]] <- df[["user"]]
    df[["user"]] <- NULL
  }

  if (compact) {
    # Select only compact columns that exist in the data
    keep <- intersect(compact_columns, names(df))
    df <- df[, keep, drop = FALSE]
  }

  df
}

# Apply type coercions to session tibble
coerce_session_types <- function(df) {
  # date and end_date -> POSIXct (UTC)
  if ("date" %in% names(df)) {
    df[["date"]] <- as.POSIXct(df[["date"]], format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  }
  if ("end_date" %in% names(df)) {
    df[["end_date"]] <- as.POSIXct(df[["end_date"]], format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  }

  # duration: ms -> seconds

  if ("duration" %in% names(df)) {
    df[["duration"]] <- as.numeric(df[["duration"]]) / 1000
  }

  # Ensure numeric columns that might come as character
  numeric_patterns <- c("^c3d_metrics_", "^c3d_metric_components_",
                        "^c3d_session_sensor_", "^c3d_debug_",
                        "^c3d_geo_latitude$", "^c3d_geo_longitude$",
                        "^c3d_roomsize_meters$",
                        "^c3d_participant_height$", "^c3d_participant_armlength$",
                        "^c3d_device_memory$", "^gaze_interval$")
  for (col in names(df)) {
    for (pat in numeric_patterns) {
      if (grepl(pat, col)) {
        df[[col]] <- as.numeric(df[[col]])
        break
      }
    }
  }

  df
}

# Internal: join scene_name from lookup onto a parsed sessions/events tibble
#
# @param df Tibble with a scene_version_id column.
# @param scenes_lookup Named character vector (version_id as character -> scene_name).
# @return The tibble with a scene_name column added.
# @keywords internal
join_scene_names <- function(df, scenes_lookup) {
  if (!"scene_version_id" %in% names(df) || length(scenes_lookup) == 0) {
    df$scene_name <- NA_character_
    return(df)
  }

  vid_chr <- as.character(df$scene_version_id)
  df$scene_name <- ifelse(
    vid_chr %in% names(scenes_lookup),
    unname(scenes_lookup[vid_chr]),
    NA_character_
  )

  df
}
