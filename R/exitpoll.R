#' Retrieve exit poll response counts
#'
#' Queries the Cognitive3D exit poll API and returns a tidy tibble with
#' one row per response option per question per version. Raw value codes
#' are mapped to human-readable labels using question metadata.
#'
#' This function coordinates three API endpoints:
#' \enumerate{
#'   \item GET available question set hooks for the project
#'   \item GET question set metadata (questions, versions) per hook
#'   \item POST response count queries (aggregate counts per answer)
#' }
#'
#' @param project_id Integer. Project ID. Uses the default from
#'   \code{c3d_project()} if \code{NULL}.
#' @param hook Character or \code{NULL}. Filter to a specific hook name
#'   (e.g., \code{"end_questions"}). If \code{NULL} (default), retrieves
#'   data from all hooks.
#' @param version Integer vector or \code{NULL}. Specific version number(s) to
#'   retrieve. If \code{NULL} (default), retrieves all versions.
#' @param exclude_test Logical. If \code{TRUE} (default), filter out sessions
#'   tagged as test.
#' @param exclude_idle Logical. If \code{TRUE} (default), filter out sessions
#'   tagged as junk/idle.
#' @param start_date Date, POSIXct, or \code{"YYYY-MM-DD"} string. Start of
#'   date range. If neither \code{start_date} nor \code{end_date} is provided,
#'   no date filter is applied (all-time).
#' @param end_date Date, POSIXct, or \code{"YYYY-MM-DD"} string. End of date
#'   range. Defaults to now if \code{start_date} is set but \code{end_date}
#'   is not.
#'
#' @return A tibble with columns: \code{hook}, \code{version},
#'   \code{question_index}, \code{question_title}, \code{question_type},
#'   \code{value}, \code{value_label}, \code{count}. Skipped responses
#'   appear with \code{value = "skipped"} and \code{value_label = "skipped"}.
#' @export
#'
#' @examples
#' \dontrun{
#' c3d_auth()
#' c3d_project(2797)
#'
#' # Get all hooks and versions
#' results <- c3d_exitpoll()
#'
#' # Filter to a specific hook
#' results <- c3d_exitpoll(hook = "end_questions")
#'
#' # Specific hook and version with date filter
#' results <- c3d_exitpoll(hook = "end_questions", version = 3,
#'                          start_date = "2025-01-01")
#' }
c3d_exitpoll <- function(
  project_id = NULL,
  hook = NULL,
  version = NULL,
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

  if (!is.null(hook)) {
    if (!is.character(hook) || length(hook) != 1 || nchar(hook) == 0) {
      cli::cli_abort("{.arg hook} must be a non-empty string or NULL.")
    }
  }

  if (!is.null(version)) {
    version <- as.integer(version)
    if (any(is.na(version))) {
      cli::cli_abort("{.arg version} must be a valid integer or integer vector.")
    }
  }

  # --- Step 1: Determine which hooks to fetch ---
  if (is.null(hook)) {
    hooks <- fetch_exitpoll_hooks(project_id)
    if (length(hooks) == 0) {
      cli::cli_alert_info("No exit poll hooks found for project {.val {project_id}}.")
      return(empty_exitpoll_tibble())
    }
  } else {
    hooks <- hook
  }

  # --- Step 2: Build session filters ---
  session_filters <- build_session_filters(
    exclude_test = exclude_test,
    exclude_idle = exclude_idle,
    start_date = start_date,
    end_date = end_date
  )

  # --- Step 3: Fetch and parse each hook ---
  all_results <- list()

  for (current_hook in hooks) {
    hook_results <- fetch_exitpoll_hook_data(
      project_id, current_hook, version, session_filters
    )
    if (!is.null(hook_results)) {
      all_results <- c(all_results, list(hook_results))
    }
  }

  if (length(all_results) == 0) {
    cli::cli_alert_info("No exit poll response data found.")
    return(empty_exitpoll_tibble())
  }

  # --- Step 4: Combine and return ---
  dplyr::bind_rows(all_results)
}


# --- Internal helpers ---

#' Fetch available exit poll hook names for a project
#' @keywords internal
fetch_exitpoll_hooks <- function(project_id) {
  endpoint <- paste0("/v0/questionSets?projectIds=", project_id)
  raw <- c3d_get(endpoint)

  if (length(raw) == 0) return(character(0))

  hooks <- vapply(raw, function(h) {
    as.character(h$name %||% h$id %||% NA_character_)
  }, character(1))
  unique(hooks[!is.na(hooks)])
}


#' Fetch and parse all data for a single hook
#' @keywords internal
fetch_exitpoll_hook_data <- function(project_id, hook, version,
                                     session_filters) {
  metadata <- fetch_exitpoll_metadata(project_id, hook)

  if (nrow(metadata$versions) == 0) return(NULL)

  # Filter versions if requested

  target_versions <- metadata$versions
  if (!is.null(version)) {
    target_versions <- target_versions[target_versions$version %in% version, ]
    if (nrow(target_versions) == 0) return(NULL)
  }

  all_results <- list()

  for (i in seq_len(nrow(target_versions))) {
    ver <- target_versions$version[i]
    questions_for_ver <- metadata$questions[metadata$questions$version == ver, ]

    raw_counts <- fetch_response_counts(project_id, hook, ver, session_filters)

    if (length(raw_counts) == 0) next

    version_tbl <- parse_exitpoll_responses(raw_counts, ver, hook,
                                            questions_for_ver)
    all_results <- c(all_results, list(version_tbl))
  }

  if (length(all_results) == 0) return(NULL)

  dplyr::bind_rows(all_results)
}


#' Fetch and parse question set metadata for a specific hook
#' @keywords internal
fetch_exitpoll_metadata <- function(project_id, hook) {
  endpoint <- paste0("/v0/projects/", project_id, "/questionSets/", hook)
  raw <- c3d_get(endpoint)

  empty_versions <- tibble::tibble(
    version = integer(0),
    question_set_id = character(0),
    title = character(0)
  )

  empty_questions <- tibble::tibble(
    version = integer(0),
    question_index = integer(0),
    question_title = character(0),
    question_type = character(0),
    answers = list()
  )

  if (length(raw) == 0) {
    return(list(versions = empty_versions, questions = empty_questions))
  }

  ver_rows <- list()
  q_rows <- list()

  for (ver_obj in raw) {
    ver_num <- as.integer(ver_obj$version)
    ver_rows <- c(ver_rows, list(list(
      version = ver_num,
      question_set_id = as.character(ver_obj$id %||% NA_character_),
      title = as.character(ver_obj$title %||% NA_character_)
    )))

    qs <- ver_obj$questions %||% list()
    for (qi in seq_along(qs)) {
      q <- qs[[qi]]
      q_rows <- c(q_rows, list(list(
        version = ver_num,
        question_index = as.integer(qi),
        question_title = trimws(as.character(q$title %||% NA_character_)),
        question_type = tolower(as.character(q$type %||% NA_character_)),
        answers = list(q$answers %||% list())
      )))
    }
  }

  versions <- empty_versions
  if (length(ver_rows) > 0) {
    versions <- tibble::as_tibble(do.call(
      rbind, lapply(ver_rows, as.data.frame, stringsAsFactors = FALSE)
    ))
    versions$version <- as.integer(versions$version)
  }

  questions <- empty_questions
  if (length(q_rows) > 0) {
    questions <- tibble::tibble(
      version = vapply(q_rows, function(r) r$version, integer(1)),
      question_index = vapply(q_rows, function(r) r$question_index, integer(1)),
      question_title = vapply(q_rows, function(r) r$question_title, character(1)),
      question_type = vapply(q_rows, function(r) r$question_type, character(1)),
      answers = lapply(q_rows, function(r) r$answers[[1]])
    )
  }

  list(versions = versions, questions = questions)
}


#' Fetch response counts for a single version
#' @keywords internal
fetch_response_counts <- function(project_id, hook, version, session_filters) {
  endpoint <- paste0(
    "/v0/projects/", project_id,
    "/questionSets/", hook,
    "/", version,
    "/responseCountQueries"
  )

  body <- list()
  if (length(session_filters) > 0) {
    body$sessionFilters <- list(list(op = "and", children = session_filters))
  } else {
    body$sessionFilters <- list()
  }

  c3d_request(endpoint, body)
}


#' Parse response count arrays into a tidy tibble
#' @keywords internal
parse_exitpoll_responses <- function(raw_counts, version_num, hook,
                                     questions_meta) {
  if (length(raw_counts) == 0) {
    return(empty_exitpoll_tibble())
  }

  n_questions <- nrow(questions_meta)
  n_counts <- length(raw_counts)

  if (n_counts != n_questions) {
    cli::cli_warn(
      "Response count array length ({n_counts}) does not match question count ({n_questions}) for version {version_num}. Processing {min(n_counts, n_questions)} questions."
    )
  }

  all_rows <- list()
  n_process <- min(n_counts, n_questions)

  for (qi in seq_len(n_process)) {
    response_options <- raw_counts[[qi]]
    q_title <- questions_meta$question_title[qi]
    q_type <- questions_meta$question_type[qi]
    q_answers <- questions_meta$answers[[qi]]
    q_meta <- list(answers = q_answers)

    for (resp in response_options) {
      raw_value <- resp$value
      is_skipped <- isTRUE(resp$skipped) || is.null(raw_value)

      if (is_skipped) {
        value_chr <- "skipped"
        label <- "skipped"
      } else {
        value_chr <- as.character(raw_value)
        label <- map_value_label(value_chr, q_type, q_meta)
      }

      all_rows <- c(all_rows, list(list(
        hook = hook,
        version = as.integer(version_num),
        question_index = as.integer(qi),
        question_title = q_title,
        question_type = q_type,
        value = value_chr,
        value_label = label,
        count = as.integer(resp$count %||% 0L)
      )))
    }
  }

  if (length(all_rows) == 0) {
    return(empty_exitpoll_tibble())
  }

  tibble::tibble(
    hook = vapply(all_rows, function(r) r$hook, character(1)),
    version = vapply(all_rows, function(r) r$version, integer(1)),
    question_index = vapply(all_rows, function(r) r$question_index, integer(1)),
    question_title = vapply(all_rows, function(r) r$question_title, character(1)),
    question_type = vapply(all_rows, function(r) r$question_type, character(1)),
    value = vapply(all_rows, function(r) r$value, character(1)),
    value_label = vapply(all_rows, function(r) r$value_label, character(1)),
    count = vapply(all_rows, function(r) r$count, integer(1))
  )
}


#' Map a raw response value to a human-readable label
#' @keywords internal
map_value_label <- function(value, question_type, question_meta) {
  if (is.null(value) || is.na(value)) {
    return(NA_character_)
  }

  switch(question_type,
    boolean = switch(value, "0" = "False", "1" = "True", value),
    scale = value,
    happysad = switch(value, "0" = "Sad", "1" = "Happy", value),
    multiple = {
      idx <- as.integer(value) + 1L
      answers <- question_meta$answers
      if (!is.null(answers) && length(answers) >= idx && idx >= 1L) {
        as.character(answers[[idx]]$answer %||% value)
      } else {
        value
      }
    },
    thumbs = switch(value, "0" = "Down", "1" = "Up", value),
    voice = switch(value, "0" = "Responded", value),
    value
  )
}


#' Return an empty tibble with the correct exitpoll column structure
#' @keywords internal
empty_exitpoll_tibble <- function() {
  tibble::tibble(
    hook = character(0),
    version = integer(0),
    question_index = integer(0),
    question_title = character(0),
    question_type = character(0),
    value = character(0),
    value_label = character(0),
    count = integer(0)
  )
}
