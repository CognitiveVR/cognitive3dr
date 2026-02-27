#' Retrieve Cognitive3D event data
#'
#' Queries the Cognitive3D sessions API with event data included, then
#' unnests and flattens events into a one-row-per-event tibble with
#' session context attached.
#'
#' @param project_id Integer. Project ID. Uses the default from
#'   `c3d_project()` if `NULL`.
#' @param exclude_test Logical. If `TRUE` (default), filter out sessions tagged
#'   as test.
#' @param exclude_idle Logical. If `TRUE` (default), filter out sessions tagged
#'   as junk/idle.
#' @param start_date Date, POSIXct, or `"YYYY-MM-DD"` string. Start of date range.
#'   If neither `start_date` nor `end_date` is provided, defaults to 30 days ago.
#' @param end_date Date, POSIXct, or `"YYYY-MM-DD"` string. End of date range.
#'   Defaults to now if `start_date` is set but `end_date` is not.
#' @param min_duration Numeric. Minimum session duration in seconds.
#' @param page_limit Integer. Number of sessions per API page (default 100).
#' @param max_sessions Integer. Hard cap on total sessions to retrieve
#'   events for (default 500).
#'
#' @return A tibble where each row is one event. Session context columns
#'   (`session_id`, `participant_id`, `user_key`, `device_id`,
#'   `session_date`, `duration_s`) appear first, followed by core event
#'   columns and dynamic `prop_*` columns.
#' @export
#'
#' @examples
#' \dontrun{
#' c3d_auth()
#' c3d_project(4460)
#'
#' # Get events from last 30 days
#' events <- c3d_events()
#'
#' # Filter by date range
#' events <- c3d_events(start_date = "2026-01-01", end_date = "2026-02-01")
#'
#' # Minimum 60 second sessions, up to 100 sessions
#' events <- c3d_events(min_duration = 60, max_sessions = 100)
#' }
c3d_events <- function(
  project_id = NULL,
  exclude_test = TRUE,
  exclude_idle = TRUE,
  start_date = NULL,
  end_date = NULL,
  min_duration = NULL,
  page_limit = 100,
  max_sessions = 500
) {
  # --- Input validation ---
  if (is.null(project_id)) {
    project_id <- get_project_id()
  }
  project_id <- as.integer(project_id)
  if (is.na(project_id)) {
    cli::cli_abort("{.arg project_id} must be a valid integer.")
  }

  max_sessions <- as.integer(max_sessions)
  if (is.na(max_sessions) || max_sessions < 1) {
    cli::cli_abort("{.arg max_sessions} must be a positive integer.")
  }
  if (max_sessions > 500L) {
    cli::cli_abort("{.arg max_sessions} must be <= 500. Got {.val {max_sessions}}.")
  }

  page_limit <- as.integer(page_limit)
  if (is.na(page_limit) || page_limit < 1) {
    cli::cli_abort("{.arg page_limit} must be a positive integer.")
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

  # Duration filter
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

  # --- Pagination with includeEvents ---
  all_results <- paginate_sessions(
    project_id = project_id,
    session_filters = session_filters,
    max_sessions = max_sessions,
    page_limit = page_limit,
    include_events = TRUE
  )

  if (length(all_results) == 0) {
    cli::cli_alert_info("No sessions found.")
    return(empty_events_tibble())
  }

  # --- Fetch dynamic objects lookup ---
  objects_lookup <- fetch_objects_lookup(project_id)

  # --- Fetch scenes lookup for scene_name resolution ---
  scenes_meta <- fetch_scenes_metadata(project_id)

  # --- Parse events ---
  events_df <- parse_all_events(all_results, objects_lookup)

  # --- Join scene_name ---
  join_scene_names(events_df, scenes_meta$lookup)
}


# --- Internal helpers ---

# Parse all events from a list of session objects into a single tibble
# @param sessions List of raw session objects from the API.
# @return A tibble with one row per event.
# @keywords internal
parse_all_events <- function(sessions, objects_lookup = character(0)) {
  all_event_rows <- list()
  any_limited <- FALSE

  for (session in sessions) {
    # Check eventsLimited flag
    if (isTRUE(session[["eventsLimited"]])) {
      any_limited <- TRUE
    }

    # Extract session context
    ctx <- list(
      session_id = as.character(session[["sessionId"]] %||% NA_character_),
      participant_id = as.character(session[["participantId"]] %||% NA_character_),
      user_key = as.character(session[["userKey"]] %||% NA_character_),
      device_id = as.character(session[["deviceId"]] %||% NA_character_),
      session_date = parse_iso_datetime(session[["date"]]),
      duration_s = as.double(session[["duration"]] %||% NA_real_) / 1000
    )

    events <- session[["events"]]
    if (is.null(events) || length(events) == 0) {
      next
    }

    for (event in events) {
      row <- ctx  # start with session context

      # Core event fields
      row$event_name <- as.character(event[["name"]] %||% NA_character_)
      row$event_date <- parse_iso_datetime(event[["date"]])
      row$position_x <- as.double(event[["x"]] %||% NA_real_)
      row$position_y <- as.double(event[["y"]] %||% NA_real_)
      row$position_z <- as.double(event[["z"]] %||% NA_real_)
      raw_obj <- as.character(event[["object"]] %||% NA_character_)
      row$object_id <- raw_obj
      row$object <- if (!is.na(raw_obj) && raw_obj %in% names(objects_lookup)) {
        unname(objects_lookup[[raw_obj]])
      } else {
        NA_character_
      }
      row$scene_version_id <- as.integer(event[["parentSceneVersionId"]] %||% NA_integer_)

      # Flatten properties with prop_ prefix
      props <- event[["properties"]]
      if (!is.null(props) && length(props) > 0) {
        for (key in names(props)) {
          col_name <- paste0("prop_", gsub(" ", "_", key))
          val <- props[[key]]
          row[[col_name]] <- if (is.null(val)) NA else val
        }
      }

      all_event_rows <- c(all_event_rows, list(row))
    }
  }

  if (any_limited) {
    cli::cli_warn(
      "One or more sessions had events truncated at the 8000 event limit ({.field eventsLimited = TRUE}). Event data for those sessions is incomplete."
    )
  }

  if (length(all_event_rows) == 0) {
    cli::cli_alert_info("Sessions found but none contained events.")
    return(empty_events_tibble())
  }

  # Build tibble from ragged list of rows (same approach as parse_sessions)
  all_names <- unique(unlist(lapply(all_event_rows, names)))

  # Ensure fixed columns come first in the expected order
  fixed_cols <- c(
    "session_id", "participant_id", "user_key", "device_id",
    "session_date", "duration_s",
    "event_name", "event_date", "position_x", "position_y", "position_z",
    "object_id", "object", "scene_version_id"
  )
  prop_cols <- sort(setdiff(all_names, fixed_cols))
  col_order <- c(fixed_cols, prop_cols)

  cols <- lapply(col_order, function(nm) {
    vals <- lapply(all_event_rows, function(row) {
      v <- row[[nm]]
      if (is.null(v)) NA else v
    })
    unlist(vals)
  })
  names(cols) <- col_order

  df <- tibble::as_tibble(cols)

  # Parse date columns to POSIXct
  if ("session_date" %in% names(df) && is.character(df[["session_date"]])) {
    df[["session_date"]] <- as.POSIXct(df[["session_date"]],
                                         format = "%Y-%m-%dT%H:%M:%OS",
                                         tz = "UTC")
  }
  if ("event_date" %in% names(df) && is.character(df[["event_date"]])) {
    df[["event_date"]] <- as.POSIXct(df[["event_date"]],
                                       format = "%Y-%m-%dT%H:%M:%OS",
                                       tz = "UTC")
  }

  df
}


# Parse an ISO 8601 datetime string to POSIXct, or return NA
# Returns the string as-is for now; bulk conversion happens after tibble build
# @keywords internal
parse_iso_datetime <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(NA_character_)
  }
  as.character(x)
}


# Return an empty tibble with the correct events column structure
# @keywords internal
empty_events_tibble <- function() {
  tibble::tibble(
    session_id = character(0),
    participant_id = character(0),
    user_key = character(0),
    device_id = character(0),
    session_date = as.POSIXct(character(0), tz = "UTC"),
    duration_s = double(0),
    event_name = character(0),
    event_date = as.POSIXct(character(0), tz = "UTC"),
    position_x = double(0),
    position_y = double(0),
    position_z = double(0),
    object_id = character(0),
    object = character(0),
    scene_version_id = integer(0),
    scene_name = character(0)
  )
}
