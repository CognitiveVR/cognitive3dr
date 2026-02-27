# Internal: build session filter list for API requests
#
# Shared helper used by c3d_sessions() and c3d_objective_results().
# Constructs the sessionFilters array from common filter parameters.
#
# @param exclude_test Logical. Filter out test-tagged sessions.
# @param exclude_idle Logical. Filter out junk/idle-tagged sessions.
# @param start_date Date/POSIXct/string or NULL. Start of date range.
# @param end_date Date/POSIXct/string or NULL. End of date range.
# @return A list of filter objects for the API request body.
# @keywords internal
build_session_filters <- function(exclude_test = TRUE, exclude_idle = TRUE,
                                  start_date = NULL, end_date = NULL) {
  filters <- list()

  # Date range filters
  if (!is.null(start_date)) {
    start_ms <- to_epoch_ms(start_date)
    filters <- c(filters, list(list(
      field = list(fieldName = "date", fieldParent = "session"),
      op = "gte",
      value = start_ms
    )))

    if (is.null(end_date)) {
      end_date <- Sys.time()
    }
  }
  if (!is.null(end_date)) {
    end_ms <- to_epoch_ms(end_date)
    filters <- c(filters, list(list(
      field = list(fieldName = "date", fieldParent = "session"),
      op = "lte",
      value = end_ms
    )))
  }

  # Test tag filter
  if (isTRUE(exclude_test)) {
    filters <- c(filters, list(list(
      op = "eq",
      field = list(
        nestedFieldName = "booleanSessionProp",
        path = "c3d.session_tag.test"
      ),
      value = FALSE
    )))
  }

  # Junk/idle tag filter
  if (isTRUE(exclude_idle)) {
    filters <- c(filters, list(list(
      op = "eq",
      field = list(
        nestedFieldName = "booleanSessionProp",
        path = "c3d.session_tag.junk"
      ),
      value = FALSE
    )))
  }

  filters
}
