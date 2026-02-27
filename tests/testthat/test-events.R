# Load fixtures
fixture_path <- testthat::test_path("fixtures", "events_response.json")
fixture <- jsonlite::fromJSON(fixture_path, simplifyVector = FALSE)

objects_path <- testthat::test_path("fixtures", "objects_response.json")
objects_raw <- jsonlite::fromJSON(objects_path, simplifyVector = FALSE)

# Build objects lookup from fixture (mirrors fetch_objects_lookup logic)
objects_lookup <- stats::setNames(
  vapply(objects_raw, function(obj) as.character(obj$name), character(1)),
  vapply(objects_raw, function(obj) as.character(obj$sdkId), character(1))
)

# Helper: parse the full fixture suppressing the expected eventsLimited warning
parse_fixture <- function(lookup = objects_lookup) {
  suppressWarnings(parse_all_events(fixture$results, lookup))
}

# --- parse_all_events() tests ---

test_that("parse_all_events() returns correct number of rows", {
  result <- parse_fixture()
  # Session 1: 3 events, Session 2: 2 events, Session 3: 0 events = 5 total
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 5)
})

test_that("session context fields are attached to each event row", {
  result <- parse_fixture()

  # First 3 rows are from session 1
  expect_equal(result$session_id[1], "1770998222_b7b62b867ee75a7b9145bf9dc9a7062b")
  expect_equal(result$session_id[2], "1770998222_b7b62b867ee75a7b9145bf9dc9a7062b")
  expect_equal(result$session_id[3], "1770998222_b7b62b867ee75a7b9145bf9dc9a7062b")
  expect_equal(result$participant_id[1], "24162283756688623")
  expect_equal(result$user_key[1], "24162283756688623")
  expect_equal(result$device_id[1], "b7b62b867ee75a7b9145bf9dc9a7062b")

  # Rows 4-5 are from session 2
  expect_equal(result$session_id[4], "1770916397_9b19b3c0eabb4a76d486b0d1eb96a87d")
  expect_equal(result$participant_id[4], "29753982660912975")
  expect_equal(result$device_id[4], "9b19b3c0eabb4a76d486b0d1eb96a87d")
})

test_that("session_date is POSIXct and parsed correctly", {
  result <- parse_fixture()
  expect_s3_class(result$session_date, "POSIXct")
  expect_equal(attr(result$session_date, "tzone"), "UTC")
})

test_that("duration_s is converted from ms to seconds", {
  result <- parse_fixture()
  # Session 1: 249492 ms -> 249.492 s
  expect_equal(result$duration_s[1], 249.492)
  # Session 2: 304504 ms -> 304.504 s
  expect_equal(result$duration_s[4], 304.504)
})

test_that("core event fields are parsed correctly", {
  result <- parse_fixture()

  # Event 1: c3d.sessionStart
  expect_equal(result$event_name[1], "c3d.sessionStart")
  expect_s3_class(result$event_date, "POSIXct")
  expect_equal(result$position_x[1], 0.5)
  expect_equal(result$position_y[1], 1.2)
  expect_equal(result$position_z[1], -3.0)
  expect_true(is.na(result$object_id[1]))
  expect_true(is.na(result$object[1]))
  expect_equal(result$scene_version_id[1], 1001L)

  # Event 3: has an object
  expect_equal(result$object_id[3], "dyn_obj_42")
  expect_equal(result$object[3], "Power Console")
})

test_that("event_date is POSIXct in UTC", {
  result <- parse_fixture()
  expect_s3_class(result$event_date, "POSIXct")
  expect_equal(attr(result$event_date, "tzone"), "UTC")
})

test_that("prop_ prefix is applied to property columns", {
  result <- parse_fixture()
  prop_cols <- grep("^prop_", names(result), value = TRUE)
  expect_gt(length(prop_cols), 0)

  # Check specific prop columns exist
  expect_true("prop_hook" %in% names(result))
  expect_true("prop_Power" %in% names(result))
  expect_true("prop_Answer0" %in% names(result))
  expect_true("prop_duration" %in% names(result))
  expect_true("prop_Reason" %in% names(result))
  expect_true("prop_ispaused" %in% names(result))
})

test_that("property values are correct", {
  result <- parse_fixture()

  # Event 1 props
  expect_equal(result$prop_hook[1], "session_start")
  expect_equal(result$prop_participantId[1], "24162283756688623")

  # Event 2 props (exitpoll with numeric answers)
  expect_equal(result$prop_Answer0[2], 3)
  expect_equal(result$prop_Answer1[2], 5)
  expect_equal(result$prop_duration[2], 45.2)

  # Event 3 props
  expect_equal(result$prop_Power[3], 85.5)
  expect_equal(result$prop_Reason[3], "battery_check")
})

test_that("spaces in property names are replaced with underscores", {
  result <- parse_fixture()
  expect_true("prop_Angle_from_HMD" %in% names(result))
  expect_true("prop_Height_from_HMD" %in% names(result))
  expect_true("prop_Now_Tracking" %in% names(result))
  expect_true("prop_Previously_Tracking" %in% names(result))

  # Check values from session 2 event 1
  expect_equal(result$prop_Angle_from_HMD[4], 15.3)
  expect_equal(result$prop_Now_Tracking[4], "hands")
})

test_that("missing properties become NA across events", {
  result <- parse_fixture()

  # prop_hook exists for events 1-2 but not for events 4-5
  expect_false(is.na(result$prop_hook[1]))
  expect_true(is.na(result$prop_hook[4]))

  # prop_Power exists for event 3 but not event 1
  expect_true(is.na(result$prop_Power[1]))
  expect_false(is.na(result$prop_Power[3]))
})

test_that("session with no events contributes zero rows", {
  result <- parse_fixture()
  # Session 3 has empty events array
  session_ids <- unique(result$session_id)
  expect_false("1770900000_empty_session" %in% session_ids)
  expect_equal(nrow(result), 5)
})

test_that("eventsLimited triggers a warning", {
  # Session 2 has eventsLimited: true
  expect_warning(
    parse_all_events(fixture$results),
    "events truncated"
  )
})

test_that("no eventsLimited warning when all FALSE", {
  # Only use session 1 (eventsLimited: false) and session 3 (empty)
  sessions_no_limit <- fixture$results[c(1, 3)]
  expect_no_warning(
    parse_all_events(sessions_no_limit)
  )
})

test_that("column order is correct: session context, event fields, then props", {
  result <- parse_fixture()
  col_names <- names(result)

  session_ctx <- c("session_id", "participant_id", "user_key", "device_id",
                    "session_date", "duration_s")
  event_core <- c("event_name", "event_date", "position_x", "position_y",
                   "position_z", "object_id", "object", "scene_version_id")

  # Session context columns should be first
  expect_equal(col_names[1:6], session_ctx)
  # Event core columns should follow
  expect_equal(col_names[7:14], event_core)
  # Everything after should be prop_*
  remaining <- col_names[15:length(col_names)]
  expect_true(all(grepl("^prop_", remaining)))
})

test_that("parse_all_events() handles events with empty properties", {
  session_with_empty_props <- list(list(
    sessionId = "test_session",
    participantId = "test_participant",
    userKey = "test_key",
    deviceId = "test_device",
    date = "2026-02-13T15:57:02.425Z",
    duration = 1000,
    eventsLimited = FALSE,
    events = list(
      list(
        name = "test_event",
        date = "2026-02-13T15:57:02.425Z",
        x = 0, y = 0, z = 0,
        object = NULL,
        parentSceneVersionId = 1,
        properties = list()
      )
    )
  ))
  result <- parse_all_events(session_with_empty_props)
  expect_equal(nrow(result), 1)
  expect_equal(result$event_name[1], "test_event")
  # No prop_ columns should exist
  prop_cols <- grep("^prop_", names(result), value = TRUE)
  expect_equal(length(prop_cols), 0)
})

test_that("empty_events_tibble() returns correct structure", {
  result <- empty_events_tibble()
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_true(all(c("session_id", "participant_id", "user_key", "device_id",
                     "session_date", "duration_s", "event_name", "event_date",
                     "position_x", "position_y", "position_z", "object_id",
                     "object", "scene_version_id") %in% names(result)))
  expect_s3_class(result$session_date, "POSIXct")
  expect_s3_class(result$event_date, "POSIXct")
})

test_that("parse_all_events() returns empty tibble when all sessions have no events", {
  sessions_no_events <- list(fixture$results[[3]])  # Only the empty session
  result <- parse_all_events(sessions_no_events)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

# --- Object name resolution tests ---

test_that("object column resolves sdkId to friendly name via lookup", {
  result <- parse_fixture()

  # Event 3 has object = "dyn_obj_42" which maps to "Power Console"
  expect_equal(result$object_id[3], "dyn_obj_42")
  expect_equal(result$object[3], "Power Console")

  # Events without objects have NA for both columns
  expect_true(is.na(result$object_id[1]))
  expect_true(is.na(result$object[1]))
})

test_that("object column is NA when sdkId not in lookup", {
  # Use an empty lookup so no names can be resolved
  result <- parse_fixture(lookup = character(0))

  # object_id should still have the raw value
  expect_equal(result$object_id[3], "dyn_obj_42")
  # object should be NA since lookup is empty
  expect_true(is.na(result$object[3]))
})

test_that("object column is NA for unmatched sdkIds", {
  # Use a lookup that doesn't contain "dyn_obj_42"
  partial_lookup <- c(other_id = "Other Name")
  result <- parse_fixture(lookup = partial_lookup)

  expect_equal(result$object_id[3], "dyn_obj_42")
  expect_true(is.na(result$object[3]))
})


# --- c3d_events() input validation tests ---

test_that("c3d_events() validates max_sessions", {
  c3d_auth("test_key")
  c3d_project(4460)
  expect_error(c3d_events(max_sessions = 501), "must be <= 500")
  expect_error(c3d_events(max_sessions = 0), "positive integer")
})

test_that("c3d_events() validates page_limit", {
  c3d_auth("test_key")
  c3d_project(4460)
  expect_error(c3d_events(page_limit = 0), "positive integer")
})

test_that("c3d_events() errors without project_id", {
  c3d_auth("test_key")
  .c3d_env$project_id <- NULL
  expect_error(c3d_events(), "No default project")
})

# --- scene_name column tests ---

test_that("empty_events_tibble() includes scene_name column", {
  result <- empty_events_tibble()
  expect_true("scene_name" %in% names(result))
  expect_equal(nrow(result), 0)
})

test_that("scene_name is added to events via join_scene_names()", {
  # Simulate parsed events with scene_version_id
  events_df <- parse_fixture()
  lookup <- c("1001" = "Main Level", "2001" = "Tutorial")
  result <- join_scene_names(events_df, lookup)
  expect_true("scene_name" %in% names(result))
  # Event 1 has scene_version_id = 1001
  expect_equal(result$scene_name[1], "Main Level")
})
