# Load the fixture
fixture_path <- testthat::test_path("fixtures", "list_sessions_response.json")
fixture <- jsonlite::fromJSON(fixture_path, simplifyVector = FALSE)

# --- parse_sessions() tests ---

test_that("parse_sessions() returns a tibble with correct dimensions", {
  result <- parse_sessions(fixture$results, compact = TRUE)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)
})

test_that("parse_sessions() compact mode selects expected columns", {
  result <- parse_sessions(fixture$results, compact = TRUE)
  # All columns should be from the compact list
  expect_true(all(names(result) %in% compact_columns))
  # Should have a reasonable number of columns
  expect_gt(ncol(result), 20)
})

test_that("parse_sessions() full mode has more columns than compact", {
  compact_result <- parse_sessions(fixture$results, compact = TRUE)
  full_result <- parse_sessions(fixture$results, compact = FALSE)
  expect_gt(ncol(full_result), ncol(compact_result))
})

test_that("session_id is parsed correctly", {
  result <- parse_sessions(fixture$results, compact = TRUE)
  expect_equal(
    result$session_id[1],
    "1770998222_b7b62b867ee75a7b9145bf9dc9a7062b"
  )
  expect_equal(
    result$session_id[4],
    "1770916397_9b19b3c0eabb4a76d486b0d1eb96a87d"
  )
})

test_that("date columns are POSIXct in UTC", {
  result <- parse_sessions(fixture$results, compact = TRUE)
  expect_s3_class(result$session_date, "POSIXct")
  expect_equal(attr(result$session_date, "tzone"), "UTC")
  expect_s3_class(result$end_date, "POSIXct")
})

test_that("device_id and user_key are in compact mode", {
  result <- parse_sessions(fixture$results, compact = TRUE)
  expect_true("device_id" %in% names(result))
  expect_true("user_key" %in% names(result))
  expect_equal(result$device_id[1], "b7b62b867ee75a7b9145bf9dc9a7062b")
  expect_equal(result$user_key[1], "24162283756688623")
})

test_that("duration is converted from ms to seconds", {
  result <- parse_sessions(fixture$results, compact = TRUE)
  # Session 1: 249492 ms -> 249.492 s
  expect_equal(result$duration_s[1], 249.492)
  # Session 2: 111335 ms -> 111.335 s
  expect_equal(result$duration_s[2], 111.335)
  # Session 4: 304504 ms -> 304.504 s
  expect_equal(result$duration_s[4], 304.504)
})

test_that("property names are cleaned correctly", {
  expect_equal(clean_property_name("c3d.app.version"), "c3d_app_version")
  expect_equal(clean_property_name("c3d.metrics.fps_score"), "c3d_metrics_fps_score")
  expect_equal(
    clean_property_name("c3d.metric_components.comfort_score.head_orientation_score"),
    "c3d_metric_components_comfort_score_head_orientation_score"
  )
  expect_equal(clean_property_name("c3d.device.os"), "c3d_device_os")
})

test_that("to_snake_case converts camelCase correctly", {
  expect_equal(to_snake_case("sessionId"), "session_id")
  expect_equal(to_snake_case("endDate"), "end_date")
  expect_equal(to_snake_case("friendlyName"), "friendly_name")
  expect_equal(to_snake_case("hasGaze"), "has_gaze")
})

test_that("hmd values are preserved", {
  result <- parse_sessions(fixture$results, compact = TRUE)
  expect_equal(result$hmd[1], "Head Tracking - OpenXR")
  expect_equal(result$hmd[4], "Head Tracking - OpenXR")
})

test_that("app and device fields are parsed correctly", {
  result <- parse_sessions(fixture$results, compact = TRUE)
  expect_equal(result$c3d_app_name[1], "c3d-unity-demo")
  expect_equal(result$c3d_app_version[1], "3.5")
  expect_equal(result$c3d_app_version[3], "3.2")
  expect_equal(result$c3d_device_model[1], "Oculus Quest")
  expect_equal(result$c3d_device_os[1], "Android OS 14")
})

test_that("metrics are numeric", {
  result <- parse_sessions(fixture$results, compact = TRUE)
  expect_type(result$c3d_metrics_fps_score, "double")
  expect_type(result$c3d_metrics_presence_score, "double")
  # Session 1 fps_score ~ 84.20
  expect_equal(result$c3d_metrics_fps_score[1], 84.19852035607636)
  # Session 3 fps_score ~ 94.66
  expect_equal(result$c3d_metrics_fps_score[3], 94.66446375494088)
})

test_that("ragged properties produce NA for missing columns", {
  result <- parse_sessions(fixture$results, compact = FALSE)
  # Session 1 has xrpf.allowed.audio.data but sessions 3 & 4 do not
  if ("xrpf_allowed_audio_data" %in% names(result)) {
    expect_false(is.na(result$xrpf_allowed_audio_data[1]))
    expect_true(is.na(result$xrpf_allowed_audio_data[3]))
  }
})

test_that("session tags are handled correctly", {
  result <- parse_sessions(fixture$results, compact = TRUE)
  # All sessions have empty tags arrays
  expect_true(all(is.na(result$tags)))
})

test_that("friendly_name is preserved", {
  result <- parse_sessions(fixture$results, compact = FALSE)
  expect_equal(result$friendly_name[1], "Parisa.srg")
  expect_equal(result$friendly_name[4], "matt-manuel")
})

test_that("boolean session flags are preserved in full mode", {
  result <- parse_sessions(fixture$results, compact = FALSE)
  expect_true(result$has_gaze[1])
  expect_false(result$has_fixation[1])
  expect_true(result$has_boundary[1])
})

test_that("participant_id is parsed", {
  result <- parse_sessions(fixture$results, compact = TRUE)
  expect_equal(result$participant_id[1], "24162283756688623")
  expect_equal(result$participant_id[4], "29753982660912975")
})

test_that("comfort and engagement scores are parsed", {
  result <- parse_sessions(fixture$results, compact = TRUE)
  expect_equal(result$c3d_metrics_comfort_score[1], 74.34505247920842)
  expect_equal(result$c3d_metrics_controller_engagement_score[1], 54.27067375518957)
})

test_that("parse_sessions() handles empty input", {
  result <- parse_sessions(list(), compact = TRUE)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

# --- Input validation tests ---

test_that("c3d_sessions() validates n > 500", {
  c3d_auth("test_key")
  c3d_project(4460)
  expect_error(c3d_sessions(n = 501), "must be <= 500")
})

test_that("c3d_sessions() validates n is positive", {
  c3d_auth("test_key")
  c3d_project(4460)
  expect_error(c3d_sessions(n = 0), "positive integer")
})

test_that("c3d_sessions() errors without project_id", {
  c3d_auth("test_key")
  .c3d_env$project_id <- NULL
  expect_error(c3d_sessions(), "No default project")
})

# --- to_epoch_ms() tests ---

test_that("to_epoch_ms() handles Date objects", {
  d <- as.Date("2026-01-01")
  ms <- to_epoch_ms(d)
  expect_equal(ms, as.numeric(as.POSIXct("2026-01-01", tz = "UTC")) * 1000)
})

test_that("to_epoch_ms() handles POSIXct objects", {
  dt <- as.POSIXct("2026-01-01 12:00:00", tz = "UTC")
  ms <- to_epoch_ms(dt)
  expect_equal(ms, as.numeric(dt) * 1000)
})

test_that("to_epoch_ms() handles YYYY-MM-DD strings", {
  ms <- to_epoch_ms("2026-01-01")
  expected <- as.numeric(as.POSIXct("2026-01-01", tz = "UTC")) * 1000
  expect_equal(ms, expected)
})

test_that("to_epoch_ms() errors on invalid strings", {
  expect_error(to_epoch_ms("not-a-date"), "Cannot parse")
})

test_that("to_epoch_ms() errors on non-date types", {
  expect_error(to_epoch_ms(12345), "must be a Date")
})

# --- Scene sessions tests ---

# Load scene fixtures
scene_meta_path <- testthat::test_path("fixtures", "scenes_metadata.json")
scene_meta_raw <- jsonlite::fromJSON(scene_meta_path, simplifyVector = FALSE)

scene_sessions_path <- testthat::test_path("fixtures", "scene_sessions_response.json")
scene_sessions_fixture <- jsonlite::fromJSON(scene_sessions_path, simplifyVector = FALSE)

# --- fetch_scenes_metadata() tests ---

test_that("fetch_scenes_metadata() parses versions correctly", {
  # Simulate what fetch_scenes_metadata does with the raw response
  scenes <- scene_meta_raw$scenes

  ver_rows <- list()
  for (scene in scenes) {
    sid <- as.character(scene$id)
    sname <- as.character(scene$sceneName)
    for (ver in scene$versions) {
      ver_rows <- c(ver_rows, list(list(
        scene_id = sid,
        scene_name = sname,
        version_id = as.integer(ver$id),
        version_number = as.integer(ver$versionNumber)
      )))
    }
  }
  versions <- tibble::as_tibble(do.call(rbind, lapply(ver_rows, as.data.frame,
                                                       stringsAsFactors = FALSE)))
  versions$version_id <- as.integer(versions$version_id)
  versions$version_number <- as.integer(versions$version_number)

  # 2 scenes: scene 1 has 3 versions, scene 2 has 2 versions = 5 total
  expect_equal(nrow(versions), 5)
  expect_equal(versions$scene_name[1], "Level 0 - User Onboarding")
  expect_equal(versions$scene_id[1], "de704574-b03f-424e-be87-4985f85ed2e8")
  expect_equal(versions$version_id[1], 7011L)
  expect_equal(versions$version_number[1], 4L)
})

test_that("fetch_scenes_metadata() builds correct lookup", {
  scenes <- scene_meta_raw$scenes

  ver_rows <- list()
  for (scene in scenes) {
    for (ver in scene$versions) {
      ver_rows <- c(ver_rows, list(list(
        scene_name = as.character(scene$sceneName),
        version_id = as.integer(ver$id)
      )))
    }
  }

  lookup <- stats::setNames(
    vapply(ver_rows, function(v) v$scene_name, character(1)),
    vapply(ver_rows, function(v) as.character(v$version_id), character(1))
  )

  expect_equal(lookup[["7011"]], "Level 0 - User Onboarding")
  expect_equal(lookup[["5356"]], "Level 0 - User Onboarding")
  expect_equal(lookup[["7008"]], "Level 1 - Fantasy Environment")
  expect_equal(lookup[["5260"]], "Level 1 - Fantasy Environment")
})

# --- parse_sessions() with scene fields ---

test_that("parse_sessions() extracts scene_id and scene_version_id from scene sessions", {
  result <- parse_sessions(scene_sessions_fixture$results, compact = FALSE)

  expect_true("scene_id" %in% names(result))
  expect_true("scene_version_id" %in% names(result))
  expect_equal(nrow(result), 3)
  expect_equal(result$scene_id[1], "de704574-b03f-424e-be87-4985f85ed2e8")
  expect_equal(result$scene_version_id[1], 7011)
  expect_equal(result$scene_id[2], "f6ffe056-f92e-4431-a863-7eedc6592423")
  expect_equal(result$scene_version_id[2], 7008)
})

test_that("same session_id can appear multiple times in scene sessions", {
  result <- parse_sessions(scene_sessions_fixture$results, compact = FALSE)

  # Sessions 1 and 2 have the same session_id but different scenes
  expect_equal(result$session_id[1], result$session_id[2])
  expect_false(result$scene_id[1] == result$scene_id[2])
})

test_that("scene columns appear in compact mode for scene sessions", {
  result <- parse_sessions(scene_sessions_fixture$results, compact = TRUE)

  expect_true("scene_id" %in% names(result))
  expect_true("scene_version_id" %in% names(result))
})

test_that("project sessions do not have scene fields", {
  result <- parse_sessions(fixture$results, compact = FALSE)

  # scene_id and scene_version_id should be NA for project sessions
  if ("scene_id" %in% names(result)) {
    expect_true(all(is.na(result$scene_id)))
  }
})

# --- join_scene_names() tests ---

test_that("join_scene_names() resolves version_id to scene_name", {
  df <- tibble::tibble(
    session_id = c("a", "b", "c"),
    scene_version_id = c(7011L, 7008L, 5356L)
  )
  lookup <- c("7011" = "Level 0 - User Onboarding",
              "7008" = "Level 1 - Fantasy Environment",
              "5356" = "Level 0 - User Onboarding")

  result <- join_scene_names(df, lookup)
  expect_equal(result$scene_name[1], "Level 0 - User Onboarding")
  expect_equal(result$scene_name[2], "Level 1 - Fantasy Environment")
  expect_equal(result$scene_name[3], "Level 0 - User Onboarding")
})

test_that("join_scene_names() returns NA for empty lookup", {
  df <- tibble::tibble(
    session_id = c("a", "b"),
    scene_version_id = c(7011L, 7008L)
  )
  result <- join_scene_names(df, character(0))
  expect_true(all(is.na(result$scene_name)))
})

test_that("join_scene_names() returns NA for unmatched version_ids", {
  df <- tibble::tibble(
    session_id = c("a", "b"),
    scene_version_id = c(7011L, 9999L)
  )
  lookup <- c("7011" = "Level 0")
  result <- join_scene_names(df, lookup)
  expect_equal(result$scene_name[1], "Level 0")
  expect_true(is.na(result$scene_name[2]))
})

test_that("join_scene_names() handles missing scene_version_id column", {
  df <- tibble::tibble(session_id = c("a", "b"))
  lookup <- c("7011" = "Level 0")
  result <- join_scene_names(df, lookup)
  expect_true(all(is.na(result$scene_name)))
})

# --- c3d_sessions() scene input validation ---

test_that("c3d_sessions() errors when scene_id used with session_type='project'", {
  c3d_auth("test_key")
  c3d_project(4460)
  expect_error(
    c3d_sessions(scene_id = "some-uuid"),
    "can only be used with"
  )
})

test_that("c3d_sessions() errors when scene_version_id used with session_type='project'", {
  c3d_auth("test_key")
  c3d_project(4460)
  expect_error(
    c3d_sessions(scene_version_id = 7011),
    "can only be used with"
  )
})

test_that("c3d_sessions() validates scene_id type", {
  c3d_auth("test_key")
  c3d_project(4460)
  expect_error(
    c3d_sessions(session_type = "scene", scene_id = 123),
    "single character string"
  )
})

test_that("c3d_sessions() validates scene_version_id type", {
  c3d_auth("test_key")
  c3d_project(4460)
  suppressWarnings(expect_error(
    c3d_sessions(session_type = "scene", scene_version_id = "abc"),
    "valid integer"
  ))
})
