# Load fixtures
metadata_path <- testthat::test_path("fixtures", "objectives_metadata.json")
metadata_raw <- jsonlite::fromJSON(metadata_path, simplifyVector = FALSE)

results_by_obj_path <- testthat::test_path("fixtures", "objective_results_by_objective.json")
results_by_obj <- jsonlite::fromJSON(results_by_obj_path, simplifyVector = FALSE)

results_by_ver_path <- testthat::test_path("fixtures", "objective_results_by_version.json")
results_by_ver <- jsonlite::fromJSON(results_by_ver_path, simplifyVector = FALSE)

steps_829_path <- testthat::test_path("fixtures", "step_results_829.json")
steps_829 <- jsonlite::fromJSON(steps_829_path, simplifyVector = FALSE)

steps_900_path <- testthat::test_path("fixtures", "step_results_900.json")
steps_900 <- jsonlite::fromJSON(steps_900_path, simplifyVector = FALSE)


# --- Test helpers (defined before tests that use them) ---

# Helper to parse metadata from fixture (mirrors fetch_objectives_metadata logic
# without the HTTP call)
parse_metadata_fixture <- function(raw) {
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

# Helper to build step results from fixture data (mirrors fetch_all_step_results
# logic without the HTTP calls)
build_steps_from_fixture <- function(version_ids, step_data, metadata) {
  all_steps <- list()

  for (vid in version_ids) {
    raw_steps <- step_data[[as.character(vid)]]
    if (is.null(raw_steps) || length(raw_steps) == 0) next

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

  if (length(all_steps) == 0) return(empty_steps_tibble())

  steps <- tibble::as_tibble(do.call(rbind, lapply(all_steps, as.data.frame,
                                                   stringsAsFactors = FALSE)))
  steps$objective_version_id <- as.integer(steps$objective_version_id)
  steps$step_number <- as.integer(steps$step_number)
  steps$succeeded <- as.integer(steps$succeeded)
  steps$failed <- as.integer(steps$failed)

  total <- steps$succeeded + steps$failed
  steps$step_completion_rate <- ifelse(total == 0, NA_real_,
                                       steps$succeeded / total)

  ver_meta <- metadata$versions[, c("objective_version_id", "objective_id",
                                    "version_number")]
  steps <- merge(steps, ver_meta, by = "objective_version_id", all.x = TRUE)

  obj_meta <- metadata$objectives[, c("objective_id", "objective_name")]
  steps <- merge(steps, obj_meta, by = "objective_id", all.x = TRUE)

  step_comps <- metadata$components[metadata$components$is_step == TRUE,
                                    c("objective_version_id", "step_number",
                                      "step_type", "step_detail", "step_name")]
  steps <- merge(steps, step_comps,
                 by = c("objective_version_id", "step_number"),
                 all.x = TRUE)

  steps[, c("objective_id", "objective_name", "objective_version_id",
            "version_number", "step_number", "step_type", "step_detail",
            "step_name", "succeeded", "failed", "step_completion_rate",
            "avg_completion_time_s", "avg_step_duration_s")]
}


# --- build_session_filters() tests ---

test_that("build_session_filters() returns empty list with all defaults FALSE/NULL", {
  filters <- build_session_filters(
    exclude_test = FALSE, exclude_idle = FALSE,
    start_date = NULL, end_date = NULL
  )
  expect_equal(length(filters), 0)
  expect_type(filters, "list")
})

test_that("build_session_filters() adds test filter when exclude_test = TRUE", {
  filters <- build_session_filters(
    exclude_test = TRUE, exclude_idle = FALSE,
    start_date = NULL, end_date = NULL
  )
  expect_equal(length(filters), 1)
  expect_equal(filters[[1]]$op, "eq")
  expect_equal(filters[[1]]$field$nestedFieldName, "booleanSessionProp")
  expect_equal(filters[[1]]$field$path, "c3d.session_tag.test")
  expect_false(filters[[1]]$value)
})

test_that("build_session_filters() adds junk filter when exclude_idle = TRUE", {
  filters <- build_session_filters(
    exclude_test = FALSE, exclude_idle = TRUE,
    start_date = NULL, end_date = NULL
  )
  expect_equal(length(filters), 1)
  expect_equal(filters[[1]]$field$path, "c3d.session_tag.junk")
})

test_that("build_session_filters() adds both tag filters", {
  filters <- build_session_filters(
    exclude_test = TRUE, exclude_idle = TRUE,
    start_date = NULL, end_date = NULL
  )
  expect_equal(length(filters), 2)
  paths <- vapply(filters, function(f) f$field$path, character(1))
  expect_true("c3d.session_tag.test" %in% paths)
  expect_true("c3d.session_tag.junk" %in% paths)
})

test_that("build_session_filters() adds date range filters", {
  filters <- build_session_filters(
    exclude_test = FALSE, exclude_idle = FALSE,
    start_date = "2026-01-01", end_date = "2026-02-01"
  )
  expect_equal(length(filters), 2)
  expect_equal(filters[[1]]$op, "gte")
  expect_equal(filters[[1]]$field$fieldName, "date")
  expect_equal(filters[[2]]$op, "lte")
  expect_equal(filters[[2]]$field$fieldName, "date")
})

test_that("build_session_filters() defaults end to Sys.time() when only start provided", {
  before <- round(as.numeric(Sys.time()) * 1000) - 1
  filters <- build_session_filters(
    exclude_test = FALSE, exclude_idle = FALSE,
    start_date = "2026-01-01", end_date = NULL
  )
  after <- round(as.numeric(Sys.time()) * 1000) + 1
  expect_equal(length(filters), 2)
  # The end filter value should be close to now
  end_val <- filters[[2]]$value
  expect_true(end_val >= before && end_val <= after)
})

test_that("build_session_filters() handles all parameters together", {
  filters <- build_session_filters(
    exclude_test = TRUE, exclude_idle = TRUE,
    start_date = "2026-01-01", end_date = "2026-02-01"
  )
  # 2 date filters + 2 tag filters = 4
  expect_equal(length(filters), 4)
})


# --- fetch_objectives_metadata() parsing tests ---

# We test the internal parsing by constructing metadata from fixtures directly
# (mocking the GET request by calling the parser with loaded fixture data)

test_that("parse_objectives_metadata extracts objectives correctly", {
  # Simulate what fetch_objectives_metadata does with raw data
  metadata <- parse_metadata_fixture(metadata_raw)

  expect_equal(nrow(metadata$objectives), 2)
  expect_equal(metadata$objectives$objective_id, c(268L, 310L))
  expect_equal(metadata$objectives$objective_name,
               c("Complete Tutorial", "Navigate to Checkout"))
  expect_true(all(metadata$objectives$objective_enabled))
})

test_that("parse_objectives_metadata extracts versions correctly", {
  metadata <- parse_metadata_fixture(metadata_raw)

  expect_equal(nrow(metadata$versions), 3)
  expect_equal(metadata$versions$objective_version_id, c(829L, 801L, 900L))
  expect_equal(metadata$versions$objective_id, c(268L, 268L, 310L))
  expect_equal(metadata$versions$version_is_active, c(TRUE, FALSE, TRUE))
  # version_number derived from ascending objective_version_id within objective
  # obj 268: 801 -> 1, 829 -> 2; obj 310: 900 -> 1
  expect_equal(metadata$versions$version_number, c(2L, 1L, 1L))
})

test_that("parse_objectives_metadata extracts components correctly", {
  metadata <- parse_metadata_fixture(metadata_raw)

  # 3 steps (v829) + 2 steps (v801) + 3 steps (v900) = 8
  expect_equal(nrow(metadata$components), 8)
  expect_true(all(metadata$components$is_step))

  # Check first version's steps
  v829_steps <- metadata$components[metadata$components$objective_version_id == 829L, ]
  expect_equal(nrow(v829_steps), 3)
  expect_equal(v829_steps$step_number, 1:3)
  expect_equal(v829_steps$step_name,
               c("Start Tutorial", "Look at Object", "Finish Tutorial"))
})

test_that("parse_objectives_metadata handles empty input", {
  metadata <- parse_metadata_fixture(list())

  expect_equal(nrow(metadata$objectives), 0)
  expect_equal(nrow(metadata$versions), 0)
  expect_equal(nrow(metadata$components), 0)
})


# --- parse_objective_results() tests ---

test_that("parse_objective_results handles slice_by = 'objective'", {
  metadata <- parse_metadata_fixture(metadata_raw)
  result <- parse_objective_results(results_by_obj, "objective", metadata)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(
    sort(names(result)),
    sort(c("objective_id", "objective_name", "succeeded", "failed",
           "completion_rate"))
  )
})

test_that("parse_objective_results maps objective names correctly", {
  metadata <- parse_metadata_fixture(metadata_raw)
  result <- parse_objective_results(results_by_obj, "objective", metadata)

  row268 <- result[result$objective_id == 268L, ]
  expect_equal(row268$objective_name, "Complete Tutorial")
  expect_equal(row268$succeeded, 230L)
  expect_equal(row268$failed, 15L)
})

test_that("parse_objective_results computes completion_rate correctly", {
  metadata <- parse_metadata_fixture(metadata_raw)
  result <- parse_objective_results(results_by_obj, "objective", metadata)

  row268 <- result[result$objective_id == 268L, ]
  expect_equal(row268$completion_rate, 230 / (230 + 15))

  row310 <- result[result$objective_id == 310L, ]
  expect_equal(row310$completion_rate, 0.5)
})

test_that("parse_objective_results handles slice_by = 'objective_version'", {
  metadata <- parse_metadata_fixture(metadata_raw)
  result <- parse_objective_results(results_by_ver, "objective_version", metadata)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(
    sort(names(result)),
    sort(c("objective_id", "objective_name", "objective_version_id",
           "version_number", "version_is_active", "succeeded", "failed",
           "completion_rate"))
  )
})

test_that("parse_objective_results version mode joins metadata correctly", {
  metadata <- parse_metadata_fixture(metadata_raw)
  result <- parse_objective_results(results_by_ver, "objective_version", metadata)

  row829 <- result[result$objective_version_id == 829L, ]
  expect_equal(row829$objective_id, 268L)
  expect_equal(row829$objective_name, "Complete Tutorial")
  expect_true(row829$version_is_active)

  row801 <- result[result$objective_version_id == 801L, ]
  expect_false(row801$version_is_active)
})

test_that("parse_objective_results handles empty results", {
  metadata <- parse_metadata_fixture(metadata_raw)
  result <- parse_objective_results(list(), "objective", metadata)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true("objective_id" %in% names(result))
  expect_true("completion_rate" %in% names(result))
})


# --- Completion rate edge cases ---

test_that("completion_rate is NA when succeeded + failed == 0", {
  metadata <- parse_metadata_fixture(metadata_raw)
  zero_results <- list(
    list(objectiveId = 268L, succeeded = 0L, failed = 0L)
  )
  result <- parse_objective_results(zero_results, "objective", metadata)
  expect_true(is.na(result$completion_rate[1]))
})

test_that("completion_rate handles perfect success", {
  metadata <- parse_metadata_fixture(metadata_raw)
  perfect_results <- list(
    list(objectiveId = 268L, succeeded = 100L, failed = 0L)
  )
  result <- parse_objective_results(perfect_results, "objective", metadata)
  expect_equal(result$completion_rate[1], 1.0)
})

test_that("completion_rate handles total failure", {
  metadata <- parse_metadata_fixture(metadata_raw)
  fail_results <- list(
    list(objectiveId = 268L, succeeded = 0L, failed = 50L)
  )
  result <- parse_objective_results(fail_results, "objective", metadata)
  expect_equal(result$completion_rate[1], 0.0)
})


# --- Step results parsing tests ---

test_that("fetch_all_step_results parses step data correctly", {
  metadata <- parse_metadata_fixture(metadata_raw)

  # Mock: build step results as fetch_all_step_results would produce internally
  steps <- build_steps_from_fixture(
    version_ids = c(829L),
    step_data = list("829" = steps_829),
    metadata = metadata
  )

  expect_s3_class(steps, "data.frame")
  expect_equal(nrow(steps), 3)
  expect_equal(
    sort(names(steps)),
    sort(c("objective_id", "objective_name", "objective_version_id",
           "version_number", "step_number", "step_type", "step_detail",
           "step_name", "succeeded", "failed", "step_completion_rate",
           "avg_completion_time_s", "avg_step_duration_s"))
  )
})

test_that("step results join metadata correctly", {
  metadata <- parse_metadata_fixture(metadata_raw)
  steps <- build_steps_from_fixture(
    version_ids = c(829L),
    step_data = list("829" = steps_829),
    metadata = metadata
  )

  expect_equal(steps$objective_id[1], 268L)
  expect_equal(steps$objective_name[1], "Complete Tutorial")
  expect_equal(steps$step_name[1], "Start Tutorial")
  expect_equal(steps$step_type[1], "eventstep")
  expect_equal(steps$step_detail[1], "c3d.tutorial.start")
  # Step 2 is a gazestep with dynamicObjectIds
  expect_equal(steps$step_type[2], "gazestep")
  expect_equal(steps$step_detail[2], "obj_abc, obj_def")
})

test_that("step completion_rate is computed correctly", {
  metadata <- parse_metadata_fixture(metadata_raw)
  steps <- build_steps_from_fixture(
    version_ids = c(829L),
    step_data = list("829" = steps_829),
    metadata = metadata
  )

  # Step 1: 200 / (200 + 0) = 1.0
  expect_equal(steps$step_completion_rate[steps$step_number == 1L], 1.0)
  # Step 2: 180 / (180 + 20) = 0.9
  expect_equal(steps$step_completion_rate[steps$step_number == 2L], 0.9)
  # Step 3: 150 / (150 + 50) = 0.75
  expect_equal(steps$step_completion_rate[steps$step_number == 3L], 0.75)
})

test_that("step timing fields are parsed correctly", {
  metadata <- parse_metadata_fixture(metadata_raw)
  steps <- build_steps_from_fixture(
    version_ids = c(829L),
    step_data = list("829" = steps_829),
    metadata = metadata
  )

  expect_equal(steps$avg_completion_time_s[1], 5.0005)
  expect_equal(steps$avg_step_duration_s[1], 1.2003)
})

test_that("step results handle multiple versions", {
  metadata <- parse_metadata_fixture(metadata_raw)
  steps <- build_steps_from_fixture(
    version_ids = c(829L, 900L),
    step_data = list("829" = steps_829, "900" = steps_900),
    metadata = metadata
  )

  # 3 steps from v829 + 3 steps from v900 = 6
  expect_equal(nrow(steps), 6)
  v829_steps <- steps[steps$objective_version_id == 829L, ]
  v900_steps <- steps[steps$objective_version_id == 900L, ]
  expect_equal(nrow(v829_steps), 3)
  expect_equal(nrow(v900_steps), 3)
  expect_equal(v900_steps$objective_name[1], "Navigate to Checkout")
})

test_that("empty step results return correct structure", {
  result <- empty_steps_tibble()
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_true("step_type" %in% names(result))
  expect_true("step_detail" %in% names(result))
  expect_true("step_completion_rate" %in% names(result))
  expect_true("avg_completion_time_s" %in% names(result))
})

test_that("step_type and step_detail are populated for mixed component types", {
  metadata <- parse_metadata_fixture(metadata_raw)
  steps <- build_steps_from_fixture(
    version_ids = c(900L),
    step_data = list("900" = steps_900),
    metadata = metadata
  )

  # v900 step 1: eventstep
  s1 <- steps[steps$step_number == 1L, ]
  expect_equal(s1$step_type, "eventstep")
  expect_equal(s1$step_detail, "c3d.nav.open_menu")

  # v900 step 2: exitpollstep
  s2 <- steps[steps$step_number == 2L, ]
  expect_equal(s2$step_type, "exitpollstep")
  expect_equal(s2$step_detail, "assessment_end_survey:1")

  # v900 step 3: fixationstep
  s3 <- steps[steps$step_number == 3L, ]
  expect_equal(s3$step_type, "fixationstep")
  expect_equal(s3$step_detail, "checkout_btn")
})


# --- empty_objective_results() tests ---

test_that("empty_objective_results returns correct columns for objective slice", {
  result <- empty_objective_results("objective", include_steps = FALSE)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_equal(names(result),
               c("objective_id", "objective_name", "succeeded", "failed",
                 "completion_rate"))
})

test_that("empty_objective_results returns correct columns for version slice", {
  result <- empty_objective_results("objective_version", include_steps = FALSE)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_true("objective_version_id" %in% names(result))
  expect_true("version_number" %in% names(result))
  expect_true("version_is_active" %in% names(result))
})

test_that("empty_objective_results with steps returns empty steps tibble", {
  result <- empty_objective_results("objective", include_steps = TRUE)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_true("step_type" %in% names(result))
  expect_true("step_completion_rate" %in% names(result))
})


# --- derive_step_detail() tests ---

test_that("derive_step_detail returns eventName for eventstep", {
  comp <- list(type = "eventstep", eventName = "my.event")
  expect_equal(derive_step_detail(comp), "my.event")
})

test_that("derive_step_detail returns exitpollQuestionSetId for exitpollstep", {
  comp <- list(type = "exitpollstep", exitpollQuestionSetId = "survey:1")
  expect_equal(derive_step_detail(comp), "survey:1")
})

test_that("derive_step_detail returns collapsed dynamicObjectIds for gazestep", {
  comp <- list(type = "gazestep", dynamicObjectIds = list("a", "b", "c"))
  expect_equal(derive_step_detail(comp), "a, b, c")
})

test_that("derive_step_detail returns collapsed dynamicObjectIds for fixationstep", {
  comp <- list(type = "fixationstep", dynamicObjectIds = list("obj1"))
  expect_equal(derive_step_detail(comp), "obj1")
})

test_that("derive_step_detail returns collapsed dynamicObjectIds for mediapointstep", {
  comp <- list(type = "mediapointstep", dynamicObjectIds = list("mp1", "mp2"))
  expect_equal(derive_step_detail(comp), "mp1, mp2")
})

test_that("derive_step_detail returns NA for mediapointstep with no dynamicObjectIds", {
  comp <- list(type = "mediapointstep")
  expect_true(is.na(derive_step_detail(comp)))
})

test_that("derive_step_detail returns NA for unknown type", {
  comp <- list(type = "unknownstep")
  expect_true(is.na(derive_step_detail(comp)))
})

test_that("derive_step_detail returns NA when type is missing", {
  comp <- list(eventName = "something")
  expect_true(is.na(derive_step_detail(comp)))
})


# --- resolve_step_details() tests ---

test_that("resolve_step_details replaces sdkIds with friendly names for gazestep", {
  metadata <- parse_metadata_fixture(metadata_raw)
  lookup <- c(obj_abc = "Tutorial Board", obj_def = "Info Panel")
  resolved <- resolve_step_details(metadata$components, lookup)

  # v829 step 2 is a gazestep with dynamicObjectIds "obj_abc, obj_def"
  gaze_row <- resolved[resolved$objective_version_id == 829L &
                        resolved$step_number == 2L, ]
  expect_equal(gaze_row$step_detail, "Tutorial Board, Info Panel")
})

test_that("resolve_step_details replaces sdkIds for fixationstep", {
  metadata <- parse_metadata_fixture(metadata_raw)
  lookup <- c(checkout_btn = "Checkout Button")
  resolved <- resolve_step_details(metadata$components, lookup)

  # v900 step 3 is a fixationstep with "checkout_btn"
  fix_row <- resolved[resolved$objective_version_id == 900L &
                       resolved$step_number == 3L, ]
  expect_equal(fix_row$step_detail, "Checkout Button")
})

test_that("resolve_step_details leaves eventstep and exitpollstep unchanged", {
  metadata <- parse_metadata_fixture(metadata_raw)
  lookup <- c(obj_abc = "Tutorial Board")
  resolved <- resolve_step_details(metadata$components, lookup)

  # v829 step 1 is an eventstep — should be unchanged
  event_row <- resolved[resolved$objective_version_id == 829L &
                         resolved$step_number == 1L, ]
  expect_equal(event_row$step_detail, "c3d.tutorial.start")

  # v900 step 2 is an exitpollstep — should be unchanged
  poll_row <- resolved[resolved$objective_version_id == 900L &
                        resolved$step_number == 2L, ]
  expect_equal(poll_row$step_detail, "assessment_end_survey:1")
})

test_that("resolve_step_details keeps unmatched sdkIds as-is", {
  metadata <- parse_metadata_fixture(metadata_raw)
  # Lookup only has obj_abc, not obj_def
  lookup <- c(obj_abc = "Tutorial Board")
  resolved <- resolve_step_details(metadata$components, lookup)

  gaze_row <- resolved[resolved$objective_version_id == 829L &
                        resolved$step_number == 2L, ]
  expect_equal(gaze_row$step_detail, "Tutorial Board, obj_def")
})

test_that("resolve_step_details handles empty lookup", {
  metadata <- parse_metadata_fixture(metadata_raw)
  resolved <- resolve_step_details(metadata$components, character(0))

  # Everything should be unchanged
  expect_equal(resolved$step_detail, metadata$components$step_detail)
})

test_that("resolve_step_details handles empty components", {
  empty_comps <- tibble::tibble(
    objective_version_id = integer(0),
    step_number = integer(0),
    step_type = character(0),
    step_detail = character(0),
    step_name = character(0),
    is_step = logical(0)
  )
  lookup <- c(obj_abc = "Tutorial Board")
  resolved <- resolve_step_details(empty_comps, lookup)
  expect_equal(nrow(resolved), 0)
})


# --- Input validation tests ---

test_that("c3d_objective_results() errors without project_id", {
  c3d_auth("test_key")
  .c3d_env$project_id <- NULL
  expect_error(c3d_objective_results(), "No default project")
})

test_that("c3d_objective_results() validates slice_by argument", {
  c3d_auth("test_key")
  c3d_project(4460)
  expect_error(
    c3d_objective_results(slice_by = "invalid"),
    "should be one of"
  )
})
