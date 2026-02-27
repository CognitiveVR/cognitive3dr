# Load fixture
objects_path <- testthat::test_path("fixtures", "objects_response.json")
objects_raw <- jsonlite::fromJSON(objects_path, simplifyVector = FALSE)


# --- fetch_objects_lookup() parsing tests ---
# We test the internal parsing logic by replicating what fetch_objects_lookup
# does with raw data (without the HTTP call).

build_lookup_from_fixture <- function(raw) {
  if (length(raw) == 0) {
    return(stats::setNames(character(0), character(0)))
  }
  sdk_ids <- vapply(raw, function(obj) {
    as.character(obj$sdkId %||% NA_character_)
  }, character(1))
  names_vec <- vapply(raw, function(obj) {
    as.character(obj$name %||% NA_character_)
  }, character(1))
  valid <- !is.na(sdk_ids)
  stats::setNames(names_vec[valid], sdk_ids[valid])
}

test_that("fetch_objects_lookup returns named character vector", {
  lookup <- build_lookup_from_fixture(objects_raw)
  expect_type(lookup, "character")
  expect_true(length(lookup) > 0)
  expect_true(all(nzchar(names(lookup))))
})

test_that("fetch_objects_lookup maps sdkId to name correctly", {
  lookup <- build_lookup_from_fixture(objects_raw)
  expect_equal(unname(lookup[["dyn_obj_42"]]), "Power Console")
  expect_equal(unname(lookup[["obj_abc"]]), "Tutorial Board")
  expect_equal(unname(lookup[["obj_def"]]), "Info Panel")
  expect_equal(unname(lookup[["checkout_btn"]]), "Checkout Button")
})

test_that("fetch_objects_lookup returns correct count", {
  lookup <- build_lookup_from_fixture(objects_raw)
  expect_equal(length(lookup), 4)
})

test_that("fetch_objects_lookup handles empty response", {
  lookup <- build_lookup_from_fixture(list())
  expect_type(lookup, "character")
  expect_equal(length(lookup), 0)
})

test_that("fetch_objects_lookup drops entries with missing sdkId", {
  raw_with_missing <- c(objects_raw, list(list(
    id = 99999,
    name = "No SDK ID",
    sdkId = NULL
  )))
  lookup <- build_lookup_from_fixture(raw_with_missing)
  # Should still be 4 (the NULL sdkId entry is dropped)
  expect_equal(length(lookup), 4)
  expect_false("No SDK ID" %in% lookup)
})
