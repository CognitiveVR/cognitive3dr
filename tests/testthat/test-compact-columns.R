# Guard compact_columns against drift from the cortex field registry.
#
# The list is curated by hand, so it can go stale in two ways: a column can
# name a key the registry has retired, or the hand-maintained lifecycle lists
# in R/registry.R can themselves fall behind the registry. The first three
# tests always run; the last one runs only when a cvr-cortex checkout sits
# alongside this repo.

# Registry-retired columns deliberately kept in compact output.
# Name = column, value = why it stays. Empty today.
lifecycle_exceptions <- character(0)

cortex_yaml_path <- function() {
  candidates <- c(
    Sys.getenv("SLICER_YAML", unset = NA_character_),
    file.path("..", "..", "..", "cvr-cortex", "features", "slicer",
              "slicer_fields.yaml")
  )
  candidates <- candidates[!is.na(candidates)]
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) NULL else normalizePath(hit[[1]])
}

test_that("compact_columns has no duplicates", {
  expect_equal(anyDuplicated(compact_columns), 0)
})

test_that("no registry-deprecated columns in compact output", {
  offenders <- setdiff(
    intersect(compact_columns, deprecated_registry_columns()),
    lifecycle_exceptions
  )
  expect(
    length(offenders) == 0,
    paste(
      "Deprecated in the registry:", paste(offenders, collapse = ", "),
      "- drop them from compact_columns (the data stays available via",
      "compact = FALSE), or add them to lifecycle_exceptions with a reason."
    )
  )
})

test_that("no registry-sunsetted columns in compact output", {
  offenders <- setdiff(
    intersect(compact_columns, sunsetted_registry_columns()),
    lifecycle_exceptions
  )
  expect(
    length(offenders) == 0,
    paste(
      "Sunsetted in the registry:", paste(offenders, collapse = ", "),
      "- drop them, or add them to lifecycle_exceptions with a reason."
    )
  )
})

test_that("lifecycle lists match the cortex registry", {
  skip_if_not_installed("yaml")
  yaml_path <- cortex_yaml_path()
  skip_if(is.null(yaml_path), "cvr-cortex checkout not found")

  registry <- yaml::read_yaml(yaml_path)

  flagged <- function(section, grouped, flag) {
    entries <- registry[[section]]
    if (is.null(entries)) return(character(0))
    if (grouped) entries <- unlist(unname(entries), recursive = FALSE)
    keys <- names(entries)
    if (is.null(keys)) return(character(0))
    keys[vapply(entries, function(meta) isTRUE(meta[[flag]]), logical(1))]
  }

  collect <- function(flag) {
    sort(unique(c(
      flagged("session_fields", FALSE, flag),
      flagged("event_fields", FALSE, flag),
      flagged("session_properties", TRUE, flag),
      flagged("event_properties", TRUE, flag)
    )))
  }

  expect_equal(
    sort(deprecated_registry_keys), collect("deprecated"),
    info = "R/registry.R deprecated_registry_keys is stale — resync it."
  )
  expect_equal(
    sort(sunsetted_registry_keys), collect("sunsetted"),
    info = "R/registry.R sunsetted_registry_keys is stale — resync it."
  )
})
