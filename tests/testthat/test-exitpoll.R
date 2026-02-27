# Load fixtures
hooks_path <- testthat::test_path("fixtures", "exitpoll_hooks.json")
hooks_raw <- jsonlite::fromJSON(hooks_path, simplifyVector = FALSE)

questionset_path <- testthat::test_path("fixtures", "exitpoll_questionset.json")
questionset_raw <- jsonlite::fromJSON(questionset_path, simplifyVector = FALSE)

responses_v3_path <- testthat::test_path("fixtures", "exitpoll_responses_v3.json")
responses_v3_raw <- jsonlite::fromJSON(responses_v3_path, simplifyVector = FALSE)


# --- Test helpers ---

# Helper to parse metadata from fixture (mirrors fetch_exitpoll_metadata logic
# without the HTTP call)
parse_metadata_fixture <- function(raw) {
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


# --- Metadata parsing tests ---

test_that("metadata parsing extracts versions correctly", {
  metadata <- parse_metadata_fixture(questionset_raw)

  expect_equal(nrow(metadata$versions), 2)
  expect_equal(metadata$versions$version, c(2L, 3L))
  expect_equal(metadata$versions$question_set_id,
               c("end_questions:2", "end_questions:3"))
  expect_equal(metadata$versions$title,
               c("End Feedback v2", "End Feedback v3"))
})

test_that("metadata parsing extracts questions correctly", {
  metadata <- parse_metadata_fixture(questionset_raw)

  # Version 2 has 4 questions, version 3 has 5
  expect_equal(nrow(metadata$questions), 9)

  v2_qs <- metadata$questions[metadata$questions$version == 2L, ]
  expect_equal(nrow(v2_qs), 4)
  expect_equal(v2_qs$question_type,
               c("boolean", "scale", "multiple", "happysad"))

  v3_qs <- metadata$questions[metadata$questions$version == 3L, ]
  expect_equal(nrow(v3_qs), 5)
  expect_equal(v3_qs$question_type,
               c("boolean", "scale", "multiple", "thumbs", "voice"))
})

test_that("metadata parsing stores MULTIPLE answers correctly", {
  metadata <- parse_metadata_fixture(questionset_raw)

  # Version 2 question 3 is MULTIPLE with 3 answers
  v2_multiple <- metadata$questions[metadata$questions$version == 2L &
                                      metadata$questions$question_type == "multiple", ]
  answers <- v2_multiple$answers[[1]]
  expect_equal(length(answers), 3)
  expect_equal(answers[[1]]$answer, "Female")
  expect_equal(answers[[3]]$answer, "Prefer not to say")

  # Version 3 question 3 is MULTIPLE with 4 answers
  v3_multiple <- metadata$questions[metadata$questions$version == 3L &
                                      metadata$questions$question_type == "multiple", ]
  answers_v3 <- v3_multiple$answers[[1]]
  expect_equal(length(answers_v3), 4)
  expect_equal(answers_v3[[4]]$answer, "Nonbinary")
})

test_that("metadata parsing handles empty response", {
  metadata <- parse_metadata_fixture(list())

  expect_equal(nrow(metadata$versions), 0)
  expect_equal(nrow(metadata$questions), 0)
})

test_that("metadata parsing trims leading/trailing whitespace from question titles", {
  metadata <- parse_metadata_fixture(questionset_raw)

  # Fixture has leading spaces on v2 SCALE, MULTIPLE, and HAPPYSAD titles
  v2_qs <- metadata$questions[metadata$questions$version == 2L, ]
  expect_equal(v2_qs$question_title[2], "How comfortable was the experience?")
  expect_equal(v2_qs$question_title[3], "What is your gender?")
  expect_equal(v2_qs$question_title[4], "How did you feel about the experience?")
})

test_that("metadata parsing assigns 1-based question indices", {
  metadata <- parse_metadata_fixture(questionset_raw)

  v3_qs <- metadata$questions[metadata$questions$version == 3L, ]
  expect_equal(v3_qs$question_index, 1:5)
})


# --- map_value_label() tests ---

test_that("map_value_label maps boolean 0 to False", {
  expect_equal(map_value_label("0", "boolean", list()), "False")
})

test_that("map_value_label maps boolean 1 to True", {
  expect_equal(map_value_label("1", "boolean", list()), "True")
})

test_that("map_value_label maps scale value as-is", {
  expect_equal(map_value_label("7", "scale", list()), "7")
  expect_equal(map_value_label("10", "scale", list()), "10")
})

test_that("map_value_label maps happysad 0 to Sad", {
  expect_equal(map_value_label("0", "happysad", list()), "Sad")
})

test_that("map_value_label maps happysad 1 to Happy", {
  expect_equal(map_value_label("1", "happysad", list()), "Happy")
})

test_that("map_value_label maps multiple index to answer text", {
  answers <- list(
    list(icon = NULL, answer = "Female"),
    list(icon = NULL, answer = "Male"),
    list(icon = NULL, answer = "Prefer not to say")
  )
  q_meta <- list(answers = answers)

  expect_equal(map_value_label("0", "multiple", q_meta), "Female")
  expect_equal(map_value_label("1", "multiple", q_meta), "Male")
  expect_equal(map_value_label("2", "multiple", q_meta), "Prefer not to say")
})

test_that("map_value_label returns raw value for multiple out-of-range index", {
  answers <- list(list(icon = NULL, answer = "Only One"))
  q_meta <- list(answers = answers)

  expect_equal(map_value_label("5", "multiple", q_meta), "5")
})

test_that("map_value_label maps thumbs 0 to Down", {
  expect_equal(map_value_label("0", "thumbs", list()), "Down")
})

test_that("map_value_label maps thumbs 1 to Up", {
  expect_equal(map_value_label("1", "thumbs", list()), "Up")
})

test_that("map_value_label maps voice 0 to Responded", {
  expect_equal(map_value_label("0", "voice", list()), "Responded")
})

test_that("map_value_label returns NA for NULL value", {
  expect_true(is.na(map_value_label(NULL, "boolean", list())))
})

test_that("map_value_label returns NA for NA value", {
  expect_true(is.na(map_value_label(NA_character_, "boolean", list())))
})

test_that("map_value_label returns raw value for unknown type", {
  expect_equal(map_value_label("42", "newtype", list()), "42")
})

test_that("map_value_label returns raw value for unknown boolean value", {
  expect_equal(map_value_label("99", "boolean", list()), "99")
})


# --- parse_exitpoll_responses() tests ---

test_that("parse_exitpoll_responses returns correct number of rows", {
  metadata <- parse_metadata_fixture(questionset_raw)
  v3_qs <- metadata$questions[metadata$questions$version == 3L, ]

  result <- parse_exitpoll_responses(responses_v3_raw, 3L, "end_questions", v3_qs)

  # Q1: 2 rows, Q2: 3 rows, Q3: 4 rows, Q4: 2 rows, Q5: 2 rows = 13
  expect_equal(nrow(result), 13)
})

test_that("parse_exitpoll_responses has correct column names", {
  metadata <- parse_metadata_fixture(questionset_raw)
  v3_qs <- metadata$questions[metadata$questions$version == 3L, ]

  result <- parse_exitpoll_responses(responses_v3_raw, 3L, "end_questions", v3_qs)

  expect_equal(names(result),
               c("hook", "version", "question_index", "question_title",
                 "question_type", "value", "value_label", "count"))
})

test_that("parse_exitpoll_responses has correct column types", {
  metadata <- parse_metadata_fixture(questionset_raw)
  v3_qs <- metadata$questions[metadata$questions$version == 3L, ]

  result <- parse_exitpoll_responses(responses_v3_raw, 3L, "end_questions", v3_qs)

  expect_type(result$hook, "character")
  expect_type(result$version, "integer")
  expect_type(result$question_index, "integer")
  expect_type(result$question_title, "character")
  expect_type(result$question_type, "character")
  expect_type(result$value, "character")
  expect_type(result$value_label, "character")
  expect_type(result$count, "integer")
})

test_that("parse_exitpoll_responses maps BOOLEAN labels correctly", {
  metadata <- parse_metadata_fixture(questionset_raw)
  v3_qs <- metadata$questions[metadata$questions$version == 3L, ]

  result <- parse_exitpoll_responses(responses_v3_raw, 3L, "end_questions", v3_qs)

  # Question 1 is boolean
  q1 <- result[result$question_index == 1L, ]
  expect_equal(q1$question_type, c("boolean", "boolean"))
  expect_equal(q1$value, c("0", "1"))
  expect_equal(q1$value_label, c("False", "True"))
  expect_equal(q1$count, c(5L, 3L))
})

test_that("parse_exitpoll_responses maps SCALE labels as raw values", {
  metadata <- parse_metadata_fixture(questionset_raw)
  v3_qs <- metadata$questions[metadata$questions$version == 3L, ]

  result <- parse_exitpoll_responses(responses_v3_raw, 3L, "end_questions", v3_qs)

  # Question 2 is SCALE
  q2 <- result[result$question_index == 2L, ]
  expect_equal(q2$value, c("7", "10", "5"))
  expect_equal(q2$value_label, c("7", "10", "5"))
})

test_that("parse_exitpoll_responses maps MULTIPLE labels to answer text", {
  metadata <- parse_metadata_fixture(questionset_raw)
  v3_qs <- metadata$questions[metadata$questions$version == 3L, ]

  result <- parse_exitpoll_responses(responses_v3_raw, 3L, "end_questions", v3_qs)

  # Question 3 is MULTIPLE with answers: Female, Male, Prefer not to say, Nonbinary
  q3 <- result[result$question_index == 3L, ]
  expect_equal(q3$value, c("0", "1", "2", "3"))
  expect_equal(q3$value_label, c("Female", "Male", "Prefer not to say", "Nonbinary"))
})

test_that("parse_exitpoll_responses maps THUMBS labels correctly", {
  metadata <- parse_metadata_fixture(questionset_raw)
  v3_qs <- metadata$questions[metadata$questions$version == 3L, ]

  result <- parse_exitpoll_responses(responses_v3_raw, 3L, "end_questions", v3_qs)

  # Question 4 is THUMBS
  q4 <- result[result$question_index == 4L, ]
  expect_equal(q4$value, c("1", "0"))
  expect_equal(q4$value_label, c("Up", "Down"))
})

test_that("parse_exitpoll_responses marks skipped entries correctly", {
  metadata <- parse_metadata_fixture(questionset_raw)
  v3_qs <- metadata$questions[metadata$questions$version == 3L, ]

  result <- parse_exitpoll_responses(responses_v3_raw, 3L, "end_questions", v3_qs)

  # Question 5 (voice) has one skipped entry
  q5 <- result[result$question_index == 5L, ]
  expect_equal(nrow(q5), 2)

  responded <- q5[q5$value != "skipped", ]
  expect_equal(responded$value, "0")
  expect_equal(responded$value_label, "Responded")
  expect_equal(responded$count, 7L)

  skipped <- q5[q5$value == "skipped", ]
  expect_equal(skipped$value, "skipped")
  expect_equal(skipped$value_label, "skipped")
  expect_equal(skipped$count, 1L)
})

test_that("parse_exitpoll_responses preserves hook and version", {
  metadata <- parse_metadata_fixture(questionset_raw)
  v3_qs <- metadata$questions[metadata$questions$version == 3L, ]

  result <- parse_exitpoll_responses(responses_v3_raw, 3L, "end_questions", v3_qs)

  expect_true(all(result$hook == "end_questions"))
  expect_true(all(result$version == 3L))
})

test_that("parse_exitpoll_responses handles empty counts", {
  metadata <- parse_metadata_fixture(questionset_raw)
  v3_qs <- metadata$questions[metadata$questions$version == 3L, ]

  result <- parse_exitpoll_responses(list(), 3L, "end_questions", v3_qs)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("parse_exitpoll_responses warns on count/question mismatch", {
  metadata <- parse_metadata_fixture(questionset_raw)
  v3_qs <- metadata$questions[metadata$questions$version == 3L, ]

  # Only pass 2 response arrays for 5 questions
  truncated <- responses_v3_raw[1:2]

  expect_warning(
    result <- parse_exitpoll_responses(truncated, 3L, "end_questions", v3_qs),
    "does not match"
  )
  # Should still process the 2 available questions
  expect_equal(length(unique(result$question_index)), 2)
})


# --- empty_exitpoll_tibble() tests ---

test_that("empty_exitpoll_tibble has correct structure", {
  result <- empty_exitpoll_tibble()

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_equal(names(result),
               c("hook", "version", "question_index", "question_title",
                 "question_type", "value", "value_label", "count"))
})

test_that("empty_exitpoll_tibble has correct column types", {
  result <- empty_exitpoll_tibble()

  expect_type(result$hook, "character")
  expect_type(result$version, "integer")
  expect_type(result$question_index, "integer")
  expect_type(result$question_title, "character")
  expect_type(result$question_type, "character")
  expect_type(result$value, "character")
  expect_type(result$value_label, "character")
  expect_type(result$count, "integer")
})


# --- Hooks parsing tests ---

test_that("hooks parsing returns character vector", {
  hooks <- vapply(hooks_raw, function(h) {
    as.character(h$name %||% h$id %||% NA_character_)
  }, character(1))
  hooks <- unique(hooks[!is.na(hooks)])

  expect_type(hooks, "character")
  expect_equal(length(hooks), 2)
  expect_true("end_questions" %in% hooks)
  expect_true("mid_session_check" %in% hooks)
})

test_that("hooks parsing handles empty response", {
  hooks <- vapply(list(), function(h) {
    as.character(h$name %||% h$id %||% NA_character_)
  }, character(1))
  expect_equal(length(hooks), 0)
  expect_type(hooks, "character")
})


# --- Input validation tests ---

test_that("c3d_exitpoll errors when hook is empty string", {
  c3d_auth("test_key")
  c3d_project(2797)
  expect_error(c3d_exitpoll(hook = ""), "hook")
})

test_that("c3d_exitpoll errors without project_id", {
  c3d_auth("test_key")
  .c3d_env$project_id <- NULL
  expect_error(c3d_exitpoll(hook = "end_questions"), "No default project")
})

test_that("c3d_exitpoll errors with invalid version", {
  c3d_auth("test_key")
  c3d_project(2797)
  expect_error(
    suppressWarnings(c3d_exitpoll(hook = "end_questions", version = "abc")),
    "version"
  )
})
