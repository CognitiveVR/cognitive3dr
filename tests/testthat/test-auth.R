test_that("c3d_auth() stores a provided API key", {
  c3d_auth("test_key_123")
  expect_equal(get_api_key(), "test_key_123")
})

test_that("c3d_auth() reads from C3D_API_KEY env var", {
  .c3d_env$api_key <- NULL

  withr::with_envvar(c(C3D_API_KEY = "env_key_456"), {
    c3d_auth()
    expect_equal(get_api_key(), "env_key_456")
  })
})

test_that("c3d_auth() errors when no key is available", {
  .c3d_env$api_key <- NULL

  withr::with_envvar(c(C3D_API_KEY = ""), {
    expect_error(c3d_auth(), "No API key found")
  })
})

test_that("c3d_auth() rejects empty string", {
  expect_error(c3d_auth(""), "non-empty string")
})

test_that("c3d_project() stores a project ID", {
  c3d_project(4460)
  expect_equal(get_project_id(), 4460L)
})

test_that("c3d_project() coerces to integer", {
  c3d_project(4460.5)
  expect_equal(get_project_id(), 4460L)
})

test_that("c3d_project() errors on missing arg", {
  expect_error(c3d_project(), "required")
})

test_that("get_api_key() errors when not authenticated", {
  .c3d_env$api_key <- NULL
  expect_error(get_api_key(), "Not authenticated")
})

test_that("get_project_id() errors when no project set", {
  .c3d_env$project_id <- NULL
  expect_error(get_project_id(), "No default project")
})
