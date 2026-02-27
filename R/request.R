# Internal: make an authenticated POST request to the Cognitive3D API
#
# @param endpoint Character. API path (e.g. "/v0/datasets/sessions/paginatedListQueries").
# @param body List. Will be serialized to JSON.
# @return Parsed JSON response as a list.
# @keywords internal
c3d_request <- function(endpoint, body) {
  api_key <- get_api_key()
  url <- paste0("https://api.cognitive3d.com", endpoint)

  resp <- httr2::request(url) |>
    httr2::req_headers(Authorization = api_key) |>
    httr2::req_body_json(body) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  c3d_check_status(resp, url)
}

# Internal: make an authenticated GET request to the Cognitive3D API
#
# @param endpoint Character. API path (e.g. "/v0/projects/123/objectives").
# @return Parsed JSON response as a list.
# @keywords internal
c3d_get <- function(endpoint) {
  api_key <- get_api_key()
  url <- paste0("https://api.cognitive3d.com", endpoint)

  resp <- httr2::request(url) |>
    httr2::req_headers(Authorization = api_key) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  c3d_check_status(resp, url)
}

# Internal: check HTTP status and parse response
#
# @param resp httr2 response object.
# @param url Character. Request URL (for error messages).
# @return Parsed JSON response as a list.
# @keywords internal
c3d_check_status <- function(resp, url) {
  status <- httr2::resp_status(resp)

  if (status >= 400) {
    body_text <- tryCatch(
      httr2::resp_body_string(resp),
      error = function(e) "(no response body)"
    )
    cli::cli_abort(c(
      "Cognitive3D API request failed.",
      "x" = "HTTP {status}: {.url {url}}",
      "i" = "Response: {body_text}"
    ))
  }

  httr2::resp_body_json(resp)
}
