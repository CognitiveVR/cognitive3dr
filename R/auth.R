# Internal environment to store credentials
# Not exported — prevents API key from leaking into global options
.c3d_env <- new.env(parent = emptyenv())

#' Authenticate with the Cognitive3D API
#'
#' Stores an API key for use in subsequent requests. If no key is provided,
#' looks for the `C3D_API_KEY` environment variable.
#'
#' @param api_key Character string. Your Cognitive3D API key. If `NULL`,
#'   reads from the `C3D_API_KEY` environment variable.
#' @return Invisibly returns `TRUE` on success.
#' @export
#' @examples
#' \dontrun{
#' # Set key directly
#' c3d_auth("your_api_key_here")
#'
#' # Or set C3D_API_KEY env var and call without arguments
#' Sys.setenv(C3D_API_KEY = "your_api_key_here")
#' c3d_auth()
#' }
c3d_auth <- function(api_key = NULL) {
  if (!is.null(api_key)) {
    if (!is.character(api_key) || nchar(api_key) == 0) {
      cli::cli_abort("{.arg api_key} must be a non-empty string.")
    }
    .c3d_env$api_key <- api_key
    cli::cli_alert_success("API key stored.")
    return(invisible(TRUE))
  }

  env_key <- Sys.getenv("C3D_API_KEY", unset = "")
  if (nchar(env_key) > 0) {
    .c3d_env$api_key <- env_key
    cli::cli_alert_success("API key loaded from {.envvar C3D_API_KEY}.")
    return(invisible(TRUE))
  }

  cli::cli_abort(c(
    "No API key found.",
    "i" = "Provide {.arg api_key} directly or set the {.envvar C3D_API_KEY} environment variable."
  ))
}

#' Set the default Cognitive3D project ID
#'
#' Stores a project ID so it doesn't need to be repeated on every
#' `c3d_sessions()` call.
#'
#' @param project_id Integer. The Cognitive3D project ID.
#' @return Invisibly returns the `project_id`.
#' @export
#' @examples
#' \dontrun{
#' c3d_project(4460)
#' }
c3d_project <- function(project_id) {
  if (missing(project_id) || is.null(project_id)) {
    cli::cli_abort("{.arg project_id} is required.")
  }
  project_id <- as.integer(project_id)
  if (is.na(project_id)) {
    cli::cli_abort("{.arg project_id} must be a valid integer.")
  }
  .c3d_env$project_id <- project_id
  cli::cli_alert_success("Default project set to {.val {project_id}}.")
  invisible(project_id)
}

# Internal helper: retrieve stored API key or abort
get_api_key <- function() {
  key <- .c3d_env$api_key
  if (is.null(key)) {
    cli::cli_abort(c(
      "Not authenticated.",
      "i" = "Call {.fn c3d_auth} first."
    ))
  }
  key
}

# Internal helper: retrieve stored project ID or abort
get_project_id <- function() {
  id <- .c3d_env$project_id
  if (is.null(id)) {
    cli::cli_abort(c(
      "No default project set.",
      "i" = "Call {.fn c3d_project} or pass {.arg project_id} directly."
    ))
  }
  id
}
