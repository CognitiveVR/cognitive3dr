# Internal: fetch scenes metadata for a project
#
# Calls `GET /v0/projects/{project_id}` and extracts the `scenes` array.
# Returns a structured list with a versions tibble and a version_id -> scene_name
# lookup vector.
#
# @param project_id Integer. Project ID.
# @return A list with:
#   \item{versions}{A tibble with columns `scene_id` (chr), `scene_name` (chr),
#     `version_id` (int), `version_number` (int).}
#   \item{lookup}{A named character vector mapping `as.character(version_id)` to
#     `scene_name`.}
# @keywords internal
fetch_scenes_metadata <- function(project_id) {
  endpoint <- paste0("/v0/projects/", project_id)
  raw <- c3d_get(endpoint)

  scenes <- raw[["scenes"]]

  empty_versions <- tibble::tibble(
    scene_id = character(0),
    scene_name = character(0),
    version_id = integer(0),
    version_number = integer(0)
  )

  if (is.null(scenes) || length(scenes) == 0) {
    return(list(
      versions = empty_versions,
      lookup = stats::setNames(character(0), character(0))
    ))
  }

  ver_rows <- list()

  for (scene in scenes) {
    sid <- as.character(scene$id %||% NA_character_)
    sname <- as.character(scene$sceneName %||% NA_character_)

    for (ver in (scene$versions %||% list())) {
      ver_rows <- c(ver_rows, list(list(
        scene_id = sid,
        scene_name = sname,
        version_id = as.integer(ver$id),
        version_number = as.integer(ver$versionNumber %||% NA_integer_)
      )))
    }
  }

  if (length(ver_rows) == 0) {
    return(list(
      versions = empty_versions,
      lookup = stats::setNames(character(0), character(0))
    ))
  }

  versions <- tibble::as_tibble(do.call(rbind, lapply(ver_rows, as.data.frame,
                                                       stringsAsFactors = FALSE)))
  versions$version_id <- as.integer(versions$version_id)
  versions$version_number <- as.integer(versions$version_number)

  # Build lookup: version_id (as character) -> scene_name
  valid <- !is.na(versions$version_id)
  lookup <- stats::setNames(
    versions$scene_name[valid],
    as.character(versions$version_id[valid])
  )

  list(versions = versions, lookup = lookup)
}
