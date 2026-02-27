# Internal: fetch dynamic objects for a project and return a sdkId -> name lookup
#
# @param project_id Integer. Project ID.
# @return Named character vector where names are sdkId values and values are
#   friendly object names. Returns an empty named character vector if no objects
#   exist.
# @keywords internal
fetch_objects_lookup <- function(project_id) {
  endpoint <- paste0("/v0/projects/", project_id, "/objects")
  raw <- c3d_get(endpoint)

  if (length(raw) == 0) {
    return(stats::setNames(character(0), character(0)))
  }

  sdk_ids <- vapply(raw, function(obj) {
    as.character(obj$sdkId %||% NA_character_)
  }, character(1))

  names_vec <- vapply(raw, function(obj) {
    as.character(obj$name %||% NA_character_)
  }, character(1))

  # Drop entries with missing sdkId
  valid <- !is.na(sdk_ids)
  stats::setNames(names_vec[valid], sdk_ids[valid])
}
