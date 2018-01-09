#' Empty cache
#'
#' This function removes all objects in the cache, w.r.t. to the current working
#' directory.
#' @return Nothing, invoked for side effects.
#' @export
#'
#' @examples
#' # empty_cache()
empty_cache <- function() {
  lapply(list.files("cache", pattern = "\\.rda$", full.names = TRUE), file.remove)
}