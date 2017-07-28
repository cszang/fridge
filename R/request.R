#' Request object from session or cache
#' 
#' This function is a wrapper around \link{\code{thaw}}, that checks if an
#' object is currently defined in the workspace. If not, it tries to load the
#' object from the project's cache.
#' @param .name String giving the name of the requested object
#'   
#' @return Nothing, invoked for side effects (requesting an object)
#' @export
request <- function(.name) {
  if (!exists(.name)) {
    # look in cache
    if (dir.exists("cache")) {
      cached <- list.files("cache", pattern = "\\.rda$")
      cached <- sub(".rda", "", cached)
      if (any(cached == .name)) {
        thaw(.name)
      } else {
        stop("Object is neither defined nor cached.")
      }
    }
  }
}