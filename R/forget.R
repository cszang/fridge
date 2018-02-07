#' Forget cached object
#'
#' @param name unquote expression naming object to remove
#'
#' @return nothing, invoked for side effects
#' @export
forget <- function(name) {
  arg <- as.name(deparse(substitute(name)))
  cache_dir <- paste0(file.path(getwd(), "cache"), "/")
  cache_file <- paste0(cache_dir, as.character(arg), ".rda")
  sha1_file <- paste0(cache_dir, as.character(arg), "_expression_sha1.rda")
  file.remove(cache_file)
  file.remove(sha1_file)
  eval(substitute(rm(name, envir = .GlobalEnv), list(name = arg)))
}