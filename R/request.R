##' Load cached objects from /cache directory of project tree
##'
##' To save time and typing, this function will load rda files from /cache
##' directory of project tree by name.
##' @title Load cached objects from /cache directory of project tree
##' @param str Name of object saved rda file
##' @return nothing, invoked for side effects
##' @examples
##' \dontrun{thaw("someobject")}
thaw <- function(str) {
  p <- paste(file.path(getwd(), "cache", str), ".rda", sep = "")
  if (file.exists(p)) {
    load(p, envir = .GlobalEnv)
    message(paste("Loaded object", str, "from cache."))
  } else {
    warning("Cached file does not exist.")
  }
}

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
  } else {
    cat(sprintf("Object %s from workspace.\n", .name))
  }
}

##' Request objects by pattern from session or cache
##'
##' This functions works like \link{\code{request}}, but takes a
##'   regular expression to match against objects in workspace and
##'   cache.
##' @param .pattern a regular expression as string
##' @return Nothing, invoked for side effects (requesting objects)
##' @export
request_all <- function(.pattern) {
  objects <- ls()
  in_workspace <- grep(.pattern, objects)
  if (dir.exists("cache")) {
    cached <- list.files("cache", pattern = "\\.rda$")
    cached <- sub(".rda", "", cached)
    ## exclude sha1 sums
    cached <- cached[grep("_expression_sha1$", cached, invert = TRUE)]
    in_cached <- grep(.pattern, cached)
  } else {
    in_cached <- numeric(0)
  }
  match_in_workspace <- length(in_workspace) > 0
  match_in_cache <- length(in_cached) > 0
  if (!match_in_workspace & !match_in_cache) {
    stop("Pattern not matched in workspace or cache.")
  }
  if (match_in_workspace) {
    request_workspace <- objects[in_workspace]
  } else {
    request_workspace <- NULL
  }
  if (match_in_cache) {
    request_cache <- cached[in_cached]
  } else {
    request_cache <- NULL
  }
  request_objects <- c(request_workspace, request_cache)
  request_objects <- request_objects[!duplicated(request_objects)]
  invisible(lapply(request_objects, request))
}
