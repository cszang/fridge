##' Assign value to variable if the value is not cached
##'
##' For saving time, objects can be cached under their object name
##' with the ".rda" extension in a cache directory "/cache" under
##' the current working directory. If a cached version of the variable
##' exists, do not recalculate the assigned part of the expression,
##' but rather use the cache. This differs from memoization as a
##' permanently cached version is used.
##' @title Assign value to variable if it is not cached
##' @param object a string given the name of the variable to be
##' assigned to
##' @param expression the expression evaluating into the value to be assigned
##' @return nothing, invoked for side effects (assigning a value to a variable)
##' @importFrom digest digest
##' @examples
##' \dontrun{freeze("a_big_sum", sum(1:1020))}
##' @export
freeze <- function(object, expression) {

  wd <- getwd()
  cache_dir <- file.path(wd, "cache")
  if (!file.exists(cache_dir)) dir.create(cache_dir)
  
  cache_file <- paste(file.path(cache_dir, object), ".rda", sep = "")
  sha1_file <- paste(file.path(cache_dir, object), "_expression_sha1.rda", sep = "")
  is_cached <- file.exists(cache_file)
  expression_sha1 <- digest(expression, algo = "sha1")

  if (is_cached) {
    message("Reloading object from cache...")
    load(cache_file, envir = .GlobalEnv)
    if (file.exists(sha1_file)) {
      load(sha1_file, envir = sys.frame())
    } else {
      cached_sha1 <- NA
    }
    if (expression_sha1 == cached_sha1) {
      message("Done.")
    } else {
      message("Done.")
      warning("SHA1 of call differs from cached version.")
    }
  } else {
    eval(substitute(assign(object, expression, envir = .GlobalEnv),
                    list(
                      object = object,
                      expression = expression
                    )))
    eval(substitute(save(object, file = cache_file),
                    list(object = object)))
    cached_sha1 <- expression_sha1
    save(cached_sha1, file = sha1_file)
    message("Cached object.")
  }
}

##' Infix variant of \code{freeze}
##'
##' Does the same as \code{freeze}, but as infix operator.
##'
##' @param x unquoted expression naming variable to create
##' @param value unquoted expression to evaluate the first time
##'   \code{name} is accessed
##'
##' @export
##' @rdname assign-freeze
"%<f-%" <- function(x, value) {
    name <- substitute(x)
    value <- substitute(value)
    
    if (!is.name(name)) stop("Left-hand side must be a name")

    freeze(deparse(name), value)
    invisible()
}

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

