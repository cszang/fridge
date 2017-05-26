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
##' @param create shall a cache file be created (default TRUE)
##' @return nothing, invoked for side effects (assigning a value to a variable)
##' @author Christian Zang
##' @examples
##' \dontrun{freeze("a_big_sum", sum(1:1020))}
##' @export
freeze <- function(object, expression, create = TRUE) {

  wd <- getwd()
  cache_dir <- file.path(wd, "cache")
  cache_file <- paste(file.path(cache_dir, object), ".rda", sep = "")

  if (file.exists(cache_file)) {
    is_cached <- TRUE
  } else {
    is_cached <- FALSE
  }

  if (is_cached) {
    
    load(cache_file, envir = .GlobalEnv)
    message("Loaded variable from cache.")
    
  } else {
    e_assign <- substitute(assign(object, expression, envir = .GlobalEnv),
                           list(
                             object = object,
                             expression = expression
                             )
                           )

    eval(e_assign)

    if (create) {

      e_create <- substitute(save(object, file = cache_file),
                             list(object = object)) 
      eval(e_create)
      
    }
  }
}

##' @rdname freeze
##' @export
assignifnotcached <- freeze

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
