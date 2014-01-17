##' Load packages
##'
##' Load a number of packages without needing a new line of
##' "library(...)" for each one.
##' @param packages a character vector holding the names of packages
##' to load
##' @return nothing, invoked for side effects (package loading)
##' @export
load_packages <- function(packages) {
  n <- length(packages)
  for (i in 1:n) {
    eval(
      substitute(
        library(pck),
        list(pck = packages[i])
        )
      )
    cat("fridge loaded package", packages[i], "\n")
  }
}
