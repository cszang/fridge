##' Assign read in data with creating/checking SHA1 sum
##'
##' This infix function creates a normal assignment as side effect,
##' but also checks the read in data against a previously cached SHA1
##' sum (if available), or creates the SHA1 sum in the first
##' place. When SHA1 sums differ, a warning will be issued.
##'
##' @usage x \%<c-\% value
##' @param x unquoted expression naming variable to create
##' @param value unquoted expression to evaluate the first time \code{name} is
##'   accessed
##' @export
##' @importFrom digest digest
##' @rdname assign-check-sha1
##' @examples
##' \dontrun{
##' x %<c-% read.csv("some_file.csv")
##' }
"%<c-%" <- function(x, value) {
  name <- substitute(x)
  call <- substitute(value)
  if (!is.name(name)) stop("Left-hand side must be a name")
  if (!is.call(call)) stop("Right-hand side must be a function call")

  read_fun <- call[[1]]
  fun_name <- deparse(read_fun)
  allowed_funs <- c("read.csv", "read.csv2", "read.rwl", "read.crn",
                   "nc_open", "read_delim", "read_csv", "read_csv2",
                   "read.table", "read.delim", "read_table", "fread")
  if (!any(fun_name %in% allowed_funs)) {
    stop_msg <- "Function call must by any of"
    stop_msg <- paste0(stop_msg, " ", paste(allowed_funs,
                                           collapse = ", "), ".")
    stop(stop_msg)
  }

  x <- eval(value)
  sha1_x_current <- digest(x, algo = "sha1")

  sha1_dir <-  file.path(getwd(), "checksums")
  sha1_file <- file.path(sha1_dir, (name))
  if (file.exists(sha1_file)) {
    sha1_x_stored <- readLines(sha1_file)[1]
    if (sha1_x_stored != sha1_x_current) {
      sha1_archive_file <- paste0(sha1_file, ".archive")
      cat(sha1_x_stored, file = sha1_archive_file)
      cat("\n", file = sha1_archive_file, append = TRUE)
      warning("Checksum does not match with version of file read in previously. The previous checksum has been archived.")
    }
  } else {
    if (!file.exists(sha1_dir)) {
      dir.create(sha1_dir)
    }
  }
  cat(sha1_x_current, file = sha1_file)
  cat("\n", file = sha1_file, append = TRUE)    

  assign(deparse(name), x, pos = parent.frame())
  invisible()
}
