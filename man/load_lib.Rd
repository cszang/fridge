\name{load_lib}
\alias{load_lib}
\title{Load R files from /lib directory of project tree}
\usage{
load_lib(str = "all", ...)
}
\arguments{
  \item{str}{a character vector specifying the lib files to
  source, defaults to "all", i.e., all R-files inside /lib
  will be sourced}
}
\value{
nothing, invoked for side effects (sourcing of files)
}
\description{
Load R files from /lib directory of project tree
}
\details{
To save time and typing, this function will load R files
from /lib directory of project tree, either by name (such
as \code{c("lib1", "lib2")}) or all at once (default).
}
\examples{
\dontrun{load_lib()}
}
\author{
Christian Zang
}

