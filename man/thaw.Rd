\name{thaw}
\alias{thaw}
\title{Load cached objects from /cache directory of project tree}
\usage{
thaw(str)
}
\arguments{
  \item{str}{Name of object saved RData file}
}
\value{
nothing, invoked for side effects
}
\description{
Load cached objects from /cache directory of project tree
}
\details{
To save time and typing, this function will load RData
files from /cache directory of project tree by name.
}
\examples{
\dontrun{thaw("someobject")}
}
\author{
Christian Zang
}

