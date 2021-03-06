% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ChangbaiLiToolsRfunctions.R
\name{myapply}
\alias{myapply}
\title{My Apply Function}
\usage{
myapply(X, MARGIN, FUN, ...)
}
\arguments{
\item{X}{a matrix}

\item{MARGIN}{either (the number) 1 or (the number) 2}

\item{FUN}{an R function that maps vectors to vectors}
}
\value{
array or matrix
}
\description{
Myapply function just like the function array
function(X, MARGIN, FUN, ...)
}
\examples{
m <- matrix(1:6, ncol = 2)
s = array(rnorm(72),c(3,3,8))
result10 <- myapply(m,1,"mean")

}
