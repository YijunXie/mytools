% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pwls.R
\name{pwls}
\alias{pwls}
\title{Piecewise linear scroing function}
\usage{
pwls(x, t, q)
}
\arguments{
\item{x}{tested value}

\item{t}{true value}

\item{q}{quantile level}
}
\value{
numerical value of the score
}
\description{
Piecewise linear scroing function
}
\examples{
#' pwls(0.1,0.05,0.95)
#' pwls(0.2,0.2,0.9)
}

