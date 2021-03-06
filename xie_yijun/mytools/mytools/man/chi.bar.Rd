% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chibar.R
\name{chi.bar}
\alias{chi.bar}
\title{function for calculating chibar}
\usage{
chi.bar(dat, q = 0.95, n = 1, conf = 0.95)
}
\arguments{
\item{dat}{numerical matrix}

\item{q}{quantile level}

\item{n}{discarded number}

\item{conf}{confidence level}
}
\value{
vector
}
\description{
Find the tail index chibar
}
\examples{
#' chi.bar(rnorm(1000),q = 0.9,n = 1)
}

