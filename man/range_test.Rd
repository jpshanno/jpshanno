% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processing_functions.R
\name{range_test}
\alias{range_test}
\title{Perform a range test with data-derived or specified min/max}
\usage{
range_test(data, observations, lower = NULL, upper = NULL,
  backfill = TRUE, alpha = 0.01, ...)
}
\arguments{
\item{observations}{The unquoted column name of the data you want to test}

\item{lower}{The lower bound of acceptable data}

\item{upper}{The lower bound of acceptable data}

\item{backfill}{A logical indicating if \code{\link[zoo]{na.approx}} should
be used}

\item{...}{Arguments passed on to \code{\link[zoo]{na.approx}}}
}
\description{
If lower and upper are \code{NULL}, the minimum and maximum global values
will be used
}
\details{
Global lower and upper are calculated if not supplied. There could be a
window argument here to do annual or monthly range tests
}
\seealso{
Other QAQC functions: \code{\link{offset_solinst_time}},
  \code{\link{spike_test}}
}
\concept{QAQC functions}
