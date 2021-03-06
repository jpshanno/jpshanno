% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regional_mapping_project.R
\name{classify_plot}
\alias{classify_plot}
\title{Classify a CFI plot using a specified classifier}
\usage{
classify_plot(data, ..., classifier = get_dominant_species,
  point.threshold = 0.2, plot.threshold = 0.2, idw.power = 3,
  use.parallel = FALSE, workers = 2)
}
\arguments{
\item{data}{Dataframe or tibble containing plot data with columns plot_id and
species}

\item{...}{The measures used to classify plots as unquoted column names}

\item{classifier}{The classifier to use, see below for options}

\item{point.threshold}{A threshold specifying what proportion of the relative
measure at given point a species has represent to be considered present.
Passed on to calculate_relative_matrix}

\item{plot.threshold}{A threshold specifying what proportion of a plot a
species must be present in to be considered dominant or present}

\item{idw.power}{The power used for IDW}

\item{use.parallel}{Logical indicating if parallel processing should be used
(requires the furrr package)}

\item{workers}{The number of works to use if `use.parallel=TRUE`}
}
\value{
A tibble containing the columns 'plot_id' and 'species' and the column(s)
of the results of the classifier
}
\description{
Classify a CFI plot using a specified classifier
}
\seealso{
Other plot classifiers: \code{\link{get_dominant_species}},
  \code{\link{get_species_presence}},
  \code{\link{get_species_proportion}}
}
\concept{plot classifiers}
