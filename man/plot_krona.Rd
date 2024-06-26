% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_krona.R
\name{plot_krona}
\alias{plot_krona}
\title{Plot Krona chart}
\usage{
plot_krona(tax, cdm = NULL, group = NULL, outfile = "krona_plot.html")
}
\arguments{
\item{tax}{data frame with taxonomy data}

\item{cdm}{community data matrix, or vector with total taxon abundance}

\item{group}{a groupping vector for samples}

\item{outfile}{name of the output file}
}
\value{
An html file with a Krona chart.
}
\description{
Plots a Krona chart from a taxonomy data set, with the option of including abundance data.
Requires a local install, with system-wide access, of KronaTools (https://github.com/marbl/Krona/tree/master/KronaTools).
}
\keyword{taxonomy}
