% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_species_range.R
\name{find_species_range}
\alias{find_species_range}
\title{Find species distribution range}
\usage{
find_species_range(x, lon = "longitude", lat = "latitude")
}
\arguments{
\item{x}{data frame with the coordinates of the species occurrence.}

\item{lon}{name of data frame column with the longitude data.}

\item{lat}{of data frame column with the latitude data.}
}
\description{
Calculates the range of a species, in Km.
}
\examples{
x <- data.frame(longitude = runif(10, min = -180, max = 180),
  latitude = runif(10, min = -90, max = 90))
find_species_range(x)
}
\keyword{distance}
