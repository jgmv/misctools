% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fungalTraits_annotation.R
\name{fungalTraits_annotation}
\alias{fungalTraits_annotation}
\title{Assigns Fungal Guilds using FungalTraits database}
\usage{
fungalTraits_annotation(
  x,
  print_missing = F,
  amf = T,
  coltaxa = c("genus", "phylum")
)
}
\arguments{
\item{x}{a data frame with taxonomy data.}

\item{print_missing}{whether taxa not found in the database should be displayed.}

\item{amf}{whether Glomeromycota are assigned to arbuscular mycorrhizas at the phylum level.}

\item{coltaxa}{names of columns with genus and phylum, respectively.}
}
\value{
data frame
}
\description{
Assigns fungal guilds using the FungalTraits database (https://doi.org/10.1007/s13225-020-00466-2) by comparing genus names. In the case of Glomeromycota, assignment to arbuscular mycorrhizal lifestyles can be optionally done at the phylum level.
}
\examples{
fungalTraits_annotation(example_taxonomy)
}
\keyword{fungal}
\keyword{guild}
