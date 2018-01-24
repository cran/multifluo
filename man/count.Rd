\name{count}
\alias{count}
\docType{data}
\title{
count
}
\description{
Spermatozoa bulls were labelled with a fluorescence probe (Laurdan) and then observed with a microscop and its related software Symphotime. 
Three data were recorded per pixels: the counts of photons (indicating the number of photons perceived on this pixel after simulation),the lifetime of fluorescence (indicating the time of first perceived photon after stimulation)
and the  generalized polarization (indicating a ratio between a green-light  simulation and a  blue-light simulation)
These matrices have as many rows as observed pixels in lines, and as many columns as observed pixels in columns.
This specific data represents the count of photons per pixel. 
}
\usage{data("count")}
\format{
  A data frame with 203 lines on the following 204 columns.
}
\examples{
data(count)
}
\seealso{\link{gp},\link{lifetime}}
\keyword{datasets}
