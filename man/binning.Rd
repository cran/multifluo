\name{binning}
\alias{binning}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bins the pixels
}
\description{
Reduces the size of an image by binning the pixels.
}
\usage{
binning(dataset, n.bin, fun = "mean")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{
The dataset to be binned (in a matrix form)
}
  \item{n.bin}{
number of pixels to be binned (a natural number)
}

  \item{fun}{
if "mean", the value of the binned pixel is the average of the values of the initial pixels 
if "sum", the value of the binned pixel is the sum of the values of the initial pixels 
}
}
\value{
The binned dataset
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
data(gp)
binning(gp,2)
}
