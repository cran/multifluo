\name{pcapic}
\alias{pcapic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pictures from the first component of PCA
}
\description{
This function returns a picture where each pixel value corresponds to its coordinate on the first component of PCA (not scaled) 
}
\usage{
pcapic(dfbool, d = c(203, 204), lim = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfbool}{
result from \link{reshapimg}, with return.bool=TRUE
}
  \item{d}{
d is the final size of the image. It has to correspond to the initial images taken to run the \link{reshapimg}
}
  \item{lim}{
limit of the colorscale 
}
}
\value{Returns the resulted matrix with d dimensions, and plots the image with pca. 
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{reshapimg}
}
\examples{
data(dfboolTest)
pcapic(dfbool=dfboolTest,lim=c(-1,1))
}
