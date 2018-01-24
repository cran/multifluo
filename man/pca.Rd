\name{pca}
\alias{pca}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
pca
}
\description{
Calculates Principal Component Analysis with agreement ellipses
}
\usage{
pca(data, zone = "zone", pixel = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
should contain a column named as zone, and another named pixel
}
  \item{zone}{
name of the column containing the zone
}
  \item{pixel}{
name of the column containing the pixel name (required when ellipses, individual projections or tests are asked in the PCA)
}
}

\value{
A list containing
 \item{B }{matrix of zone covariance}
  \item{IndSup}{supplementary individuals}
   \item{EingenVectors }{eigen vectors obtained by the PCA}
  \item{EingenValues }{eigen values obtained by the PCA}
   \item{IndivCoord }{coordinates of the individuals (here, zone means)}
  \item{VarCoord }{coordinates of the variables }
   \item{NbdimSig }{number of significant dimensions}
}
\references{
Peltier, C., Visalli, M. and Schlich, P. (2015), Canonical Variate Analysis of Sensory Profiling Data. J Sens Stud, 30: 316 328. doi:10.1111/joss.12160
}



%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{plotpca} \link{reshapimg}
}
\examples{
data(df.scaled)
resPCA=pca(data=df.scaled[,-1], zone="zone",pixel="pixel")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
