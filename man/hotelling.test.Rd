\name{hotelling.test}
\alias{hotelling.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hotelling's T2 Test
}
\description{
Tests multivariate differences between two samples with Hotelling's T2 Test
}
\usage{
hotelling.test(matx, maty)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{matx}{
First  sample (matrix with observations as lines and variables as columns)

}
  \item{maty}{
Second  sample (matrix with observations as lines and variables as columns)
}
}

\value{
Returns a list containing the stat and the p-value
}
\references{
Hotelling, H. (1931). "The generalization of Student's ratio." Annals of Mathematical Statistics 2. (3): 360 378
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{difftable},\link{sri.test}
}
\examples{
A=matrix(rnorm(13.15),13,15)
B=matrix(rnorm(13.15),13,15)
hotelling.test(A,B)
}

