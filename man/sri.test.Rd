\name{sri.test}
\alias{sri.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Srivastava's Test
}
\description{
Tests multivariate differences between two samples with Srivastava's Test
}
\usage{
sri.test(matx, maty)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{matx}{
First  sample (with observations as lines and variables as columns)
}
  \item{maty}{
Second  sample (with observations as lines and variables as columns). matx and maty have to have the same number of variables.
}
}

\value{
a list containing the p-value and the statistique (corresponding to T1 in the Srivastava's paper)
}
\references{
M. S. Srivastava and M. Du.  A test for the mean vector with fewer observations than the dimension.Journal of Multivariate Analysis , 99(3):386 402, Mar. 2008.
}



%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{difftable},\link{hotelling.test}
}
\examples{
A=matrix(rnorm(13.15),13,15)
B=matrix(rnorm(13.15),13,15)
sri.test(A,B)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
