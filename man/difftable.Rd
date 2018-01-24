\name{difftable}
\alias{difftable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Table of multivariate differences
}
\description{
Built a table crossing modalities of a trestmanet variable about multivariate differences
}
\usage{
difftable(mat,vep=NULL,axes=c(1,2),var.col=NULL,trt="zone",test="hotelling")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{
the matrix must have one column named as trt, and 
}
  \item{vep}{
when vep is not NULL, a preliminary operation multiplies mat [,var.col] by the vep matrix (useful to do the test only on the first principal components of mat).
}
  \item{axes}{
vep can contain all eigenvectors. If it is the case, axes allows the user to select only some eigenvectors (1:2 for the first two eigenvectors, 1:5 for the first five eigenvectors...)
}
  \item{var.col}{
indicates the number of columns of the numerical variables. If NULL, all columns but trt are selected.
}
  \item{trt}{
name of the column
}
  \item{test}{
"hotelling" or "sri". Indicates the type of test to detect multivariate differences. "hotelling" is the usual hotelling T2 test whereas "sri" corresponds to the Srivastava's Test, allowing a test of differences even if there is more variables than observations.
}
}

\value{A table containing the p-values is returned.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{hotelling.test}, \link{sri.test}
}
\examples{
data(df.scaled)
\donttest{difftable(df.scaled[,-5],trt="zone")
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
