\name{plotpca}
\alias{plotpca}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting a Principal Component Analysis
}
\description{
Plots the Principal Component Analysis from the pca function, and custumizes the output with 
}
\usage{
plotpca(x,map="b",n="a",l=0.05,col=NULL,calc="Chi",epd=NULL,et="bar",dif="h",dax="all")
}
\arguments{
  \item{x}{
results of \link{pca} 
}
\item{map}{if "bil", biplot is plotted with supplementary individuals projected, and labels of supplementary individuals are written
if "bip", biplot is plotted with supplementary individuals projected, and points of supplementary individuals are plotted
if "b", biplot is plotted 
if "til", two-way pca is plotted with supplementary individuals projected, and labels of supplementary individuals are written
if "tip", two-way pca is plotted with supplementary individuals projected, and la points of supplementary individuals are plotted
if "t", two-way pca is plotted
}
  \item{n}{
number of axes to plot. By default, "a", which calculates an automatic theoretical number of dimensions (when the contribution of loadings is higher than 1)
}

  \item{l}{
by default 0.05. Risk to be taken in the agreement ellipses
}
  \item{et}{
"bar" for the confidence ellipses around the product means "ind" for the representation of a proportion of 1-alpha of the panelist scores
}
  \item{col}{
select the colors of the different individuals
}

  \item{calc}{
"Chi", "F" or "Sas". Indicates the type of ellipse to be calculated.
}
  \item{epd}{
when a biplot is chosen, the arrows can be oversized.
}
  \item{dif}{
if "n", no tests are run
if "h", hotelling  test is run to detect differences between zones.
if "hc", the p-value is corrected with the bonferroni Correction (l is divided by k(k+1)/ where k is the number of zones)
if "s", srivastava  test is run to detect differences between zones.
if "sc", the p-value is corrected with the bonferroni Correction (l is divided by k(k+1)/ where k is the number of zones)

When a test is run, zones not significantly difference are related by a segment}
 
  \item{dax}{
if "all", the test is run on all the Principal Components. If "sig", it is done only on the significant Principal Components. dax can also be a vector determining the index of Principal Component to be chosen for the test (ex dax=c(1,2)).
}
}

\examples{data(df.scaled)
resPCA=pca(data=df.scaled[,-1], zone="zone",pixel="pixel")
couleurs=find.colors(rownames(resPCA$IndivCoord)) 
\donttest{plotpca(x=resPCA)}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

