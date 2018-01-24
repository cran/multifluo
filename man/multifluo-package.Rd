\name{multifluo-package}
\alias{multifluo-package}
\alias{multifluo}
\docType{package}
\title{
\packageTitle{multifluo}
}
\description{
\packageDescription{multifluo}
}

\author{
\packageAuthor{multifluo}

Maintainer: \packageMaintainer{multifluo}
}

\keyword{ package }
\seealso{\link{create.zones},\link{reshapimg},\link{plotimage}

}
\examples{
data(count)
count.img=plotimage(mat=count,lim=c(0,5000))
data(df.scaled)
resPCA=pca(data=df.scaled[,-1], zone="zone",pixel="pixel")
couleurs=find.colors(rownames(resPCA$IndivCoord)) 
\donttest{plotpca(resPCA)}

}
