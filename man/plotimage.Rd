\name{plotimage}
\alias{plotimage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting an image
}
\description{
Plots an image with its colorscale.
}
\usage{
plotimage(mat,lim=NULL,nc=1000,m=NULL,z=FALSE,add=FALSE,cols="rainbow",lc=c(0,0.7),p="l")}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{mat}{
matrix representing the image. Thus, a numerous matrix with 256 rows and 256 columns will represent an image with 256*256 pixels. The colour of each pixel will be determined thanks to the value of the related pixel in the matrix, and the values of the limit scales.
}
  \item{lim}{minimum and maximum value for the color scale. Ex: lim=c(0,10)
}
 
  \item{nc}{
number of colors
} 
  \item{m}{
boolean matrix with the same size than mat, selecting pixels which will plotted (TRUE) or not (FALSE) (mask)
}
  \item{z}{
TRUE when mat is a zone matrix stemming from \link{img.zone}
}

  \item{add}{
when TRUE, the graph is drawn over the existing graph. If FALSE, a new graph is plotted.
}

  \item{cols}{
"rainbow" by default, but other scales are also available : ""gray.scale","cm.colors","topo.colors","terraincors","heat.colors". These scales correspond to the usual colors scales in R.
}

\item{lc}{c(0,0.7) by default. Parameter corresponding to the start color and the end color in the rainbow scale,a number in [0,1] (start in rainbow function)}
\item{p}{Further parameters to plot. if "n", nothing more is plotted, if "a", axes are plotted, if "l", color legend is plotted, if "al" axes and color legend are plotted. If "d" is added to this parameter (giving "ad","ld","ald" or "nd", no smoothing of the pixel is computed (discrete)
}
}

\value{
Returns the created image as a cimg (type cimg from the imager package)
}


\examples{
data(lifetime)
data(count)
data(gp) 
count.img=plotimage(mat=count,lim=c(0,5000))

# with m
count.img=plotimage(mat=count,lim=c(1000,4000),m=count>1000)
\donttest{plotimage(mat=lifetime,lim=c(2.8,3.5))
gp.img=plotimage(mat=gp,lim=c(-0.2,0.8))}

#add
\donttest{
resT=plotimage(mat=lifetime,lim=c(2.8,3.5),m=count>1000)
resGP=plotimage(mat=gp,lim=c(2.8,3.5),add=TRUE)
}


}
