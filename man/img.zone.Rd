\name{img.zone}
\alias{img.zone}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts a zone object (point list) into a matrix 
}
\description{
Converts a zone object (point list) into a matrix : each pixel is attributed to a zone. This matrix can also be plotted with \link{plotimage}. This matrix is to be related to the other image matrices, for example in the \link{reshapimg} function, to be used in \link{pca}.
}
\usage{
img.zone(zone,d=NULL,mask=NULL,edge.only=NULL,wo.edge=NULL,graph=FALSE,img=NULL,lim=30)
}
\arguments{
  \item{zone}{
A zone created with \link{create.zones}
}
  \item{d}{
dimension of the matrix to create
}
  \item{mask}{
boolean matrice can be specified here
}
  \item{edge.only}{
if TRUE, only the edges of the images are kept. They are defined with "limit" for their width. This requires that the parameter img is fulfilled
}
  \item{wo.edge}{
if TRUE, the edges of the images are removed. They are defined with "limit" for their width.This requires that the parameter img is fulfilled
}
 \item{img}{
image on which the edges are selected}
 \item{graph}{
if TRUE, the selected edges are plotted
}
 \item{lim}{
sensitivity of the scale detection
}
}

\value{
Zone object (with control points) is given as a matrix /image
}
\seealso{
\link{create.zones}, \link{plotzone},\link{reshapimg},\link{erase},\link{plotimage}
}
\examples{

data(zones)
data(count)
zone.imgRect=img.zone(zone=zones,d=dim(count))
\donttest{plotimage(mat=zone.imgRect,z=T)}

}
