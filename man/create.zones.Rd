\name{create.zones}
\alias{create.zones}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creating a list of consecutive zones of images
}
\description{
Creates a list of consecutive zones of images, interactively (by pointing directely control points on an image) or manually (by entering coordinates of control points). The control points can describe either a rectangle (two diagonal points), a parallelogram(three consecutive points representing an angle of the parallelogram) or a circle (center of the circle, then a point of the circle).
}
\usage{
create.zones(img=NULL,shape="rect",n.zones=NULL,interact=TRUE,pts=NULL,name.zones=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{img}{
img is an image (cimg type) from imager stemming from "plot.image"
}
  \item{shape}{
When "rect", the shape would be a rectangle: the selection tool is a rectangle by clicking and maintaining the mouse on the screen. 
When "para", the shape would be a parallelogram: the selection tool is the consecutive input of three points of one angle of the parallelogram. 
When "circle", the selection tool is the consecutive input of the center of the circle and one point on the circle.
When "various" and interactive, the user is asked for the shape on the next selection.
}
  \item{n.zones}{
number of zones to be input
}
\item{interact}{if TRUE, the interactive tool is launched to determine the control points of the different zones. If FALSE, these control points are defined in pts}
\item{pts}{matrix of points corresponding to the selected zones}
\item{name.zones}{if not NULL, corresponds to the names of the zones to select.}

}

\value{
A list containing as many component as the number of zones. Each component is also a list containing : 
the matrix of control points (pts),the name of the selected zone (name) and the name of the selected shape (shape)
If "rect is chosen", pts contains the two points of the diagonal of the rectangle, 
If "para" is chosen, pts to the three points of the selected angle of the parallelogram
If "circle" is chosen, pts contains the center of the circle and one point in the circle
}

\seealso{ \link{plotzone}, \link{plotimage}
}
\examples{
data(zone.mat)
zones=create.zones(pts=zone.mat,shape="para",interact=FALSE)
data(count)
\donttest{
img=plotimage(count,main="Counts",min.scale=1000,max.scale=4000,mask=count>1000)
zonesRect=create.zones(img,shape="rect",n.zones=3)
zonesPara=create.zones(img,shape="para",n.zones=3)
zonesCircle=create.zones(img,shape="circle",n.zones=3)
zonesVarious=create.zones(img,shape="various",n.zones=3)
}

}
