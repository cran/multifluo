\name{erase}
\alias{erase}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Eraser
}
\description{
Suppresses a zone (determined on an image) from a given selection stemming from a zone image
}
\usage{
erase(zone.img, img, interact = TRUE, shape = "rect", pts = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{zone.img}{
the zone image to be modified
}
  \item{img}{
the image on which the zone to suppress is selected
}
  \item{interact}{
if TRUE, the zone is determined interactively. Else pts is used as a matrix of control points (see \link{create.zones})
}
  \item{shape}{
"rect", "para" or "circle"
}
  \item{pts}{
Matrix containing the control points (see \link{create.zones})
}
}


\value{
returned a zone image without the selected zone
}
\seealso{\link{img.zone},\link{plotimage}}
\examples{
\donttest{
data(zone.img)
plotimage(zone.img,is.zone=TRUE)
data(img)
zone.img2=erase(zone.img,img,interact=TRUE,shape="rect")
plotimage(zone.img2,is.zone=TRUE)
}

}

