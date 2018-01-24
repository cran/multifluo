\name{plotzone}
\alias{plotzone}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting the different selected zones on an image
}
\description{plots the different selected zones on an image}
\usage{
plotzone(zone,name = FALSE,dim.img=NULL,d=200,cex=1,col="black",main="Zone",lwd=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{zone}{
zone stemming from \link{create.zones}
}
  \item{name}{
if TRUE, the zone names are written above the zones
}
  \item{dim.img}{
dimensions of the image were the zones are drawn
}
  \item{d}{
the distance between the zone and the text is given in a portion of the image (divided by 200 by default)
}
  \item{cex}{
size of the text (by.default 1)
}
  \item{col}{
color of the zones and the text
}
  \item{main}{
main Title
}

 \item{lwd}{
width of the lines
}
}




\seealso{
\link{create.zones}; \link{img.zone}
}
\examples{
data(zones)
plotzone(zones)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
