\name{translation}
\alias{translation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Translation of the data
}
\description{
Sometimes, a slight shift exists between two images of the same sample. This function allows them to be superimposed.
The shift can be determined with a vector by entering its coordinates; or interactively by clicking on a specific point on a first image, and its corresponding point on the second image)
}
\usage{
translation(dtt,dtc=NULL,x,y,interact="none",dtt.lim=NULL,dtc.lim=NULL,n.around=c(5,5))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dtt}{
 image matrix (for 512*512 image, the dimensions of this matrix are 512*512) to translate
}
  \item{dtc}{
potential image matrix (for 512*512 image, the dimensions of this matrix are 512*512) on which the dtt is superimposed
}
  \item{x}{
If interactive is FALSE, abscisse of vector of translation
}
  \item{y}{
If interactive is FALSE, ordinate of vector of translation

}

  \item{interact}{
if TRUE, dtt is displayed as an image, and the user has to select one point on it. Then, dtc is displayed as an image and the user has to select the related same point on it.
}
  \item{dtt.lim}{
limits of the scale of dtt (to change when the image is not contrasted enough to detect specific points)
}
  \item{dtc.lim}{
limits of the scale of dtc (to change when the image is not contrasted enough to detect specific points)
}
  \item{n.around}{
Tests every translation in a neighborhood of n.around[1] in x and n.around[2] of y (clicked by the user), and select the translation that maximizes the number of not TRUE common to dtc and dtt.
It requires that both dtc and dtt are boolean matrices.}
}
\value{
The function returns a list containing the translated data ("data") and the vector of translation ("translation")
}




\examples{
data(gp)
data(count)
\donttest{GP37Anew=translation(dtt=gp,dtc=count,interact=TRUE,dtt.lim=c(-0.4,0.8),dtc.lim=c(1000,4000))}
GP37Anew=translation(gp,x=7,y=-3)$data
}
