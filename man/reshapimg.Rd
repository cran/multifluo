\name{reshapimg}
\alias{reshapimg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reshape image matrices as one sigle dataframe (useful to pca, for example)
}
\description{
Reshapes a list of (numeric) image matrices, and a zone matrix related to one set of images, in order to obtain a dataframe, with each line corresponding to one pixel and each matrix corresponding to one single column.
}
\usage{
reshapimg(zone.img,list.img,list.bool=NULL,reduction=TRUE,return.bool=FALSE,name.img=NULL)
}

\arguments{
  \item{zone.img}{
result of \link{img.zone}
}
  \item{list.img}{
list of images to be compared
}
  \item{list.bool}{
potential list of boolean matrices reflecting conditions of the analysis. Each pixel which does not respect at least one of the boolean conditions will be removed from the resulting dataset.
}
  \item{reduction}{
if TRUE, each column of the related datafraime (corresponding to each  numeric image matrix) is centered and reduced,else, it is only centered
}
  \item{return.bool}{
if TRUE, a list containing the resulting data (data) and the boolean matrix of the treated pixels (boolean) is returned
}
  \item{name.img}{
vector containing the names of the successive images of list.img which are going to be the names of the column of the resulting dataset)
}
}

\value{
The resulting data contains one column containing the zone, and then as many columns as components in list.img. The names of columns can correspond to the name.img vector (default), or to the names of the matrices in list.mat, else it will be Var1, Var2,...
}

\seealso{\link{img.zone},\link{pca},\link{create.zones},\link{summaryrshp}
}
\examples{
data(count)
data(newgp)
data(lifetime)
data(zone.img)

df37A=reshapimg(zone.img,list(count,lifetime,newgp),name.img=c("Count","Lifetime","GP"))
}

