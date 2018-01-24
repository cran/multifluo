\name{intpca}
\alias{intpca}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
internal pca
}
\description{
runs pca
}
\usage{
intpca(vars,ind,map="b",si=NULL,ic=NULL,sic=NULL,ell=NULL,seg=NULL,epd=NULL,x="",y="")}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vars}{
variables
}
  \item{ind}{
individuals
}
  \item{map}{
if "bil", biplot is plotted with supplementary individuals projected, and labels of supplementary individuals are written
if "bip", biplot is plotted with supplementary individuals projected, and points of supplementary individuals are plotted
if "b", biplot is plotted 
if "til", two-way pca is plotted with supplementary individuals projected, and labels of supplementary individuals are written
if "tip", two-way pca is plotted with supplementary individuals projected, and la points of supplementary individuals are plotted
if "t", two-way pca is plotted
}
 \item{si}{
supplementary individuals
}
  \item{ic}{
color of individuals
}
  \item{sic}{
color of supplementary individuals
}
  \item{ell}{
"bar" or "ind" or "none"
}
  \item{seg}{
TRUE or FALSE
}
  \item{epd}{
expand
}
  \item{x}{
x label
}
  \item{y}{
y label
}
 
}
