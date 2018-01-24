\name{calc.ell}
\alias{calc.ell}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculating agreement ellipses
}
\description{
calculates the coordinates of agreement ellipses
}
\usage{
calc.ell(sup.ind,vep,axes=c(1,2),conf=0.9,ell.t="bar",trt="zone",sup="pixel",calc="Chi")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sup.ind}{
matrix of supplementary individuals
}
  \item{vep}{
eigenvectors
}
  \item{axes}{
selected axes (as a vector)
}
  \item{conf}{
confidence interval
}
  \item{ell.t}{
"barycentric" or "individual"
}
  \item{trt}{
name of trt
}
  \item{sup}{
name of the individuals around a same trt
}
  \item{calc}{
"Chi","F" or "SAS"
}
}