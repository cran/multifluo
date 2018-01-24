\name{find.colors}
\alias{find.colors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
find.colors
}
\description{
Finds different colors for a corresponding to the first character of each value of a vector.
}
\usage{
find.colors(vect, color = NULL)
}

\arguments{
  \item{vect}{
the vector to find colors for.
}
  \item{color}{
can be a vector of colors (with the same size as the number of groups), if NULL, rainbow scale is chosen
}
 
}
\value{
vector of colors related to vect
}

\examples{
find.colors(c("V1","V3","R22"))

  }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.)