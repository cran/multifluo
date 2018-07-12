\name{summaryrshp}
\alias{summaryrshp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summarizing reshaped data
}
\description{
Gives natural statistics (number of observations, average, standard deviations) for each variable and each zones. 
 ANOVA of the model "variable~zone" is run, and difference tests are computed to find groups.
}
\usage{
summaryrshp(rshp, test = "hsd", p.adj = "none", alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rshp}{
data resulting from \link{reshapimg}
}
  \item{test}{
"lsd" (corresponding to Least Significant Difference) or "hsd" (corresponding to the Tukey Test)
}
  \item{p.adj}{
LSD.test parameter to adjust the p-value for the multiple comparison
}
  \item{alpha}{
Threshold of significance for determining the gorups
}
}

\value{
Returns a list containing, for each variable: the results of the ANOVA; the summary of the variable; and the usual statistics by zones (n,average, sd,group)
}


\seealso{
\link{reshapimg}
}
\examples{
data(df.scaled)
summaryrshp(df.scaled)
}

