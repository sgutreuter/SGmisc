% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numericFunctions.R
\name{sampsize_DHS}
\alias{sampsize_DHS}
\title{Compute the stratum-specific cluster/household sample size for proportions
from binomial counts estimated from a Demographic and Health Survey}
\usage{
sampsize_DHS(
  p = NULL,
  Deff = NULL,
  RSE = NULL,
  R1 = NULL,
  R2 = NULL,
  eligibles = NULL
)
}
\arguments{
\item{p}{A vector of anticipated proportions.}

\item{Deff}{The anticipated conventional design effect (square of the Kish
design effect, Deft).  Hint: Deff = 2.0 is an over-used assumption; try do
to better.}

\item{RSE}{The desired relative standard error of estimation expressed as a
proportion.}

\item{R1}{Expected proportion of clusters (households) that participate.}

\item{R2}{Expected proportion of eligible clusters (household) members who
participate.}

\item{eligibles}{The anticipated average number of eligible respondents per
cluster (household).}
}
\value{
A list containing a vector of estimated required sample sizes
(numbers of clusters/households) and the function call.
}
\description{
Compute the stratum-specific cluster/household sample size for proportions
from binomial counts estimated from a Demographic and Health Survey
}
\references{
ICF International.  Demographic and Health Survey Sampling and Household
Listing Manual.  MEASURE DHS, Calverton, Maryland. 2012.
\url{https://dhsprogram.com/pubs/pdf/DHSM4/DHS6_Sampling_Manual_Sept2012_DHSM4.pdf}
}
\author{
Steve Gutreuter
}
