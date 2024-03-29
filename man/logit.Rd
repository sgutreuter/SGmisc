% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numericFunctions.R
\name{logit}
\alias{logit}
\title{Compute an extended logit transformation of a real vector}
\usage{
logit(x, repl.inf = FALSE, bound = 1e+12)
}
\arguments{
\item{x}{A real-valued vector with elements in [0,1].}

\item{repl.inf}{(Logical) Replace -Inf and Inf values with finite
approximations (Default: FALSE).}

\item{bound}{Absolute value of the finite approximation to -Inf and Inf
(Default: 1e-12).}
}
\value{
A numeric vector containing logit(x). See Details.
}
\description{
Compute an extended logit transformation of a real vector
}
\details{
In contrast to the conventional logit function which has support on (0,1),
this version has support on [0,1], where /code{logit(0)} = \verb{-Inf} and
/code{logit(1)} = \verb{Inf}. For sloppy computational convenience when, for
example, averaging logits, the infinte values can be replaced with finite
approximations.
}
\seealso{
\code{\link[boot]{logit}} from the \code{boot} package for a
conventional C++ alternative.
}
\author{
Steve Gutreuter
}
