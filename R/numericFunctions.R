## Small statistical functions

#' Logit transformation of a real vector
#'
#' @param x A real-valued vector with elements in (0,1)
#'
#' @return A numeric vector containing logit(x)
#'
#' @author Steve Gutreuter
#'
#' @export
logit <- function(x){
         return(log(x/(1-x))) }

#' Inverse logit (expit) tranformation of a numeric vector
#'
#' @param x A numeric vector
#'
#' @return A numeric vector of the inverse logits of x.
#'
#' @author Steve Gutreuter
#'
#' @keywords expit
#' @export
ilogit <- function (x) {1 / (1 + exp(-x))}

#' Compute the discrete empirical cumulative distribution of a real vector
#'
#' @param x A numeric vector
#' @param knots Number of breaks in the abscissa
#'
#' @return A numeric vector
#'
#' @author Steve Gutreuter
#' @importFrom graphics hist
#' @export
empCDF <- function (x, knots=10) {
    x.hist <- graphics::hist(x, plot=FALSE, breaks=knots)
    empCDF <- data.frame(x.hist$mids,
    (cumsum(x.hist$counts)/sum(x.hist$counts)))
    names(empCDF) <- c("abscissa", "CDF")
    empCDF
}

#' Compute the stratum-specific sample size for a proportion estimated from a
#' Demographic and Health Survey
#'
#' @param p A vector of anticipated proportions
#' @param Deff The anticipated design effect
#' @param RSE The desired relative standard error of estimation
#' @param R1 Household response fraction
#' @param R2 Individual response fraction
#' @param eligibles The anticipated average number of eligible respondents per
#' household
#'
#' @return A numeric vector of estimated sample sizes
#'
#' @author Steve Gutreuter
#'
#' @seealso \url{https://dhsprogram.com/publications/publication-dhsg1-dhs-questionnaires-and-manuals.cfm}
#' @export
sampsize_DHS <- function(p = 0.5, Deff = 2.0, RSE = 1, R1 = 1, R2 = 1,
                         eligibles = 1){
    n <-  (Deff^2) * ((1/p) - 1)/RSE / R1*R2*eligibles
    ans <- list(estimated_proportion = p, design_effect = Deff,
                relative_SE = RSE, individual_response_rate = R1,
                household_response_rate = R2,
                eligibles_per_household = eligibles, sample_size = n)
    ans
}
