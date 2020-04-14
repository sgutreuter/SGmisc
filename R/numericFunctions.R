#################################################################################
##     R SCRIPT: numericFunctions.R
##
##      PACKAGE: SGmisc
##
##  DESCRIPTION: Miscellaneous small numeric functions
##
##   WRITTEN BY: Steve Gutreuter
##               E-mail:  sgutreuter@gmail.com
#################################################################################


#' Logit transformation of a real vector
#'
#' @param x A real-valued vector with elements in (0,1)
#'
#' @return A numeric vector containing logit(x)
#'
#' @author Steve Gutreuter
#'
#' @export
logit <- function(x){ (log(x/(1-x))) }


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
ilogit <- function (x) { 1 / (1 + exp(-x)) }


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
empCDF <- function (x, knots = 10) {
    x.hist <- graphics::hist(x, plot = FALSE, breaks = knots)
    empCDF <- data.frame(x.hist$mids,
    (cumsum(x.hist$counts)/sum(x.hist$counts)))
    names(empCDF) <- c("abscissa", "CDF")
    empCDF
}


#' Estimate HIV incidence rate using "Osmond's" method
#'
#' Estimate HIV incidence rate (infections per person-year) as the simple ratio
#' of the numbers of infections to the total imputed time at risk.  Frequentist
#' confidence intervals are computed by bootstrapping.  Dates of infection are
#' known only within intervals, and individual times-at-risk are imputed as the
#' disease-free periods between sexual debut and either as uniformly distributed
#' random dates between dates of sexual debut and diagnosis or as the midpoints
#' between those dates.
#'
#' @param DF A dataframe containing the disease indicator and an
#' estimate of time at risk.
#' @param x Character-valued name of the binary (0 = negative, 1 = positive)
#' infection status indicator.
#' @param left Character-valued name of the (class = \verb{"Date"}) variable for
#' the earliest possble date of infection.
#' @param right Character-valued name of the (class = \verb{"Date"}) variable for
#' the latest possible date of infection.
#' @param surveyDate Character-valued name of the (class = \verb{"Date"}) variable
#' containing the interview dates.
#' @param debutDate Character-valued name of the (class = \verb{"Date"}) variable
#' containing the self-reported dates of sexual debut.
#' @param imputer Character; either \verb{"random"} (the default) or
#' \verb{"midpoint"} which specifies how the date of infection is imputed.  With
#' argument \code{imputer = "random"}, an independently and identically
#' uniformly distributed date of infection is imputed for each
#' bootstrap replication.  This is slow for reasonable numbers of bootstrap
#' replications.
#' @param sweight (Optional) Numeric survey sampling weights.
#' @param reps Integer number of bootstrap replicates.
#' @param ci.type Bootstrap confidence interval type; any subset of
#' \code{c("norm","basic", "stud", "perc", "bca")}, or \code{"all"}.  The default
#' is \verb{"bca"}, which may require large numbers of bootstrap replicates.
#' @param conf Numeric confidence level for bootstrap intervals.
#' @param ... Additional arguments passed to \code{boot} or \code{boot.ci}
#'
#' @return A list containing:
#' \describe{
#' \item{\verb{boot.res}}{An object of class \verb{boot} including the incidence
#' rate estimate.}
#' \item{\verb{boot.ci}}{An object of class \verb{bootci} containng the
#'  bootstrap confidence intervals.}
#' \item{\verb{Call}}{The function call.}
#' }
#'
#' @author Steve Gutreuter, \email{sgutreuter@@cdc.com}
#'
#' @references
#' Osmond DH, Page K, Wiley J, Garrett K, Sheppard HW, Moss AR, Schrager L,
#' Winkelstein W.  HIV infection in homosexual and bisexual men 18 to 29
#' years of age: The San Francisco Young Men's Health Study.  American
#' Journal of Public Health 1994;84(12):1933-1937.
#'
#' Davison AC, Hinkley DV.  Bootstrap Methods and Their Application, Chapter 5.
#' Cambridge University Press; 1997.
#'
#' @seealso \code{\link{boot}} \code{link{boot.ci}}
#' @importFrom dplyr mutate select if_else
#' @importFrom magrittr `%>%`
#' @importFrom boot boot boot.ci
#' @export
Oz_incidencer <- function(DF, x = NULL, left = NULL, right = NULL,
                          surveyDate = NULL, debutDate = NULL,
                          sweight = NULL, imputer = "random", reps = 1001L,
                          ci.type = "bca", conf = 0.95, ...){
    if(!is.data.frame(DF)) stop("Argument not a dataframe")
    if(!all(c(x, left, right, surveyDate, debutDate) %in% colnames(DF))){
        stop("Some argument(s) not in dataframe")
    }
    if(!(imputer %in% c("random", "midpoint"))) stop("Invalid imputer value")
    this_call  <- match.call()
    if(is.null(sweight)){
        DF$wgt <- rep(1, dim(DF)[1])
    } else {
        DF$wgt <- DF[[sweight]]
    }
    DF <- DF[ , c(x, left, right, surveyDate, debutDate, "wgt")]
    names(DF) <- c("x", "left", "right", "surveyDate", "debutDate", "wgt")
    if(!(imputer == "random")){
        DFn <- DF %>%
            mutate(.tar = if_else(as.logical(x),
                                 as.numeric((right - left) / 365),
                                 as.numeric((SGmisc::mid_date(debutDate, surveyDate) - debutDate) / 365))) %>%
            select(x, .tar, wgt)
        incid <- function(DFn, i){
            sum(DFn$x[i]*DFn$wgt[i], na.rm = TRUE) /
                sum(DFn$.tar[i]*DFn$wgt[i], na.rm = TRUE)
        }
        boot.res <- boot::boot(DFn, incid, R = reps, stype = "i")
    } else {
        incidRand <- function(DF, i){
            M <- dim(DF)[1]
            idx <- sample(seq_along(1:M), M, replace = TRUE)
            DFn <- DF[idx, ] %>%
                mutate(.tar = if_else(as.logical(x),
                                 as.numeric((right - left) / 365),
                                 as.numeric((SGmisc::rand_date(debutDate, surveyDate)
                                     - debutDate)/365))) %>%
                select(x, .tar, wgt)
            sum(DFn$x[i]*DFn$wgt[i], na.rm = TRUE) /
                sum(DFn$.tar[i]*DFn$wgt[i], na.rm = TRUE)
        }
        boot.res <- boot::boot(DF, incidRand, R = reps, stype = "i")
    }
    confint <- boot::boot.ci(boot.res, conf = conf, type = ci.type)
    result <- list(boot.res = boot.res, bootci = confint, Call = this_call)
    result
}


#' Compute the stratum-specific household sample size for a proportion estimated
#' from a Demographic and Health Survey
#'
#' @param p A vector of anticipated proportions.
#' @param Deff The anticipated conventional design effect (square of the Kish
#' design effect, Deft).  Hint: Deff = 2.0 is a tired assumption; try do to better.
#' @param RSE The desired relative standard error of estimation expressed as a proportion.
#' @param R1 Expected proportion of households that participate.
#' @param R2 Expected proportion of eligible household members who participate.
#' @param eligibles The anticipated average number of eligible respondents per
#' household.
#'
#' @return A list containing a vector of estimated required sample sizes (numbers
#' of households) and the function call.
#'
#' @author Steve Gutreuter
#'
#' @references
#' ICF International.  Demographic and Health Survey Sampling and Household
#' Listing Manual.  MEASURE DHS, Calverton, Maryland. 2012.
#' \url{https://dhsprogram.com/pubs/pdf/DHSM4/DHS6_Sampling_Manual_Sept2012_DHSM4.pdf}
#' @export
sampsize_DHS <- function(p = NULL, Deff = NULL, RSE = NULL, R1 = NULL, R2 = NULL,
                         eligibles = NULL){
    if((is.null(p) | is.null(Deff) | is.null(RSE) | is.null(R1) |
                is.null(R2) | is.null(eligibles))) stop("One or more arguments are NULL")
    if(!(p > 0 & p < 1)) stop("p must be a proportion")
    if(Deff < 1) stop("Deff not >= 1")
    if(RSE <= 0 | RSE > 1) stop("RSE not in (0,1])")
    if(R1 <=0 | R1 >1 | R2 <=0 | R2 >1) {
        stop("R1 and R2 must be in (0,1])")
    }
    n <-  Deff * ((1 / p) - 1) / (RSE^2) / R1*R2*eligibles
    ans <- list(households = ceiling(n), Call = match.call())
    ans
}
################################   END of FILE   ################################
