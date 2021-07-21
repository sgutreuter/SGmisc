################################################################################
##     R SCRIPT: numericFunctions.R
##
##      PACKAGE: SGmisc
##
##  DESCRIPTION: Miscellaneous small numeric functions
##
##   WRITTEN BY: Steve Gutreuter
##               E-mail:  sgutreuter@gmail.com
################################################################################


#' Compute the shape parameters of the Beta distribution from the mean and
#' variance
#'
#' @param mean Mean of the Beta distribution
#' @param var Variance of the Beta distribution
#'
#' @return A list containing the two shape parameters of the Beta distribution
#'
#' @author Steve Gutreuter
#' @export
computeBetaParms <- function(mean, var) {
    if (!((mean < 1) | (mean > 0))) stop("mean must be in (0, 1)")
    if (var >= mean * (1 - mean)) stop("var must be < mean * (1 - mu)")
    a <- ((1 - mean) / var - 1 / mean) * mean ^ 2
    b <- a * (1 / mean - 1)
    fx <- list(a = a, b = b)
    fx
}


#' Compute the mean and variance of the Beta distribution from the shape
#' parameters
#'
#' @param a Beta distribution shape1 parameter
#' @param b Beta distribution shape2 parameter
#'
#' @return A list containing the mean and variance
#'
#' @author Steve Gutreuter
#' @export
computeBetaMoments <- function(a, b) {
    if (!(a > 0 & b > 0)) stop("a and b must be greater than 0")
    m_ <- a / (a + b)
    v_ <- a * b / (((a + b)^2) * (a + b + 1))
    fx <- list(mean = m_, variance = v_)
    fx
}


#' Compute the discrete empirical cumulative distribution of a real vector
#'
#' @param x A numeric vector
#' @param knots Number of breaks in the abscissa
#'
#' @return A numeric vector containing the discrete empirical CDF
#'
#' @author Steve Gutreuter
#' @importFrom graphics hist
#' @export
empCDF <- function(x, knots = 10) {
    x.hist <- graphics::hist(x, plot = FALSE, breaks = knots)
    fx <- data.frame(x.hist$mids,
    (cumsum(x.hist$counts) / sum(x.hist$counts)))
    names(fx) <- c("abscissa", "CDF")
    fx
}


#' Compute the conventional design effect (Deff) from the intraclass correlation
#'
#' @param icc Numeric intraclass (intracluster) correlation coefficient
#' @param N The average number of elements per class/cluster
#'
#' @return The design effect (Deff)
#'
#' @author Steve Gutreuter
#' @export
icc2deff <- function(icc, N) {
    if (!(icc > 0 & icc < 1)) stop("icc must be in (0, 1)")
    if (!N > 0) stop("N must be > 0")
    fx <- 1 + icc * (N - 1)
    fx
}


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
ilogit <- function(x) {
    stopifnot(is.numeric(x))
    1 / (1 + exp(-x))
}
#' @rdname ilogit
#' @export
expit <- ilogit


#' Logit transformation of a real vector
#'
#' @param x A real-valued vector with elements in [0,1]
#'
#' @return A numeric vector containing logit(x)
#'
#' @author Steve Gutreuter
#'
#' @seealso \code{\link[boot]{logit}} from the \code{boot} package for a C++
#' alternative.
#' @export
logit <- function(x) {
    stopifnot(x >= 0 & x <= 1)
    fx <- (log(x / (1 - x)))
    fx
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
#' @param right Character-valued name of the (class = \verb{"Date"}) variable
#' for the latest possible date of infection.
#' @param surveyDate Character-valued name of the (class = \verb{"Date"})
#' variable containing the interview dates.
#' @param debutDate Character-valued name of the (class = \verb{"Date"})
#' variable containing the self-reported dates of sexual debut.
#' @param imputer Character; either \verb{"random"} (the default) or
#' \verb{"midpoint"} which specifies how the date of infection is imputed.
#' With argument \code{imputer = "random"}, an independently and identically
#' uniformly distributed date of infection is imputed for each
#' bootstrap replication.  This is slow for reasonable numbers of bootstrap
#' replications.
#' @param sweight (Optional) Numeric survey sampling weights.
#' @param reps Integer number of bootstrap replicates.
#' @param ci.type Bootstrap confidence interval type; any subset of
#' \code{c("norm","basic", "stud", "perc", "bca")}, or \code{"all"}.  The
#' default is \verb{"bca"}, which may require large numbers of bootstrap
#' replicates.
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
#' @author Steve Gutreuter
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
#' @seealso \code{\link[boot]{boot}} \code{\link[boot]{boot.ci}}
#' @importFrom dplyr mutate select if_else
#' @importFrom magrittr `%>%`
#' @importFrom boot boot boot.ci
#' @export
Oz_incidencer <- function(DF, x = NULL, left = NULL, right = NULL,
                          surveyDate = NULL, debutDate = NULL,
                          sweight = NULL, imputer = "random", reps = 1001L,
                          ci.type = "bca", conf = 0.95, ...) {
    if (!is.data.frame(DF)) stop("Argument not a dataframe")
    if (!all(c(x, left, right, surveyDate, debutDate) %in% colnames(DF))) {
        stop("Some argument(s) not in dataframe")
    }
    if (!(imputer %in% c("random", "midpoint"))) stop("Invalid imputer value")
    this_call  <- match.call()
    if (is.null(sweight)) {
        DF$wgt <- rep(1, dim(DF)[1])
    } else {
        DF$wgt <- DF[[sweight]]
    }
    tar <- wgt <- incid <- incidRand <- NULL
    DF <- DF[, c(x, left, right, surveyDate, debutDate, "wgt")]
    names(DF) <- c("x", "left", "right", "surveyDate", "debutDate", "wgt")
    if (!(imputer == "random")) {
        DFn <- DF %>%
            mutate(tar = if_else(as.logical(x),
                                 as.numeric((right - left) / 365),
                                 as.numeric((SGmisc::mid_date(debutDate,
                                                              surveyDate) -
                                             debutDate) / 365))) %>%
            select(x, tar, wgt)
        incid <- function(DFn, i) {
            sum(DFn$x[i] * DFn$wgt[i], na.rm = TRUE) /
                sum(DFn$tar[i] * DFn$wgt[i], na.rm = TRUE)
        }
        boot.res <- boot::boot(DFn, incid, R = reps, stype = "i")
    } else {
        incidRand <- function(DF, i) {
            M <- dim(DF)[1]
            idx <- sample(seq_along(1:M), M, replace = TRUE)
            DFn <- DF[idx, ] %>%
                mutate(tar = if_else(as.logical(x),
                                 as.numeric((right - left) / 365),
                                 as.numeric((SGmisc::rand_date(debutDate,
                                                               surveyDate)
                                     - debutDate) / 365))) %>%
                select(x, tar, wgt)
            sum(DFn$x[i] * DFn$wgt[i], na.rm = TRUE) /
                sum(DFn$tar[i] * DFn$wgt[i], na.rm = TRUE)
        }
        boot.res <- boot::boot(DF, incidRand, R = reps, stype = "i")
    }
    confint <- boot::boot.ci(boot.res, conf = conf, type = ci.type)
    fx <- list(boot.res = boot.res, bootci = confint, Call = this_call)
    fx
}


#' Compute the stratum-specific cluster/household sample size for proportions
#' from binomial counts estimated from a Demographic and Health Survey
#'
#' @param p A vector of anticipated proportions.
#' @param Deff The anticipated conventional design effect (square of the Kish
#' design effect, Deft).  Hint: Deff = 2.0 is an over-used assumption; try do
#' to better.
#' @param RSE The desired relative standard error of estimation expressed as a
#' proportion.
#' @param R1 Expected proportion of clusters (households) that participate.
#' @param R2 Expected proportion of eligible clusters (household) members who
#' participate.
#' @param eligibles The anticipated average number of eligible respondents per
#' cluster (household).
#'
#' @return A list containing a vector of estimated required sample sizes
#' (numbers of clusters/households) and the function call.
#'
#' @author Steve Gutreuter
#'
#' @references
#' ICF International.  Demographic and Health Survey Sampling and Household
#' Listing Manual.  MEASURE DHS, Calverton, Maryland. 2012.
#' \url{https://dhsprogram.com/pubs/pdf/DHSM4/DHS6_Sampling_Manual_Sept2012_DHSM4.pdf}
#' @export
sampsize_DHS <- function(p = NULL, Deff = NULL, RSE = NULL, R1 = NULL, R2 = NULL,
                         eligibles = NULL) {
    if ((is.null(p) | is.null(Deff) | is.null(RSE) | is.null(R1) |
        is.null(R2) | is.null(eligibles))) {
        stop("One or more arguments are NULL")
    }
    if (!(p > 0 & p < 1)) stop("p must be a proportion")
    if (Deff < 1) stop("Deff not >= 1")
    if (RSE <= 0 | RSE > 1) stop("RSE not in (0,1])")
    if (R1 <= 0 | R1 > 1 | R2 <= 0 | R2 > 1) {
        stop("R1 and R2 must be in (0,1])")
    }
    n <-  Deff * ((1 / p) - 1) / (RSE^2) / R1 * R2 * eligibles
    fx <- list(households = ceiling(n), Call = match.call())
    fx
}


#' Compute the approximate worst-case sample size for a vector of multinomial
#' proportions
#'
#' @param m The (integer) number of elements (at least two) in the vector of
#' equal proportions.
#' @param relmoe Relative margin of error (relative half-width of the confidence
#' interval), expressed as a proportion.  A value of 0.25 specifies margin of
#' error equal to 0.25 times the value of the (equal) proportions.
#' @param conf Confidence level.  The default is 0.95.
#'
#' @return A list containing the following elements:
#' \describe{
#' \item{\verb{sample_size}}{The required sample size.}
#' \item{\verb{target_proportions}}{The anticipated vector of multinomially
#' distributed outcomes.}
#' \item{\verb{moe}}{The absolute margins of error for estimation of
#' \verb{target_proportions}.}
#' \item{\verb{Call}}{The function call.}
#' }
#'
#' @author Steve Gutreuter
#'
#' @references
#' Thompson SK. Sample size for estimating multinomial proportions. The American
#' Statistician 1987; 41(1):42-46.
#' @importFrom stats qnorm
#' @export
sampsize_multinomial <- function(m, relmoe, conf = 0.95) {
    stopifnot(m > 1)
    stopifnot(relmoe > 0 & relmoe <= 1)
    p <- 1 / m
    d <- relmoe * p
    n <- qnorm(1 - (1 - conf) / 2)^2 * p * (1 - p) / d^2
    list(sample_size = ceiling(n),
         target_proportions = rep(p, m),
         moe = rep(d, m),
         Call = match.call())
}


#' Compute a smooth differentiable approximation to the minimum or maximum of a
#' numeric vector
#'
#' @param x A numeric vector.
#' @param type Character \code{"min"} or \code{"max"} specifying the type of
#' extremum desired.
#' @param alpha A numeric smoothing parameter (\code{alpha} > 0).
#'
#' @return An approximation to the minimum or maximum of \code{x}.
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Smooth_maximum}
#' @export
smooth_extremum <- function(x, type = NULL, alpha = 4) {
    stopifnot(is.numeric(x))
    stopifnot(alpha > 0)
    if(!type %in% c("min", "max")) stop("Argument type must be 'min' or 'max'.")
    s <- ifelse(type == "min", -1, 1)
    res <- sum(x * exp(s * alpha * x)) / sum(exp(s * alpha * x))
    res
}


################################   END of FILE   ###############################
