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

#' Estimate Probability of Mortality Among Key Population Members
#'
#' @description
#' \code{BBS_mortality} computes estimates of the probability of
#' death over some time interval (determined by the survey questions) among
#' members of key populations using information about contacts collected as part
#' of Bio-Behavioral Surveillance Surveys. Estimation includes bootstrap
#' confidence intervals.
#'
#' @param .data a dataframe containing the four variables.
#'
#' @param n_know a character string naming the variable containing the number
#' of contacts known by the subject and who know the subject within the past
#' time interval.
#'
#' @param n_lost a character string naming the variable containing the subset
#' of \code{n_know} for whom the subject lost contact during the past time
#' interval.
#'
#' @param n_died a character string naming the variable containing the subset
#' of \code{n_know} who were known by the subject to have died, by any cause,
#' during the past time interval.
#'
#' @param degree a character string naming the variable containing the network
#' degree of the subject.
#'
#' @param recall_max the maximum contact count that is deemed plausible.
#' Default: 100.
#'
#' @param R an integer-valued count of the number of bootstrap replicates
#' desired for estimation of confidence limits. Default: 2000.
#'
#' @param conf the desired confidence level. Default: 0.95.
#'
#' @param ci_type a character string naming the type of bootstrap confidence
#' interval desired; one of \verb{'bca'} (default), \verb{'perc'} or
#' \verb{'basic'}.  See \code{\link[boot]{boot.ci}}.
#'
#' @details
#' \code{BBS_mortality} eliminates records for which
#' \code{n_know} < \code{n_lost} + \code{n_died} and any observations containing
#' missing values for those variables are also removed.
#'
#' @return
#' A dataframe containing the following variables:
#' \describe{
#' \item{\code{Pr_death}}{The estimated probability of mortality}
#' \item{\code{SE}}{The standard error of the estimate}
#' \item{\code{lower}}{The lower confidence limit}
#' \item{\code{upper}}{The upper confidence limit}
#' \item{\code{conf_level}}{The confidence level}
#' \item{\code{type}}{The type of bootstrap confidence interval}
#' }
#' @author Ian Fellows and Steve Gutreuter.  Algorithm by Ian Fellows.
#' @importFrom stats var
#' @export
BBS_mortality <- function(.data, n_know = NULL, n_lost = NULL, n_died = NULL,
                     degree = NULL, recall_max =  100, R =  2000L, conf = 0.95,
                     ci_type = c("bca", "perc", "basic")) {
    citype <- match.arg(ci_type)
    d_ <- data.frame(n_know = .data[[n_know]],
                    n_lost = .data[[n_lost]],
                    n_died = .data[[n_died]],
                    degree = .data[[degree]])
    for(cx in c("n_know", "n_lost", "n_died", "degree")) {
        if(!is.numeric(d_[[cx]])) stop(paste0(cx, " must be numeric"))
    }
    d_ <- d_[!(is.na(d_$n_know) | is.na(d_$n_lost) | is.na(d_$n_died)),]
    tst <- d_$n_know < (d_$n_died + d_$n_lost)
    if(any(tst))
        warning(paste0(sum(tst),
                       " observation(s) deleted; n_died  >  n_know + n_lost"))
    d_$n_know <- ifelse(d_$n_know > recall_max, NA, d_$n_know)
    d_$n_lost <- ifelse(d_$n_lost > recall_max, NA, d_$n_lost)
    d_$n_died <- ifelse(d_$n_died > recall_max, NA, d_$n_died)
    d_ <- d_[!tst,]
    f_Est <- function(d, indices) {
        d <- d[indices,]
        w <- d$degree
        sw <- sum(w)
        mk <- sum(d$n_know * w) / sw
        ml <- sum(d$n_lost * w) / sw
        md <- sum(d$n_died * w) / sw
        p_lost <- ml / mk
        p_died <- md / mk
        l1 <- log(1 - p_died - p_lost) / (-1 - p_died / p_lost)
        l2 <- p_died * l1 / p_lost
        return(1 - exp(-l2))
    }
    bootObj <- boot::boot(d_, statistic = f_Est, R = R)
    se_ <- sqrt(var(bootObj$t))
    bootCI <- boot::boot.ci(bootObj, type = citype)
    return(data.frame(Pr_death= bootCI$t0,
                      SE = se_,
                      lower = bootCI[[4]][4],
                      upper = bootCI[[4]][5],
                      conf_level = bootCI[[4]][1],
                      type = citype))
}

#' Compute the binomial coefficient \eqn{\binom{n}{k}}
#'
#' @param n a non-negative number
#'
#' @param k another non-negative number such that \eqn{k \le n}
#'
#' @return Numeric value of the binomial coefficient \eqn{\binom{n}{k} = \frac{n!}{k!(n - k)!}}
#'
#' #'
#' @author Steve Gutreuter
#' @export
binom_coef <- function(n, k) {
    if(!n >= 0) stop("n cannot be negative")
    if(!(n >= k & k >=  0)) stop("n >= k >= 0 does not hold")
    exp(lgamma(n + 1) - (lgamma(k + 1) + lgamma(n - k + 1)))
}

#' Compute the binomial expansion \eqn{(x + y)^n}
#'
#' @param x A real number
#'
#' @param y A real number
#'
#' @param n integer-valued exponent
#'
#' @return The numeric value of \{eqn(x + y)^n}
#'
#' @author Steve Gutreuter
#'
#' @importFrom utils combn
#' @export
binom_expansion  <- function(x, y, n) {
    if(!floor(n) == n) stop("n cannot be coerced to an integer")
    sum_  <- 0
    for (i in 0:n) {
        sum_ <- sum_ + utils::combn(n, i) * x^i * (y^(n - 1))
    }
    sum_
}


#' Compute the shape parameters of the Beta distribution from the mean and
#' variance
#'
#' @param mu Mean of the Beta distribution
#' @param var Variance of the Beta distribution
#'
#' @return A list containing the two shape parameters of the Beta distribution
#'
#' @author Steve Gutreuter
#' @export
computeBetaParms <- function(mu, var) {
    if (!((mu < 1) | (mu > 0))) stop("mu must be in (0, 1)")
    if (var >= mu * (1 - mu)) stop("var must be < mu * (1 - mu)")
    a <- ((1 - mu) / var - 1 / mu) * mu ^ 2
    b <- a * (1 / mu - 1)
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
    mean <- a / (a + b)
    var <- a * b / (((a + b)^2) * (a + b + 1))
    fx <- list(mean = mean, variance = var)
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

#' Compute the mean and variance of the Gamma distribution from the
#' shape and rate or scale parameters
#'
#' @param mu Gamma distribution mean
#' @param var Gamma distribution variance
#'
#' @return A list containing the shape, rate and scale parameters of
#' Gamma distribution
#'
#' @author Steve Gutreuter
#' @export
computeGammaParms <- function(mu, var) {
    if (!(mu > 0)) stop("mu must be greater than 0")
    if (!(var > 0)) stop("var must be greater than 0")
    a <- mu^2 / var
    b <- mu / var
    list(shape = a, rate = b, scale =  1 / b)
}

#' Compute the mean and variance of the Gamma distribution from the
#' shape and rate or scale parameters
#'
#' @param shape Gamma distribution shape parameter
#' @param rate Gamma distribution rate parameter
#' @param scale Gamma distribution scale = 1 / rate
#'
#' @return A list containing the mean and variance of the Gamma
#' distribution
#'
#' @author Steve Gutreuter
#' @export
computeGammaMoments <- function(shape, rate = 1, scale = 1 / rate) {
    if (!missing(rate) && !missing(scale)) {
        if (abs(rate * scale - 1) < 1e-15)
            warning("specify 'rate' or 'scale' but not both")
        else stop("specify 'rate' or 'scale' but not both")
    }
    if (missing(rate)) rate <- 1 / scale
    if (!(shape > 0)) stop("shape must be greater than 0")
    if (!(rate > 0)) stop("rate must be greater than 0")

    mu <- shape / rate
    var <- shape / rate^2
    list(mean = mu, variance = var)
}


#' Compute the conventional design effect (Deff) from the intraclass correlation
#'
#' @param icc Numeric intraclass (intracluster) correlation coefficient
#' @param N The average number of elements per class/cluster
#'
#' @return The design effect (Deff)
#'
#' @references
#' Kish L. Survey Sampling. John Wiley & Sons, New York. 1965.
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
#' @param x A numeric real-valued vector
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


#' Compute an extended logit transformation of a real vector
#'
#' @param x A real-valued vector with elements in [0,1].
#'
#' @param repl.inf (Logical) Replace -Inf and Inf values with finite
#' approximations (Default: FALSE).
#'
#' @param bound Absolute value of the finite approximation to -Inf and Inf
#' (Default: 1e-12).
#'
#' @return A numeric vector containing logit(x). See Details.
#'
#' @details
#' In contrast to the conventional logit function which has support on (0,1),
#' this version has support on [0,1], where /code{logit(0)} = \verb{-Inf} and
#' /code{logit(1)} = \verb{Inf}. For sloppy computational convenience when, for
#' example, averaging logits, the infinte values can be replaced with finite
#' approximations.
#'
#' @author Steve Gutreuter
#'
#' @seealso \code{\link[boot]{logit}} from the \code{boot} package for a
#' conventional C++ alternative.
#' @export
logit <- function(x, repl.inf = FALSE, bound = 1e12) {
    stopifnot(x >= 0 & x <= 1)
    fx_ <- log(x / (1 - x))
    if(repl.inf == TRUE) {
        fx_[fx_ == -Inf] <- -bound
        fx_[fx_ == Inf] <- bound
    }
    fx_
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


#' Compute the anti-transpose of a square matrix or table
#'
#' @description The anti-transpose of a square matrix or table \code{x} is the
#' transpose taken across the off-diagonal.  \code{anti-t} is a convenience
#' wrapper for \code{apply(apply(x, 2, rev), 1, rev)}.
#'
#' The anti-transpose function is useful, for example, for converting the
#' results of \code{table(Test, Gold)} to a "confusion" matrix, and vice-versa.
#'
#' A C implementation of the anti-transpose function can also be found as in
#' package \code{gclm} as \code{gclm:::anti_t}.
#'
#' @param x A square matrix or table.
#'
#' @return The anti-transpose (transpose across the off-diagonal) of \code{x}.
#'
#' @examples
#' Gold <- rbinom(20, 1, 0.50)
#' Test <- Gold; Test[c(3, 5, 9, 12, 16)] <- 1 - Test[c(3, 9, 5, 12, 16)]
#' (tb <- table(Test, Gold))
#' anti_t(tb)
#' @export
anti_t <- function(x){
    if(!is.matrix(x) & dim(x)[1] !=  dim(x)[2])
        stop("x must be a square matrix or table")
    result <- apply(apply(x, 2, rev), 1, rev )
    result
    }
################################   END of FILE   ###############################
