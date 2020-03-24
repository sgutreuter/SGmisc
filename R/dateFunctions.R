#################################################################################
##      R PROGRAM: dateFunctions.R
##
##        PROJECT: SGmisc
##
##    DESCRIPTION: Small functions for dates
##
#################################################################################

#' Compute the midpoint between two dates
#'
#' @param startdate A vector of class \code{Date} containing the beginning
#' date(s)
#' @param enddate A vector of class \code{Date} containing the ending date(s)
#'
#'
#' @return A vector of class Date containing the midpoint date(s) between
#' \code{begindate} and \code{enddate}
#'
#' @author Steve Gutreuter
#'
#' @seealso \code{link(lubridate::interval)}
#'
#' @examples
#' begin <- ymd("2020-06-21", "2021-06-21")
#' end <- ymd("2020-12-21", "2021-12-21")
#' mid_date(begin, end)
#'
#' @importFrom lubridate interval as_date int_start int_end
#' @export
mid_date <- function(startdate, enddate){
    stopifnot(class(startdate) == "Date" & class(enddate) == "Date")
    intobj <- lubridate::interval(startdate, enddate)
    lubridate::as_date(lubridate::int_start(intobj) +
            ((lubridate::int_end(intobj) - lubridate::int_start(intobj)) / 2))
}


#' Compute uniformly distributed random date(s) between two dates
#'
#' @param startdate A vector of class \code{Date} containing the beginning
#' date(s)
#' @param enddate A vector of class \code{Date} containing the ending date(s)
#' @param size An integer vector of the number of random dates
#' @param simplify (Logical) controlling whether the resulting list is
#' simplfied to a vector or matrix
#'
#' @return A list or vector of elements of class \code{Date} containing uniformly
#' distributed dates in the closed interval (\code{begindate}, \code{enddate}).
#'
#' @author Steve Gutreuter
#'
#' @seealso \code{link(lubridate::interval)}
#'
#' @examples
#' begin <- ymd("2020-06-21", "2021-06-21")
#' end <- ymd("2020-12-21", "2021-12-21")
#' rand_date(begin, end, size = 5)
#'
#' rand_date(begin, end, simplify = TRUE)
#'
#' @export
rand_date <- function(startdate, enddate, size = 1, simplify = FALSE){
    stopifnot(class(startdate) == "Date" & class(enddate) == "Date")
    M <- length(startdate)
    stopifnot(M == length(enddate))
    res <- rep(list(rep(as.Date(NA), size)), M)
    for(i in 1:M){
        res[i] <- list(sample(x = seq.Date(from = startdate[i],
                                      to = enddate[i], by = "day"),
                         size = size))
    }
    if(simplify & size == 1) {
        res <- do.call("c", res)
    }
    res
}
