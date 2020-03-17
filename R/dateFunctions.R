## Small functions for dates

#' Compute the midpoint between two dates
#'
#' @param startdate A vector of class \code{Date} containing the beginning date(s)
#' @param enddate A vector of class \code{Date} containing the ending date(s)
#'
#'
#' @return A vector of class Date containing the midpoint date(s) between
#' \code{begindate} and \code{enddate}
#'
#' @seealso \code{link(lubridate::interval)}
#'
#' @importFrom lubridate
#' @export
mid_date <- function(startdate, enddate){
    stopifnot(class(startdate) == "Date" & class(enddate) == "Date")
    intobj <- lubridate::interval(startdate, enddate)
    lubridate::as_date(lubridate::int_start(intobj) +
            ((lubridate::int_end(intobj) - lubridate::int_start(intobj)) / 2))
}

debugonce(mid_date)
wtf <- mid_date(DF$debutdate[1], DF$today[1])

as.Date(as.integer(int_start(intobj)), origin = "1970-01-01")
as.Date(as.integer(int_start(intobj)), origin = 0)

sample(x = seq(from = DF$debutdate[1], to = DF$today[1],
                                        by = "day"),
                                size = 1)



#' Compute uniformly distributed random date(s) between two dates
#'
#' @param startdate A vector of class \code{Date} containing the beginning
#' date(s)
#'
#' @param enddate A vector of class \code{Date} containing the ending date(s)
#'
#' @return A vector of class \code{Date} containing uniformly distribute dates in
#' the closed interval (\code{begindate}, \code{enddate}).
#'
#' @seealso \code{link(lubridate::interval)}
#'
#' @importFrom lubridate
#' @export
rand_date <- function(startdate, enddate, size = 1){
    stopifnot(class(startdate) == "Date" & class(enddate) == "Date")
    sample(x = seq(from = startdate, to = enddate, by = "day"),
           size = size)
}
