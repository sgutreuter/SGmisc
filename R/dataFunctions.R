#################################################################################
##      R PROGRAM: dataFunctions.R
##
##        PROJECT: SGmisc
##
##    DESCRIPTION: Misc. functions for data manipulation and exploratory data
##                 analysis
##
##     WRITTEN BY: Steve Gutreuter
##                 E-mail:  sgutreuter@gmail.gov
##
##           DATE: 201_-__-__
##
#################################################################################

#' Recode a variable conditional on one or more other variables
#'
#' @param .x A vector to modify
#' @param condition
#' @param ... Replacement (see dplyr::recode)
#'
#' @return Contents of "Value"
#'
#' @author Unknown
#'
#' @references
#' \url{https://gerkelab.com/blog/2018/08/recode_if}
#'
#' @seealso \code{\link{dplyr}}
#'
#' @keywords recode
#'
#' @importFrom dplyr
#' @export
recode_if <- function(.x, condition, ...){
    if_else(condition, dplyr::recode(.x, ...), .x)
}


#' Count the numbers of NA, NaN and Inf in each column of a dataframe
#'
#' @param .data A dataframe or tibble
#'
#' @return A matrix containing counts of NA, NaN and Inf values.
#'
#' @author Steve Gutreuter
#'
#' @keywords EDA, missing values, NA
#' @export
count_NA <- function(.data){
    stopifnot("data.frame" %in% class(.data))
    count_NA_ <- function(x){
        N_Inf <- sum(is.infinite(x))
        N_NaN <- sum(is.nan(x))
        N_NA  <-  sum(is.na(x)) - N_NaN
        data.frame(N_NA, N_NaN, N_Inf)
    }
    x_ <- lapply(.data, count_NA_)
    x_ <- matrix(unlist(x_), nrow = 3)
    colnames(x_) <- names(.data)
    rownames(x_) <- c("NA", "NaN", "Inf")
    x_
}

tst <- tibble(w = c(1, 3, 5, NA, 7, NaN, Inf),
              x = 5 + rnorm(7),
              y = c(Inf, 2 + rnorm(6)),
              z = c(rep(NA, 3), 3 + rnorm(4)))
count_NA(tst)
tst2 <- tst
class(tst2) <- "toad"
count_NA(tst2)
#################################  End of File  #################################
