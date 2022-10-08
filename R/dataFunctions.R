################################################################################
##     R SCRIPT: dataFunctions.R
##
##      PACKAGE: SGmisc
##
##  DESCRIPTION: Misc. functions for data manipulation and exploratory data
##               analysis
##
##   WRITTEN BY: Steve Gutreuter
##               E-mail:  sgutreuter@gmail.com
################################################################################

#' Recode a variable conditional on one or more other variables
#'
#' @param .x A vector to modify
#' @param .condition An expression of a condition
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
#' @importFrom dplyr if_else recode
#' @export
recode_if <- function(.x, .condition, ...) {
    dplyr::if_else(.condition, dplyr::recode(.x, ...), .x)
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
count_NA <- function(.data) {
    stopifnot("data.frame" %in% class(.data))
    count_NA_ <- function(x) {
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

#' One and two-way summaries of frequencies, percentages and totals.
#'
#' @param .data A dataframe or tibble.
#' @param colvar Character string naming the factor variable for tabulation.
#' @param rowvar (Optional) string naming factor the row variable for cross-tabulation.
#' @param transpose Logical specifying whether to transpose the result.
#' @param useNA One of \verb{"no"} (default), \verb{"ifany"} or \verb{"always"} specifying
#' the handling of missing values.
#'
#' @return A dataframe containing frequencies and percentages. The columns
#' contain frequecies and row percentages.  A two-way summary is produced if
#' rowvar is specified.  Rows and columns can be reversed by specifying
#' \code{transpose = TRUE}. The result is uglier than a hairless cat, but can
#' easily be converted to useful tabular formats with some custom recoding.
#'
#' @author Steve Gutreuter
#'
#' @keywords table cross-tabulation frequencies percentages
#'
#' @importFrom stats addmargins
#'
#' @export
fp_table <- function(.data, colvar, rowvar = NULL, transpose = FALSE,
                       useNA = c("no", "ifany", "always")){
    if(is.null(rowvar)){
        if(!is.factor(.data[[colvar]])) stop("colvar must be a factor")
        x_<- table(.data[[colvar]], useNA = useNA)
        cols_ <- dim(x_)
        colns_ <- names(x_)
        px_ <- 100 * prop.table(x_)
        x <- stats::addmargins(x_)
        rowtot_ <- sum(x_)
        y_ <- matrix(NA, nrow = 1, ncol = 2 * cols_ + 1)
        for(i in 1:cols_){
            y_[2 * i - 1] <- x_[i]
            y_[2 * i] <- px_[i]
        }
        y_[2 * cols_ + 1] <- rowtot_
        rownames(y_) <- colvar
    } else {
        if(!(is.factor(.data[[colvar]]) & is.factor(.data[[rowvar]])))
            stop("colvar and rowvar must be factors")
        x_ <- table(.data[[rowvar]], .data[[colvar]], useNA = useNA)
        cols_ <- dim(x_)[2]
        colns_ <- colnames(x_)
        x_ <- stats::addmargins(x_, margin = 1)
        rowtot_ <- rowSums(x_)
        rows_ <- dim(x_)[1]
        px_ <- 100 * prop.table(x_, margin = 1)
        x_ <- stats::addmargins(x_, margin = 2)
        y_ <- matrix(NA, nrow = rows_, ncol = 2 * cols_ + 1)
        for(i in 1:cols_){
            y_[, 2 * i - 1] <- x_[, i]
            y_[, 2 * i] <- px_[, i]
        }
        rownames(y_) <- rownames(x_)
        y_[,  2 * cols_ + 1] <- rowtot_
    }
    sufx <- c(":Freq", ":Percent")
    colnames(y_) <- c(as.vector(t((outer(colns_, sufx, paste0)))), "Total")
    if(transpose == TRUE){y_ <- t(y_)}
    if(is.null(rowvar)) rowvar <- NA
    varn <- data.frame(primary = rep(colvar, dim(y_)[1]),
                       secondary = rep(rowvar, dim(y_)[1]),
                       level = rownames(y_))
    y_ <- cbind(varn, y_)
    rownames(y_) <- NULL
    y_
}


#################################  End of File  ################################
