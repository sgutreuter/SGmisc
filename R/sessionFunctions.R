################################################################################
##     R SCRIPT: sessionFunctions.R
##
##      PACKAGE: SGmisc
##
##  DESCRIPTION: Miscellaneous small functions for R session management
##
##   WRITTEN BY: Steve Gutreuter
##               E-mail:  sgutreuter@gmail.com
################################################################################


#' Detach all packages other than those attached by base R
#'
#' This function is used for its side effects and returns nothing.  Do no harm
#' to others; call this function only within your own interactive session.
#' Some namespaces may not be unloaded.
#' @importFrom utils sessionInfo
#' @export
nuke_packages <- function(){
    invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)),
                     detach, character.only=TRUE, unload=TRUE))
}

################################   END of FILE   ###############################
