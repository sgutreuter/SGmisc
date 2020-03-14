#################################################################################
##      R PROGRAM: lint_SGmisc.R
##
##        PROJECT: SGmisc Package
##
##    DESCRIPTION: Lint the entire SGmisc package
##
##     WRITTEN BY: Steve Gutreuter
##                 E-mail:  sgutreuter@gmail.gov
#################################################################################

#################################################################################
## Set paths and working directory
codepath <- file.path(Sys.getenv("DEVEL"), "SGmisc/R")
workpath <- file.path(Sys.getenv("DEVEL"), "SGmisc/build_it")
setwd(workpath)

#################################################################################
## Lint the screenr package
#################################################################################
library(lintr)
lint_dir(codepath)

#################################  End of File  #################################
