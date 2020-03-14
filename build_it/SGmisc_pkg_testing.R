#################################################################################
##      R PROGRAM: SGmisc_pkg_testing.R
##
##        PROJECT: SGmisc Package
##
##    DESCRIPTION: Testing sandbox code for SGmisc
##
##     WRITTEN BY: Steve Gutreuter
##                 E-mail:  sgutreuter@gmail.gov
#################################################################################

#################################################################################
## Set paths and working directory
codepath <- file.path(Sys.getenv("DEVEL"), "SGmisc/R")
workpath <- file.path(Sys.getenv("DEVEL"), "SGmisc/buildr")
setwd(workpath)

#################################################################################
## Either source the screenr R code or load the package, but not both:
#################################################################################
## 1. Source the code
source(file.path(codepath, "dataFunctions.R"))
source(file.path(codepath, "statFunctions.R"))

## 2. Load the screener package
library(SGmisc)
?SGmisc
