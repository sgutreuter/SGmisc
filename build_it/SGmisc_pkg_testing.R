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
workpath <- file.path(Sys.getenv("DEVEL"), "SGmisc/build_it")
setwd(workpath)

#################################################################################
## Either source the screenr R code or load the package, but not both:
#################################################################################
## 1. Source the code
source(file.path(codepath, "dataFunctions.R"))
source(file.path(codepath, "statFunctions.R"))
source(file.path(codepath, "dateFunctions.R"))

## 2. Load the screener package
library(SGmisc)
?SGmisc


begin <- ymd("2020-06-21", "2021-06-21")
end <- ymd("2020-12-21", "2021-12-21")
mid_date(begin, end)


debugonce(rand_date)
wtf <- rand_date(begin, end, size = 2, simplify = TRUE)
wtf
wtf <- rand_date(begin, end, simplify = TRUE)
wtf
