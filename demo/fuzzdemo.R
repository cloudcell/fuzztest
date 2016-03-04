# Description: Demo of the fuzztest package
# Author: cloudcell
# Date: 2016-03
# License: GPL-3

# set demo flags
FUZZ_DEMO_ON = TRUE
RUN_CALC=FALSE
RUN_CALC=TRUE

# source("y:\\devt\\fuzztest\\demo\\fuzzdemo.R")
tmpfpath <- system.file("demo", "include_fuzzdemo.R", package = "fuzztest")

source(file=tmpfpath)

FUZZ_DEMO_ON = FALSE
RUN_CALC=FALSE

