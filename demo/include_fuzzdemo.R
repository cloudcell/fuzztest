################################################################################
# Description: A Demo of the Package fuzztest (a Work in Progress)
# Author: cloudcell
# Date: 2016-02-29
# License: GPL-3
################################################################################

if( !exists("FUZZ_DEMO_ON") || !FUZZ_DEMO_ON ) { 
    stop ("This file must only be started by 'fuzzdemo.R'") 
}

tmpfpath <- system.file("demo", "include_fuzzdemofunc.R", package = "fuzztest")
source(tmpfpath)

# if FALSE, the demo will only display text
# RUN_CALC=FALSE

# options: a,b,c
# suboptions: a,b,c,d

message("Demo of the fuzztest package") 
message("") 
message("+----------------------------------------------------------+") 
message("|  Note: some charts may require some time to be plotted.  |") 
message("|  Please, be patient.                                     |") 
message("+----------------------------------------------------------+") 

waitForUserInput()

################################################################################
# CONTROL FLOW TEST
################################################################################
if(RUN_CALC) {
    set.seed(0)
    r <- list()
    r$x <- c(0)
    r$y <- c(0)
    r$option <- c("a", "b", "c")
    r$suboption <- c("a", "b", "c","d")
    
    generate.argset(arg_register = r, display_progress=TRUE)
    # apply.argset(FUN="fuzzdemofunc", DEBUG = T)
    apply.argset(FUN="fuzzdemofunc")#, DEBUG = T)
    test_summary()
    plot_tests()
    # plot.tests(pass = F)
    # plot.tests(fail = F)
}

waitForUserInput()

message("The summary shows that the argument 'option' explains the most") 
message("variability in the outcome. So let's concentrate on the arg. 'option.'")
message("The detailed test result table, shows that that most failures occur")
message("when option value #3 is selected. At the same time, test log ") 
message("shows that the types of errors are mixed. But for now, let's assume")
message("that fixing the bugs related to control flow is more important. ")
message("...")
message("Let's assume all the control flow related bugs are fixed without ")
message("actually going and changing anything in the code. For that ")
message("assumption to work, we will choose a combination of options ")
message("that results in the test outcome 'PASS'. Such a combination ")
message("could be, for example, {x=0, y=0, option='a', suboption='a'}.")

waitForUserInput()

################################################################################
# NUMERIC TEST
################################################################################
message("...")
message("Now we will concentrate on the numeric part of the test.")
message("There are two main approaches here:")
message(" 1. create an evenly spaced sequence of values for each (x and y)")
message("    parameter from lowest to highest and let the argument set generator")
message("    combine and test these combinations. This second approach has")
message("    has an advantage in that it aligns numeric values with ")
message("    the cardinal number of the group associated with an option")
message("    for each argument. For example, if we create a test sequence")
message("    [-10;+10] with a step of 1 for argument 'x', there will be ")
message("    21 options for argument 'x'. The visualized test results")
message("    will list those from 'Min' to 'Max'. So finding errors ")
message("    will be easier as it will be easier to deduce the dependence ")
message("    of failures on a range of input values from 'x'. ")
message(" 2. generate random parameters for selected arguments and let the test")
message("    framework generate and test all possible parameter combinations.")
message("    ")
message("...")
message("Now the first approach will be demonstrated:")

waitForUserInput()

if(RUN_CALC) { 
    set.seed(0)
    r <- list()
    # r$x <- c(seq(from=-10, to=10, length.out = 21))
    # r$y <- c(seq(from=-10, to=10, length.out = 21))
    r$x <- c(seq(from=-5, to=5, length.out = 11))
    r$y <- c(seq(from=-5, to=5, length.out = 11))
    r$option <- c("a")
    r$suboption <- c("a")
    
    generate.argset(arg_register = r, display_progress=TRUE)
    apply.argset(FUN="fuzzdemofunc")
    test_summary()
    plot_tests()
    # plot.tests(pass = F)
    # plot.tests(fail = F)
}

waitForUserInput()

message("Now one can clearly see two linear relationships between ")
message("'x' and 'y'. These correspond to 'numeric bugs' #1NC and 4NC")
message("(Please, see details in the file 'include_fuzzdemofunc.R')")
message("...")
message("Let's assume those bugs are fixed now.")

waitForUserInput()

################################################################################
# FUZZ TESTING
################################################################################
message("...")
message("Next, the second (random testing) approach will be demonstrated")
message("for numeric arguments only, although character inputs could be ")
message("easily indexed and randomly selected as well (if there were ")
message("arguments requiring character input in this particular demo function.")
message("As for options, it makes no sense randomizing those as all")
message("combinations of provided values will be tested anyway.")

waitForUserInput()

if(RUN_CALC) { 
    set.seed(0)
    r <- list()
    r$x <- runif(15, min=-10, max=10)
    r$y <- runif(15, min=-10, max=10)
    r$option <- c("b")
    r$suboption <- c("a","b","c","d")
    
    generate.argset(arg_register = r, display_progress=TRUE)
    apply.argset(FUN="fuzzdemofunc")
    test_summary()
    plot_tests()
}

waitForUserInput()
message("Immediately one can see the following:")
message("The text output shows that suboption #3 ('c') is always failing.")
message("Those axes that have only one option ('level') should either be hidden")
message("or placed at an edge of the plot. This axis reordering can be done")
message("using various methods; however, for the sake of simplicity, we will")
message("create a test register with a different sequence of arguments. ")
message("See source code of the demo for details. Let's use a smaller set")
message("of parameters to speed up testing.")
waitForUserInput()


if(RUN_CALC) { 
    set.seed(0)
    r <- list()
    r$x <- runif(5, min=-10, max=10)
    r$y <- runif(5, min=-10, max=10)
    r$suboption <- c("a","b","c","d")
    r$suboption <- c("a","b","c","d")
    r$option <- c("b")
    
    generate.argset(arg_register = r, display_progress=TRUE)
    apply.argset(FUN="fuzzdemofunc")
    test_summary()
    plot_tests()
}

message("The visual chart still looks messy. This can happen when there is a ")
message("disproportional difference in the number of parameters tested ")
message("among arguments of a function. To make the plot more clear, one")
message("can adjust the 'dist' argument of the plot_tests() function.")
message("We will try setting it to 15: 'plot_tests(dist=15)', which means")
message("that the gap between adjacent groups will fit the max number of")
message("parameters 15 times (see help on 'plot_tests()' for more info).")

waitForUserInput()

if(RUN_CALC) { 
    plot_tests(dist=15)
}
    
waitForUserInput()
message("There are many ways to proceed from here:")
message("* if bugs are trivial, eliminate each bug one by one.")
message("* if bugs are intractable, one can start with narrowing down the")
message("  range of input parameters and analyze function behavior further.")

message("")
message("-------------")
message(" End of Demo ")
message("-------------")



################################################################################
# Notes:
# 
# Parameter sets are evenly spread along vertical axes. If only one parameter
# option occupies the axis "test graph lines" will be evenly spread from 
# the bottom to the top of the chart.
# 
# The chart is drawn as follows.
# First, all test lines for those tests that returned the PASS status. They
# Second, all the test lines for FAIL tests.
# All the test lines are drawn in such a way so that when the lines intersect
# axes, no line occludes another line. I.e. each test line must be visible
# at such junction points with FAIL test lines occupying the lower portion
# of the range allocated for each 'option' on an axis.
# 
# 
# Ref on Multi-D Datamining: 
# * http://web.cs.ucdavis.edu/~ma/ECS289H/papers/Inselberg1997.pdf
# * https://syntagmatic.github.io/parallel-coordinates/examples/table.html
################################################################################
