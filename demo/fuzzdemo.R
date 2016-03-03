################################################################################
# Description: A Demo of the Package fuzztest (a Work in Progress)
# Author: cloudcell
# Date: 2016-02-29
# License: GPL-3
################################################################################

# options: a,b,c
# suboptions: a,b,c,d

message("Demo of the fuzztest package") 

waitForUserInput()

################################################################################
# CONTROL FLOW TEST
################################################################################
if(1) {
    set.seed(0)
    r <- list()
    r$x <- c(0)
    r$y <- c(0)
    r$option <- c("a", "b", "c")
    r$suboption <- c("a", "b", "c","d")
    
    generate.argset(arg_register = r, display_progress=TRUE)
    apply.argset(FUN="fuzzdemofunc")
    test_summary()
    plot_tests()
    # plot.tests(pass = F)
    # plot.tests(fail = F)
}

waitForUserInput()

message("The summary shows that the argument 'option' explains the most ") 
message("variability in the outcome. So let's concentrate on that.")
message("If we go to the detailed test result table, we will see that most")
message("failures occur when option value #3 is selected. If we read the log ") 
message("we will see that the types of errors are mixed. But for now, let's ")
message("think that we decided to fix the bugs related to control flow first. ")
message("...")
message("Let's assume we fixed all the control flow related bugs without ")
message("actually going and changing anything in the code. For that ")
message("assumption to work, we will choose a combination of options ")
message("that resulted in the 'PASS' outcome. Such a combination ")
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
message(" 2. generate random numbers within some range and let the ")
message("    test framework generate a data set of all possible combinations")
message("    of those options.")
message("...")
message("Now the first approach will be demonstrated:")

waitForUserInput()

if(1) { 
    set.seed(0)
    r <- list()
    r$x <- c(seq(from=-10, to=10, length.out = 21))
    r$y <- c(seq(from=-10, to=10, length.out = 21))
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

message("    Now one can clearly see two linear relationships between ")
message("    'x' and 'y'. These correspond to 'numeric bugs' #1NC and 4NC")
message("    'x' and 'y'. These correspond to 'numeric bugs' #1NC and 4NC")
message("...")
message("Let's assume those are fixed now.")

waitForUserInput()

################################################################################
# FUZZ TESTING
################################################################################
message("...")
message("Now the second (random testing) approach will be demonstrated")
message("for numeric arguments only, although character inputs could be ")
message("easily indexed and randomly selected as well.")

waitForUserInput()

if(1) { 
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
message("Those axes that have only one option ('level') should not be plotted")
message("or placed at the left(/right)most side of the parallel plot. This")
message("reordering can simply be accomplished by creating a test register")
message("in a different sequence (or tweaking the internals of the ")
message("plotting function of this package). Let's simply reorder and use")
message("a smaller dataset to speed up testing.")
waitForUserInput()


if(1) { 
    set.seed(0)
    r <- list()
    r$x <- runif(5, min=-10, max=10)
    r$y <- runif(5, min=-10, max=10)
    r$suboption <- c("a","b","c","d")
    r$option <- c("b")
    
    generate.argset(arg_register = r, display_progress=TRUE)
    apply.argset(FUN="fuzzdemofunc")
    test_summary()
    plot_tests()
}

waitForUserInput()
message("Now it's time to eliminate each bug one by one.")



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
