################################################################################
# Description: A Demo of the Package fuzztest (a work in progress)
# Author: cloudcello 
# Date: 2016-02-29
################################################################################

################################################################################
# Description of the block of code below the function: 
# This is my initial attempt to build a demo for the package. This will be 
# rebuilt in another file as follows:                                           
#
# 1. Do a simple scan of the demo function with only a few options for
#    each argument
# 2. When an error is identified, simply stop including that combination
#    of argument values in further tests until most 'demo faults' are discovered 
#    There are 10 'demonstration faults' introduced into the demo function
#    
################################################################################

# options: a,b,c
# suboptions: a,b,c,d

################################################################################
# CONTROL FLOW TEST
################################################################################
if(1) {
    set.seed(0)
    r <- list()
    r$x <- c(0)#:5) #c(0)
    r$y <- c(0)#:5) #c(0)
    r$option <- c("a", "b", "c")
    r$suboption <- c("a", "b", "c","d")
    
    generate.argset(arg_register = r, display_progress=TRUE)
    apply.argset(FUN="fuzzdemofunc")
    test_summary()
    plot.tests()
}

message("The summary shows that the argument 'option' explains the most ", 
        "variability in the outcome. So let's concentrate on that.")
message("If we go to the detailed test result table, we will see that the ",
        "failure occurs when option value #3 is selected. If we read the log ", 
        "we will see that the types of errors are mixed. But for now, let's ",
        "think that we fix the ones related to control flow first. ")
message("...")
message("Let's assume we fixed all the control flow related bugs without ",
        "actually going and changing anything in the code. For that ",
        "assumption to work, we will choose a combination of options ", 
        "that resulted in the 'PASS' outcome. Such a combination ",
        "could be, for example, {x=0, y=0, option='a', suboption='a'}. ")

# waitForUserInput()

################################################################################
# NUMERIC TEST
################################################################################
message("...")
message("Now we will concentrate on the numeric part of the test.")
message("There are two main approaches here:")
message(" 1. to generate random numbers within some range and let the ")
message("    test framework make create a set with all possible combinations ")
message("    of those options")
message(" 2. create an evenly spaced sequence of values for each (x and y)")
message("    parameter from lowest to highest and let the framework ")
message("    combine and test these combinations. This second approach has")
message("    has an advantage in that it aligns numeric values with ")
message("    the cardinal number of the group associated with an option")
message("    for each argument. For example, if we create a test sequence")
message("    [-10;+10] with a step of 1 for argument 'x', there will be ")
message("    21 options for argument 'x'. The visualized test results")
message("    will list those from 'Min' to 'Max'. So finding errors ")
message("    will be easier as it will be easier to deduce the dependence ")
message("    of failures on a range of input values from 'x'. ")



if(0) { 
    set.seed(0)
    r <- list()
    r$x <- c(seq(from=-10, to=10, length.out = 21))
    r$y <- c(seq(from=-10, to=10, length.out = 21))
    r$option <- c("a")
    r$suboption <- c("a")
    
    generate.argset(arg_register = r, display_progress=TRUE)
    apply.argset(FUN="fuzzdemofunc")
    test_summary()
    plot.tests()
}

message("    Now one can clearly see two linear relationships between ")
message("    'x' and 'y': ")
message("    1) when y exceeds x by 1 and  ")
message("    2) when y exceeds x by 1 and  ")



################################################################################
# Notes:
# 
# Vertical axes are equally split among options; in case there is only one
# option occupies the axis "test lines" will be evenly spread from the bottom
# to the top of the chart.
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
