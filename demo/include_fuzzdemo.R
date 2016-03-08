################################################################################
# Description: A Demo of the Package fuzztest (a Work in Progress)
# Author: cloudcell
# Date: 2016-02-29
# License: GPL-3
################################################################################

# This demo defines its own 'user feedback' spots
par(ask=FALSE)

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
    
message("{part 1a}") 
message("The text summary shows that the argument 'option' explains the most") 
message("variability in the outcome. So let's concentrate on the arg. 'option.'")
message("")
message("The detailed test result table, shows that that most failures occur")
message("when option value #3 is selected. At the same time, test log ") 
message("shows that the types of errors are mixed. But for now, let's assume")
message("that fixing the bugs related to control flow is more important. ")
message("")
message("The following three graphs will demonstrate how the same info")
message("can be represented visually. Notice that some lines are grouped")
message("when they intersect the vertical axes. The groups are ordered")
message("from the bottom of the chart to the top: i.e. the fist groupping")
message("of lines at axis 'suboption' (at the bottom) corresponds to")
message("suboption 'a', the next one up is suboption 'b', and so on.")
message("In case an argument has only one value in the test, the whole")
message("group of lines will be evenly spread from the bottom to top of the")
message("graph, as is the case for arguments 'x' and 'y'.")
# message("")
waitForUserInput()
message("{part 1b}") 
message("* Drawing all representations of tests") 
waitForUserInput()
plot_tests()

waitForUserInput()
message("")
message("{part 1c}") 
message("One can also selectively display only passing or failing tests")
message("as will be shown next:")
waitForUserInput()
message("")
message("{part 1d}") 
message("* Drawing only 'passing' representations of tests") 
waitForUserInput()
plot_tests(fail = F)

waitForUserInput()
message("")
message("{part 1e}") 
message("* Drawing only 'failing' representations of tests") 
waitForUserInput()
message("")
plot_tests(pass = F)
}

waitForUserInput()
message("")

message("{part 2a}") 
message("Let's assume all the control flow related bugs are fixed without ")
message("actually going and changing anything in the code. For that ")
message("assumption to work, we will choose a combination of options ")
message("that results in the test outcome 'PASS'. Such a combination ")
message("could be, for example, {x=0, y=0, option='a', suboption='a'}.")

waitForUserInput()
message("")

################################################################################
# NUMERIC TEST
################################################################################
# message("...")
message("{part 2b}")
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
# message("...")
message("Now the first approach will be demonstrated:")
message("(The test consists of 124 cases.)")

waitForUserInput()
message("")

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
    # plot_tests()
    # plot.tests(pass = F)
    # plot.tests(fail = F)
}

message("{part 2c}")
message("Simple test summary shows that argument 'y' contributes to ")
message("failure the most.")
message("")

message("{part 2d}")
message("What about the chart?")
waitForUserInput()
message("")
    plot_tests()
message("Now one can clearly see two linear relationships between 'x' and 'y'")
message("on the graph, which would be hard to deduce from a data table.")
message("These correspond to 'numeric bugs' #1NC and #4NC")
message("(Please, see details in the file 'include_fuzzdemofunc.R')")
message("")

waitForUserInput()
message("")

################################################################################
# FUZZ TESTING
################################################################################
message("{part 3a}")
message("Let's assume the previously discovered bugs have been fixed.")
message("So we will again choose a different combination of input parameters")
message("for arguments 'option' and 'suboption' for the next test.")
message("")

message("The second (random testing) approach will be demonstrated")
message("for numeric arguments only, although character inputs could be ")
message("easily indexed and randomly selected as well (if there were ")
message("arguments requiring character input in this particular demo function.")
message("As for options, it makes no sense randomizing those as all")
message("combinations of provided values will be tested anyway.")
message("")
message("~~v~~^~~v~~^~~v~~^~~v~~^~~v~~^~~v~~^~~v~~^~~v~~^~~v~~^~~v~~")
message("This test has 900 cases and might take a couple of minutes,")
message("so you have time to pour yourself a cup of coffee: (_)]... ")
message("~~^~~v~~^~~v~~^~~v~~^~~v~~^~~v~~^~~v~~^~~v~~^~~v~~^~~v~~^~~")

waitForUserInput()
message("")

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
message("{part 3b}")
message("One can see the following now:")
message("The text output shows that suboption #3 ('c') is always failing.")
message("")
message("Let's see if the visual approach provides a better perspective.")
# message("")

waitForUserInput()
message("")
message(" ... you may have to wait some time for R to draw this plot")
message("     before pressing 'Enter' for the next part of the demo ...")
message("")
plot_tests()
message("... if you don't see a new graph, it is being prepared ...")
waitForUserInput()
message("")

}

message("{part 3c}")
message("This graph has a confusing order of axes.")
message("An axis that has only one option should either be hidden or placed")
message("at an edge of the plot so relations with other parameters could ")
message("be clearly visible. This axis reordering can be done using various")
message("methods; however, for the sake of simplicity, we will create a")
message("'test register' with a different sequence of arguments. ")
message("A different sequence of arguments will effectively change the ")
message("sequence of axes. See the source code of the demo for details.")
message("")

message("{part 4a}")
message("Let's also use a smaller set of parameters to speed up the process.")
message("(The test consists of 100 cases.)")

waitForUserInput()
message("")


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
    
    message("... if you don't see a new graph, it is being prepared ...")
    message("")
    plot_tests()
}

message("{part 4b}")
message("The textual test summary shows the same pattern as in the previous ")
message("test, but the chart still looks confusing. ")
message("To make the plot more clear, again, one can adjust")
message("the 'dist' argument of the plot_tests() function.")
message("")
message("We will try setting it to -0.5: 'plot_tests(dist=-0.5)' to reduce") 
message("space between groups and add more space between lines within groups.")

waitForUserInput()
message("")

if(RUN_CALC) { 
    message("... if you don't see a new graph, it is being prepared ...")
    message("")
    
    plot_tests(dist=-0.5)
    
}
    
waitForUserInput()
message("")
message("{part 4c}")
message("Now, with a reduced parameter set, the test produced a more clear")
message("picture of the combinations of input parameters")
message("without losing important details.")
# message("")
waitForUserInput()
message("")
message("{part 5a}")
message("There are many ways to proceed from here:")
message("* if some 'error' states are valid, exclude them from mests using")
message("  subset argument of apply.argset().")
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
# 1. Min boundary line (almost invisible due to light color)
# 2. all the test lines for those tests that returned the PASS status. They
# 3. all the test lines for FAIL tests.
# 4. Max boundary line (almost invisible due to light color)
# 
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
