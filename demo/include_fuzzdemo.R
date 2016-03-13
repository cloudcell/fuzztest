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
message("The detailed statistics table shows that most failures occur when")
message("value #3 is selected within the argument 'option'. At the same time,")
message("a test log (omitted here) shows that the types of errors are mixed.")
message("For now, however, let's assume that fixing the bugs related to")
message("control flow is more important.")
message("")
message("The following three graphs will demonstrate how the data above")
message("can be represented visually. Notice that some lines are grouped")
message("when they intersect vertical axes. The groups correspond to specific")
message("options and are ordered from the bottom of the chart to the top: ")
message("i.e. the fist grouping of lines at axis 'suboption' (at the bottom)")
message("corresponds to value 'a', the next one up is suboption 'b', and so on.")
message("In case an argument has only one value in the test, the whole")
message("group of lines will be evenly spread from the bottom to the top of ")
message("the chart, as is the case for arguments 'x' and 'y'.")
# message("")
waitForUserInput()
message("{part 1b}") 
message("* All test cases") 
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
message("* Only 'passing' test cases:") 
waitForUserInput()
plot_tests(fail = F)

waitForUserInput()
message("")
message("{part 1e}") 
message("* Only 'failing' test cases:") 
waitForUserInput()
message("")
plot_tests(pass = F)
}

waitForUserInput()
message("")

message("{part 2a}") 
message("Let's assume all the control flow related bugs discussed above ")
message("are fixed now. To make this assumption to 'work' during testing")
message("we will simply choose a combination of options that will not ")
message("cause the demo function to produce 'fail' states shown above. ")
message("Such a combination could be {x=0, y=0, option='a', suboption='a'}.")

waitForUserInput()
message("")

################################################################################
# NUMERIC TEST
################################################################################
# message("...")
message("{part 2b}")
message("Now we will concentrate on the numeric part of the test.")
message("There are two main testing approaches:")
message(" 1. Create an evenly spaced sequence of values for each parameter ")
message("    (x and y) from lowest to highest and let the argument set generator")
message("    combine and test these values. This approach has an advantage ")
message("    for more intuitive visualization as sequences of values")
message("    for testing will be aligned with the vertical axis. For example, ")
message("    if we create a test sequence [-10;+10] for argument 'x', ")
message("    visualized test results will list those from 'Min' to 'Max'. ")
message("    So finding simple linear dependencies that cause errors will ")
message("    be easier as it will be easier than when using a random set")
message("    of values (below).")
message(" 2. Generate random parameters for selected arguments and let the test")
message("    framework test all possible parameter combinations.")
message("")
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
message("The test summary shows that argument 'y' contributes to ")
message("failure the most.")
message("")

message("{part 2d}")
message("What about the chart?")
waitForUserInput()
message("")
message(" ... you may have to wait some time for R to draw this plot")
message("     before pressing 'Enter' for the next part of the demo ...")
plot_tests()
message("")
message("... if you don't see a new graph, it is being prepared ...")
waitForUserInput()
message("")
message("Now one can clearly see two linear relationships between")
message("'x' and 'y'. These correspond to 'numeric bugs' #1NC and #4NC")
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
message("")

message("The Second Approach: Random Test Sequences")
message("")
message("It makes no sense testing options randomly as all those ")
message("combinations of values will be tested anyway. So the test ")
message("will be conducted for numeric arguments only.")
message("")
message("~~v~~^~~v~~^~~v~~^~~v~~^~~v~~^~~v~~^~~v~~^~~v~~^~~v~~^~~v~~")
message("  This test has 900 cases and might take a minute, so you  ")
message("  have time to pour yourself a cup of coffee: (_)]...      ")
message("~~^~~v~~^~~v~~^~~v~~^~~v~~^~~v~~^~~v~~^~~v~~^~~v~~^~~v~~^~~")
message("")

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
message("The detailed test table shows that suboption #3 ('c') always fails.")
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
message("This graph has a confusing order of axes at this point.")
message("An axis that has only one option should either be hidden or placed")
message("at an edge of the chart so relations with other parameters could")
message("be visible. To reorder axes, for simplicity, we will quickly create ")
message("a smaller test with a different sequence of arguments, which will ")
message("change the sequence of axes.")
message("")

message("{part 4a}")
message("The test consists of only 100 cases.")

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
message("test. Also, a reduced set of test cases produced a more transparent")
message("representation of test results without losing important details.")
waitForUserInput()
message("")

message("{part 5a}")
message("There are many ways to proceed from here:")
message("* if some 'error' states are valid, one can exclude them from tests ")
message("  using the 'subset' argument of apply.argset().")
message("* if bugs are trivial, one can eliminate them one by one.")
message("* if faults are intractable, one can start with narrowing down the")
message("  range of input parameters and further analyze function behavior.")
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
