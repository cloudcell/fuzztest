################################################################################
# Note: This is still a work in progress (contributors are welcome!)
# 
# Description: This is a dummy function to used in demos and unit tests
#              There are 8 'demonstration faults' in this demo function
#              * 4 related to control flow
#              * 4 related to numeric calculations
#              with some bugs having a mixed type
#
#              This classification is a bit contrived; however, the number of
#              tests needed to run the tests for each "bug" type varies by 
#              orders of magnitude, so for the purpose of demos in this package,
#              they shall be classified as such.
# 
# Author: cloudcello
# 
# Date: 2016-02-29
################################################################################

#' @export
waitForUserInput <- function() 
{ 
    message("press Enter when ready")
    invisible(readline()) 
}



#' Generates errors for several combinations of input parameters to test the
#' existing and emerging functionality of the package
#'
#' Whenever options lead the control flow within a function to a 'demo bug', 
#' the function stops and the test framework records a 'FAIL' result.
#' Upon a successful completion, the function returns a numeric value into the 
#' environment from which the function was called.
#'
#' @param x: any numeric scalar value (non-vector)
#' @param y: any numeric scalar value (non-vector)
#' @param option any character value from "a", "b", "c"
#' @param suboption any character value from "a", "b", "c", "d"
#' 
#' @author cloudcello
#' 
#' @export
fuzzdemofunc <- function(x, y, option, suboption)
{
    tmp1 <- 0
    switch(option,
           "a"={
               switch(suboption,
                      "a"={                                    },
                      "b"={                                    },
                      "c"={ if(x + y <0) stop("[control flow] demo bug #1CF") },
                      "d"={                                    },
                      { stop("Wrong suboption (valid 'FAIL')") }
               )
               if(abs(x-y+1)<0.1) stop("[numeric calc.] demo bug #1NC")
           },
           "b"={
               x <- 1
               switch(suboption,
                      "a"={ x <- x*1.5                         },
                      "b"={ x <- y                             },
                      "c"={ y <- 1                             },
                      "d"={ if(x>y) stop("[control flow] demo bug #2CF") },
                      { stop("Wrong suboption (valid 'FAIL')") }
               )
               if(abs(x %% 5 - y)<0.01) stop("[numeric calc.] demo bug #2NC")
           },
           "c"={
               switch(suboption,
                      "a"={ stop("[control flow] demo bug #3CF") },
                      "b"={                                    },
                      "c"={                                    },
                      "d"={  rm(tmp1)                          },
                      { stop("Wrong suboption (valid 'FAIL')") }
               )
               if(abs(x %/% 5 - y)<0.01) stop("[numeric calc.] demo bug #3NC")
           },
           { stop("Wrong option (valid 'FAIL')") }
    )
    
    if(!exists("tmp1")) stop("[control flow] demo bug #4CF")
    
    result <- x - y*2 + 10
    
    if(abs(result)<0.1) stop("[numeric calc.] demo bug #4NC")
    
    result
}

