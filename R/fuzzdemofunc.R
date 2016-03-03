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
# Author: cloudcell
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
#' @author cloudcell
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
                      "c"={ if(x + y <0) stop("demo bug #1CF (control flow)") },
                      "d"={                                    },
                      { stop("Wrong suboption (valid 'FAIL')") }
               )
               if(abs(x-y+1)<0.01) stop("demo bug #1NC (numeric calc.)")
           },
           "b"={
               x <- 1
               switch(suboption,
                      "a"={ x <- x*1.5                         },
                      "b"={ x <- y                             },
                      "c"={ y <- 1                             },
                      "d"={ if(x>y) stop("demo bug #2CF (control flow)") },
                      { stop("Wrong suboption (valid 'FAIL')") }
               )
               if(abs(x %% 5 - y)<0.01) stop("demo bug #2NC (numeric calc.)")
           },
           "c"={
               switch(suboption,
                      "a"={ stop("demo bug #3CF (control flow)") },
                      "b"={                                    },
                      "c"={                                    },
                      "d"={  rm(tmp1)                          },
                      { stop("Wrong suboption (valid 'FAIL')") }
               )
               if(abs(x %/% 5 - y)<0.01) stop("demo bug #3NC  (numeric calc.)")
           },
           { stop("Wrong option (valid 'FAIL')") }
    )
    
    if(!exists("tmp1")) stop("demo bug #4CF (control flow)")
    
    result <- x - y*2 + 5
    
    if(abs(result)<0.01) stop("demo bug #4NC  (numeric calc.)")
    
    result
}

