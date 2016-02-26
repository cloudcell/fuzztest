exfun <- function(operation, output_type, data1, data2)
{

    switch(operation,
           "add"={

               result <- data1 + data2

               switch(output_type,
                      "list"={
                          result <- as.list(result)
                      },
                      "data.frame"={
                          result <- as.data.frame(result)
                      },
                      {
                          stop("Wrong output type")
                      }
               )
           },
           "mult"={

               result <- data1 * data2

               switch(output_type,
                      "list"={
                          result <- as.list(result)
                      },
                      "data.frame"={
                          result <- as.data.frame(result)
                      },
                      {
                          stop("Wrong output type")
                      }
               )
           },
           {
               stop("Wrong setting")
           }
    )
    result
}



exfun("mult","data.frame",c(1,2,3),c(4,5,6))
exfun("add","list",c(1,2,3),c(4,5,6))
exfun("add","data.frame",c(1,2,3),c(4,5,6))


# take the last item of a vector and

demosubf <- function(x,y)
{
    # imitation of some illegal parameters to some arbitrary function
    if(abs(x-y)<0.001) stop("demo bug")
    result <- x^2+y*2
    result
}

# options: a,b,c,d,e,f,g,h,i
# suboptions: a,b,c,d
demofunc <- function(x, y, option, suboption)
{

    switch(option,
           "a"={
               switch(suboption,
                      "a"={           },
                      "b"={           },
                      "c"={     stop("demo bug")      },
                      "d"={           },
                      { stop("Wrong suboption") }
               )

           },
           "b"={
               switch(suboption,
                      "a"={           },
                      "b"={ x <- y    },
                      "c"={           },
                      "d"={           },
                      { stop("Wrong suboption") }
               )
               },
           "c"={
               switch(suboption,
                      "a"={           },
                      "b"={ x <- x-1  },
                      "c"={           },
                      "d"={           },
                      { stop("Wrong suboption") }
               )
               },
           "d"={
               switch(suboption,
                      "a"={           },
                      "b"={           },
                      "c"={           },
                      "d"={           },
                      { stop("Wrong suboption") }
               )
               },
           "e"={
               x <- 1
               switch(suboption,
                      "a"={           },
                      "b"={     x <- x+1      },
                      "c"={           },
                      "d"={     y <- 1      },
                      { stop("Wrong suboption") }
               )
               },
           "f"={
               switch(suboption,
                      "a"={           },
                      "b"={           },
                      "c"={     stop("demo bug")       },
                      "d"={           },
                      { stop("Wrong suboption") }
               )
               },
           "g"={
               switch(suboption,
                      "a"={     y <- y+2      },
                      "b"={           },
                      "c"={           },
                      "d"={           },
                      { stop("Wrong suboption") }
               )
               x <- x^2
               },
           "h"={
               switch(suboption,
                      "a"={           },
                      "b"={     stop("demo bug")       },
                      "c"={           },
                      "d"={           },
                      { stop("Wrong suboption") }
               )
               y <- y^2
               },
           "i"={
               switch(suboption,
                      "a"={           },
                      "b"={           },
                      "c"={           },
                      "d"={           },
                      { stop("Wrong suboption") }
               )
               },
           { stop("Wrong option") }
    )
    result <- demosubf(x,y)

    result
}


# Description of the demo:
# This example might seem a bit contrived. Nevertheless, it fulfills its purpose, 
# which  is to demonstrate how to test functions with a long list of options

# This package is just a framework one can extend as one sees fit during testing.

# TODO: add to the description of the package: the package does not try to
#       hide the testing environment object to make the process more
#       transparent (be sure not to delete it unless you really mean it).

if(0) { # test for the Wiki

    set.seed(0)

    r <- list()
    r$x <- as.list(runif(3))
    r$y <- as.list(runif(3))
    r$option <- c("a","b","c","d","e","f","g","h","i")
    r$suboption <- c("a","b","c","d")

    # TODO: prepare and store an argument test set in a separate environment
    # .stresstest.env or '.stress'
    generate.argset(arg_register = r)

    # produce results {PASS,FAIL} for every argument test set
    apply.argset(FUN="demofunc") # , subset=c(1,5,222,333,444,555,666,777,888,999,41472)

    # print test summary
    test_summary()

    plot.tests()
}

if(0) { # test for testing the package itself

    set.seed(0)
    r <- list()
    r$x <- runif(2)
    r$y <- runif(2)
    r$option <- c("a","b")
    # r$option <- c("break","breako")
    # r$suboption <- list(c("a","b","c","d"))
    r$suboption <- c("c","d")
    # r$suboption <- list(c("break", "break"))

    # TODO: prepare and store an argument test set in a separate environment
    # .stresstest.env or '.stress'
    generate.argset(arg_register = r)

    # produce results {PASS,FAIL} for every argument test set
    apply.argset(FUN="demofunc") # , subset=c(1,5,222,333,444,555,666,777,888,999,41472)

    # print test summary
    test_summary(DEBUG = FALSE)

    # plot.tests(DEBUG=TRUE,verbose=TRUE)
    # plot.tests(DEBUG=TRUE)
    plot.tests(DEBUG=FALSE,verbose=TRUE)

    # demofunc(x=0,y=10,"a","break")
}





