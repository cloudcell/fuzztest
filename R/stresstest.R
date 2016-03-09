############################################################################## #
# Package: "fuzztest"
# Test functions aimed at improving the quality of R code
#------------------------------------------------------------------------------#
# Description: a set of functions to verify that none of the combinations 
#              of input arguments causes a function undergoing stress/fuzz test
#              to produce an invalid error state
#
# Author: cloudcell
# Date: 2016-02-19
# License: GPL-3
# Licensing Reference: http://choosealicense.com/
############################################################################## #

# TODO: use my assertion code templates to assure argument register is valid
#       before processing it ! (or drawing anything based on it)

# TODOs (in the order of priority) ----
# A. start using the default environment named '.stress'
# B. allow a test to run as a separate process (so the main process is not
#    affected by crashes)
# C. start using foreach() to speed up the test
#
# 1. consider using variable names as character strings and numeric values:
#    variable names as strings could be displayed more easily
#    during assignment phase, they could simply be stripped of technical tags
#    to be used with 'assign()' i.e. __var.pr would mean that
#    a variable 'pr' will be used during assignment
#    At the same time, displaying analytics will be much easier as no special
#    treatment will be required
# 3. display results using this: http://www.milbo.org/rpart-plot/prp.pdf
# 4. print structures such that indentation clearly represents nesting levels
#    ref: http://stackoverflow.com/questions/1970653/generating-textual-representation-of-directory-contents
# 5. try adjusting (nesting level) indentation within str.xts()
# 6. consider this alternative: https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html#tree-creation


# makes an index and  calculates the product of all options
getComboQty <- function(register, verbose=TRUE)
{
    # if(class(register) != "data.frame") stop ("Argument 'register' must be of 'data.frame' class.")
    if(class(register) != "list") stop ("Argument 'register' must be of 'list' class.")
    if(verbose) print(register)

    if(verbose) message("----------------------------------------\n")
    if(verbose) message(" arg_id\t:  qty \t:       arg_name\n")
    if(verbose) message("----------------------------------------\n")

    # use a handy variable name:
    # no duplication in R unless the new variable is modified
    r=register

    result <- data.frame(arg_id=integer(),
                         qty=integer(),
                         row.names=NULL, stringsAsFactors = FALSE) # NULL
    # names(result)
    accum <- 0L
    for(i in 1:length(r)){
        if(verbose)
            message(paste0( "  ", i, "\t:   ", length(r[[i]]), "\t: ", names(r[i]) ) )
        current_length <- length(r[[i]])

        if(i==1){
            accum=current_length
        } else {
            accum=accum*current_length
        }
        # if(nrow(result)==0) {
        #     result <- as.data.frame(arg_id=i,qty=length(r[[i]]))
        # } else {
        result <- rbind(result,list(arg_id=i,qty=length(r[[i]])), deparse.level=0 )
        # }
    }
    # names(result) <- NULL
    if(verbose) message("----------------------------------------")
    if(verbose) message(paste0("The total number of combinations ==", accum, ""))
    accum
    args_qty <- length(r)
    out <- list(total_qty=accum,idx=result, args_qty=args_qty)
    out
}


#------------------------------------------------------------------------------#
#
# -- generate.argset(arg_register)
# ---- [output value typed 'argset']
# [the output value (which is an environment which acts as a container)
# has type "argset"]
#
# -- apply.argset()
# ---- [input value typed 'argset']
# [acts in the same manner as apply.paramset, distributing work and combining
#  results using parfor]
#------------------------------------------------------------------------------#

#' Generates a set of all the combinations of arguments (argset) to be used
#' to test a selected function
#' 
#' This function uses an argument 'register' created earlier and generates all
#' possible combiations of arguments, including argument 'states', such as
#' 'missing' specified as '__MISSING__' in the register. The result is stored
#' in the work environment. There is no special function to create an
#' argument register to be used with 'generate.argset()' as the register is
#' simply a list. Please see file 'fuzzdemo.R' for an example. 
#' At the moment the following character strings are interchangeable when
#' setting up the test (the argument register):
#' "__val.NULL__" and "NULL", 
#' "__val.NA__" and "NA",
#' "__val.TRUE__" and "TRUE",
#' "__val.FALSE__" and "FALSE".
#' If you would like to actually assign character strings such as "NULL", "NA",
#' "TRUE", "FALSE", simply override the function "prepareArgs" with your
#' modified version.
#' 
#' @param arg_register an argument register that contains all argument 
#'                     'states' required to be tested (while combined with 
#'                     'states' of other arguments)
#' @param cust.env custom work environment
#' @param verbose provides additional text output during processing
#' @param DEBUG enters the debug mode on function entry
#' @param display_progress prints "." on each iteration in the standard output
#' 
#' @author cloudcell
#' 
#' @export
generate.argset <- function(arg_register, cust.env=NULL, verbose=FALSE,
                            DEBUG=FALSE, display_progress=FALSE)
{
    # TODO: assert that inputs are of the correct type (on the first level)
    # 
    browser(expr = DEBUG)

    message(rep("-",70))
    message("Generating argset")
    # message(rep("-",70))

    # create a container
    # with meta data explicitly stated:
    # capacity (number of possible records)
    # next available slot within the matrix

    if(!is.null(cust.env)){
        if(!inherits(cust.env, what = "environment"))
            stop("Custom environment 'cust.env' must be of class 'environment'.")
        e=cust.env # if a custom environment has been provided
    } else {
        e <- new.env() # storage.env
    }

    # save the register
    e$arg_register <- arg_register

    combos <- getComboQty(register=arg_register, verbose=FALSE)
    combos$args_qty
    combos$total_qty

    # reserve memory
    e$container_test_args <- matrix(nrow=combos$total_qty, 
                                    ncol = combos$args_qty)
    
    e$container_test_results <- vector( length = combos$total_qty, 
                                        mode = "character")

    e$result_slot_next=1
    e$result_slot_max=combos$total_qty

    if(verbose) str(e$container_test_args)
    if(verbose) str(e$container_test_results)
    e$result_slot_next
    e$result_slot_max

    # for use when actual param combos are put into the container environment
    e$display_progress <- display_progress
    get_leafs(idx=combos$idx,storage.env = e)
    if(!is.null(e$result_slot_next)) stop ("not all test combos have been generated.")

    # message(rep("-",70))
    message("Result: SUCCESS.")
    message("Returning test combinations of arguments within an environment")
    message(rep("-",70))
    message("The following objects are available within the testing environment:")
    print(ls(envir = e))

    # create an environment only if no custom env. was provided
    if(is.null(cust.env)) {
        .GlobalEnv$cont.env <- e
    }
}


# TODO: replace 'cat' with message/warning/stop
get_leafs <- function(idx, start_branch_id=1, accum_leafs=c(),
                      storage.env=NULL, verbose=FALSE, DEBUG=FALSE)
{
    browser(expr=DEBUG)
    if(start_branch_id>nrow(idx)) stop("Wrong branch number: id out of range.")

    # get the total quantity of 'branches'
    branch_qty <- nrow(idx)

    # rename internally for simplicity
    i <- start_branch_id
    if(verbose) message(paste0("_branch_id_ == ",i,""))

    # get total 'leafs' on this branch
    leaf_qty <- idx[i,"qty"]

    for( j in 1:leaf_qty ) {
        if(verbose) message(paste0("_leaf_id_# == ",j,""))

        if(i+1<=branch_qty) {
            # jump to a branch "up" (with a higher id)
            # from every 'leaf' on this branch
            get_leafs(idx, start_branch_id = i+1, accum_leafs=c(accum_leafs,j), 
                      storage.env = storage.env)
        } else {
            # spew out all the leafs
            printable_leafs=c(accum_leafs,j)
            if(verbose) print(printable_leafs) #TODO: use 'verbose' later

            # TODO: store the leaf set in a specially prepared environment
            # just use function's environment one level up (not 'frame' !!!)
            if(!is.null(storage.env)) {
                store_test_set(env = storage.env,test_set = printable_leafs, 
                               display_progress = storage.env$display_progress)
            }
        }
    }
}

# store an indivisual test set (one argument set for one test)
# (service function)
store_test_set <- function(env=stop("storage environment must be provided"),
                           test_set=stop("test set must be provided"),
                           display_progress=FALSE)
{
    e <- env
    ls(envir = e)

    if( is.null(e$result_slot_next) )
        stop("Data store does not have any more preallocated memory for storage.")

    i <- e$result_slot_next
    e$container_test_args[i,] <- test_set # TODO: check size !
    e$result_slot_next= i+1

    if(display_progress) cat(".")
    # cat(".")

    # mark the 'index' as unusable
    if(e$result_slot_next>e$result_slot_max) { e$result_slot_next <- NULL }
}


# prepare a single combination of arguments
prepareArgs <- function(arg_register, arg_selection_vector, verbose=FALSE, DEBUG=FALSE)
{
    browser(expr = DEBUG)
    if(DEBUG) { verbose = TRUE }

    # switch to more convenient (to me) internal variables
    r <- arg_register
    arg_ids.vct <- arg_selection_vector

    ## Form args for running the 'error' test
    final_arg <- list()
    final_arg

    # suppressMessages(verbose==FALSE)

    for(i in 1:length(r)) {
        
        if(verbose) message(rep("-",70))
        if(verbose) message("Argument number: ", i)

        arg_name <- names(r[i])
        if(verbose) message("Argument name: '", arg_name, "'" )
        if(verbose) message("Option choice number for argument id ", i, ": ", arg_ids.vct[i])

        choice <- arg_ids.vct[i]
        arg_value <- r[[i]][[choice]]

        if(verbose) message("Argument value chosen: '", arg_value, "'")
        if(verbose) print("str(arg_value):")
        if(verbose) print(str(arg_value))

        # http://stackoverflow.com/questions/27491637/r-switch-statement-with-varying-outputs-throwing-error/27491753#27491753
        # choose b/n numeric vs character version
        switch_value <- arg_value

        #----------------------------------------------------------------------#
        # R types 
        #----------------------------------------------------------------------#
        # 'mode' E {numeric, character, list, function}
        # 'typeof' -- usually the same info as 'mode(storage.mode)'
        # 'class' E {abstract type}
        #----------------------------------------------------------------------#
        # 'typeof' returns the type of an R object
        # 'mode'   returns the type of an S object
        #          i.e. gives information about the mode of an object in the
        #          sense of Becker, Chambers & Wilks (1988), and is more
        #          compatible with other implementations of the S language
        # Source: http://stackoverflow.com/questions/6258004/r-types-and-classes-of-variables
        #----------------------------------------------------------------------#

        if(mode(arg_value)!="character") {
            # so numeric, list, and function are assigned as is
            switch_value <- "DEFAULT" # the "AS IS" assignment
        }

        # dealing with character types (with the non-character mode objects
        # fall through to the 'bottom':
        switch(switch_value,
               #TODO: consider deprecating the use of "NULL" to mean NULL
               "__val.NULL__"=,
               "NULL"={
                   # ref: http://stackoverflow.com/questions/7944809/assigning-null-to-a-list-element-in-r
                   if(verbose) print(paste0("Arg: ", arg_name," : assigning NULL") )
                   # ATTENTION: using '[' and NOT '[[' !
                   final_arg[arg_name]=list(NULL) 
               },
               #TODO: consider deprecating the use of "NA" to mean NA
               "__val.NA__"=,
               "NA"={
                   if(verbose) print(paste0("Arg: ", arg_name," : assigning NA") )
                   final_arg[[arg_name]]=NA
               },
               #TODO: consider using only "__val.TRUE__" (deprecate the other)
               "__val.TRUE__"=,
               "TRUE"={
                   if(verbose) print(paste0("Arg: ", arg_name," : assigning TRUE") )
                   final_arg[[arg_name]]=TRUE
               },
               #TODO: consider using only "__val.FALSE__" (deprecate the other)
               "__val.FALSE__"=,
               "FALSE"={
                   if(verbose) print(paste0("Arg: ", arg_name," : assigning FALSE") )
                   final_arg[[arg_name]]=FALSE
               },
               '__MISSING__'={
                   if(verbose) print(paste0("Arg: ", arg_name," : skipping assigment") )
                   # final_arg[[arg_name]] <- NULL
               },
               # DEFAULT:
               # use get() or assign()
               # final_arg[[arg_name]] <- unlist(arg_value)
               {
                   if(verbose) print(paste0("Arg: ", arg_name," : assigning value AS IS") )
                   final_arg[[arg_name]] <- arg_value
               }
        )
        # if any is equal to "__MISSING__" then NULL it !
        if(verbose) message("str(final_arg) after this iteration:")
        if(verbose) print(str(final_arg))
    }
    str(final_arg)

    final_arg
}

## Function Description:
## tweak this function to produce PASS/FAIL result based on any condition
#
# the actual testing function (handles FUN and args) and catches exceptions
errorHandlingTest <- function(FUN,args)
{
    # rc <- try(do.call(what = FUN,args=args))
    # if(inherits(x = rc,what = "try-error")) {
    #     result <- "FAIL"
    # } else {
    #     result <- "PASS"
    # }

    #--------------------------------------------------------------------------#
    # preliminaries
    
    eval_result=NULL
    # eval_value=NULL
    # package evaluate:: output handler
    oh <- new_output_handler(
        # messages, warning, value, etc. handlers can be added
        error=function(x)
        {
            # evaluate:::identity
            eval_result<<-"FAIL"
        }
    )#, value=function(x){eval_value<<-})
    
    #--------------------------------------------------------------------------#
    # test
    # message(paste0("before ",eval_result))
    
    res <- evaluate::evaluate("do.call(what=FUN, args=args)", 
                              output_handler = oh, 
                              new_device = FALSE,
                              debug=FALSE) #saves to much data if true
    # res
    
    if(is.null(eval_result)) { eval_result <- "PASS"}
    
    # message(paste0("after ",eval_result))
    
    # purely log-related code
    output_captured <- capture.output( dump(list = "res", file="") )
    
    # cat(output_captured)
    # 
    # output_captured
    # replay(output_captured)
    # 
    # cat(output_captured)
    # zz <- cat(paste(output_captured, collapse = ""))
    log_msg <- paste(output_captured, collapse = "")
    # replay(zz)
    
    # ls_loggers()
    # new_logger("somename.log")
    lmessage(log_msg, verbose=FALSE)
    
    # replay(paste0(dump(list = "fuzztest_res", file=""), collapse = ""))
    
    
    #--------------------------------------------------------------------------#
    # save log
    
    #--------------------------------------------------------------------------#
    eval_result
}


#' tests a function based on a set of parameters
#' 
#' tests a function provided to the argument FUN with argument value sets
#' prepared earlier and stored in the default work environment and returns 
#' a vector with PASS/FAIL for every combination of input parameters.
#' 
#' An alternative work environment may be supplied in the future 
#' (not tested yet)
#' 
#' @param env work environment (if NULL, uses the default)
#' @param arg_register an argument register that contains all argument 'states' 
#' @param FUN a name of the function to be tested (only as a character string)
#' @param subset a vector with numbers of argsets to be applied to the function
#'        (i.e. only combinations with those numbers will be tested)
#' @param verbose provides additional text output during processing
#' @param DEBUG enters the debug mode on function entry
#' 
#' @author cloudcell
#' 
#' @export
apply.argset <- function(env=NULL, arg_register=cont.env$arg_register,
                         FUN, subset=NULL, verbose=FALSE, DEBUG=FALSE)
{
    browser(expr = DEBUG)
    
    #--------------------------------------------------------------------------#
    # libraries
    require(evaluate) # in the test function invoked from apply.argset
    
    #--------------------------------------------------------------------------#
    # preliminaries
        
    if(DEBUG) { verbose = TRUE }

    if(mode(FUN)!="character")
        stop (paste0("Wrong argument FUN: please, provide a character ",
                     "string naming the function to be called"))

    if(!exists(FUN)) stop("Function ",FUN," does not exist")

    if(!is.function(get(FUN))) {
        stop("Supplied function name \"", FUN,
             "\" does not correspond to an existing function.")
    }
    
    if(is.null(env)) {
        cont.env <- .GlobalEnv$cont.env
    } else {
        cont.env <- env
    }
    
    #--------------------------------------------------------------------------#
    # output-related variables
    
    base_fname <- paste0("StressTest_", FUN, "_", gsub("[\\ :]","-",Sys.time()))
    env_fname <- paste0(base_fname, ".RData")
    log_fname <- paste0(base_fname, ".log")
    
    #--------------------------------------------------------------------------#
    # create a logger
    
    if(verbose) message("apply.argset(): deleting an old logger")
    try(rm_logger(silent=TRUE)) # flush & delete default logger w/o warnings
    
    if(verbose) message("apply.argset(): creating a logger")
    new_logger(log_fname) # with a default logger 'handle'

    #--------------------------------------------------------------------------#
    # start testing
    
    r=arg_register # to be able to run function code "in the global env."

    # store the register in the test container environment
    cont.env$r <- arg_register
    #
    # cont.env <- getTestParamIds(register = r)
    # ls(envir = cont.env)
    cont.env$container_test_args

    # arg_ids.vct <- cont.env$container_test_args[10000,]
    # store_test_set(env = )
    # loop thru all the test arg. set

    if(is.null(subset)){
        selected_argset <- 1:cont.env$result_slot_max
    } else {
        # TODO: assert that subset is a numeric integer vector
        # TODO: make an  assertion function and create a 'compound' notion
        #       of type that satisfy multiple notions {class, attributes,
        #       size{vector, scalar}, etc.}
        #       Make boilerplate assertion block at the beginning of
        #       important functions. Pass all arguments to a special
        #       environment in which type checking is performed
        #       - OR -
        #       use R6
        selected_argset <- subset
    }
    
    lmessage(paste(rep("-",70),collapse=""))
    for(i in selected_argset) { #XXX: weird R behavior, when selected_argset is undefined, i==TRUE :(?
        
        lmessage(paste0("STRESSTEST: Argument Combination ID ", i))
        
        arg_ids.vct <- cont.env$container_test_args[i,]

        # prepare a single set of arguments for testing
        final_arg <- prepareArgs(arg_register = r, arg_selection_vector = arg_ids.vct, DEBUG=DEBUG)

        # test error crash/handling
        result <- errorHandlingTest(FUN=FUN, args = final_arg)
        
        lmessage(paste0("Result: ", result))
        lmessage(paste(rep("-",70),collapse=""))

        cont.env$container_test_results[i] <- result
    }

    lmessage("Test results were saved in the test environment in 'container_test_results'")
    print(ls(envir = cont.env))

    # cont.env$container_test_results # throw the results out (a la 'foreach')

    #--------------------------------------------------------------------------#
    # saving test 'container' environment

    # TODO: use a variable for the name of a tested function
    # env_fname <- paste0("StressTest_", FUN, "_", gsub("[\\ :]","-",Sys.time()), ".RData")
    # env_fname <- paste0(base_fname, ".RData")
    # save(list="bound_test_data", envir = cont.env, file = fname)
    # TODO: ASAP: rename cont.env() to .stress and create such an environment
    # if any custom env. was used (rename within the function that saves data)
    save(list="cont.env", envir = cont.env, file = env_fname)
    # lmessage(paste0("Test data was saved in the work directory ", getwd(), " as ", env_fname))
    lmessage(paste0("Test data was saved in the work directory ", getwd(), " as ", env_fname))

    #--------------------------------------------------------------------------#
    # deal with the logger
    rm_logger() # flush & delete default logger
}


#---sandbox--------------------------------------------------------------------#

if(0) { # the main test

    ## set up test for a function

    # set up an 'argument register' with all the required test values
    if(0) {
        r <- list()
        r$R                = list( pr ) # TODO: variable name as character string ?
        r$p                = list( 0.95, "__MISSING__" )
        r$method           = list( "modified", "gaussian", "historical", "__MISSING__" )
        r$clean            = list( "none", "boudt", "geltner", "__MISSING__" )
        r$portfolio_method = list( "single", "component", "__MISSING__" )
        r$weights          = list( "NULL", c(1.0), "__MISSING__" )
        r$mu               = list( "NULL", "__MISSING__" )
        r$sigma            = list( "NULL", "__MISSING__" )
        r$m3               = list( "NULL", "__MISSING__" )
        r$m4               = list( "NULL", "__MISSING__" )
        r$invert           = list( "TRUE", "FALSE", "__MISSING__" )
        r$operational      = list( "TRUE", "FALSE", "__MISSING__" )
        str(r)
    }


    # set up an 'argument register' with all the required test values
    r <- list()
    r$R                = list( pr2 ) # TODO: variable name as character string ?
    # r$p                = list( 0.95, "__MISSING__" )
    r$method           = list( "modified", "gaussian", "historical", "kernel", "__MISSING__" )
    r$clean            = list( "none", "boudt", "geltner", "__MISSING__" )
    r$portfolio_method = list( "single", "component", "__MISSING__" )
    r$weights          = list( "NULL", c(0.5,0.5), "__MISSING__" )
    # r$mu               = list( "NULL", "__MISSING__" )
    # r$sigma            = list( "NULL", "__MISSING__" )
    # r$m3               = list( "NULL", "__MISSING__" )
    # r$m4               = list( "NULL", "__MISSING__" )
    # r$invert           = list( "TRUE", "FALSE", "__MISSING__" )
    # r$operational      = list( "TRUE", "FALSE", "__MISSING__" )
    str(r)

    # for an alternative test:
    # r$weights          = list( "NULL", c(1.0), c(1.0,2.0), c(1.0,2.0,3.0), "__MISSING__" )

    require(PerformanceAnalytics)
    data(ES_test_data)

    # TODO: prepare and store an argument test set in a separate environment
    # .stresstest.env or '.stress'
    generate.argset(arg_register = r)

    # produce results {PASS,FAIL} for every argument test set
    apply.argset(FUN="ES") # , subset=c(1,5,222,333,444,555,666,777,888,999,41472)

    # print test summary
    test_summary()


    plot.tests()



}





