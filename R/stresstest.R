#------------------------------------------------------------------------------#
# Package: "stresstest"
# Test functions aimed at improving the quality of R code
#------------------------------------------------------------------------------#
# Description: a set of functions to verify that none of
#              the combinations of input arguments
#              causes a function undergoing stress test
#              to produce an error
#
# Author: cloudcell
# Date: 2016-02-19
# License: GPL-3
# Licensing Reference: http://choosealicense.com/
#------------------------------------------------------------------------------#

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

# generates argset and stores it in a default environment (if custom not given)
generate.argset <- function(arg_register, cust.env=NULL, verbose=FALSE, DEBUG=FALSE)
{
    message(rep("-",70))
    message("Generating argset")
    message(rep("-",70))

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
    e$container_test_args <- matrix(nrow=combos$total_qty, ncol = combos$args_qty)
    e$container_test_results <- vector( length = combos$total_qty, mode = "character")

    e$result_slot_next=1
    e$result_slot_max=combos$total_qty

    str(e$container_test_args)
    str(e$container_test_results)
    e$result_slot_next
    e$result_slot_max

    get_leafs(idx=combos$idx,storage.env = e)
    if(!is.null(e$result_slot_next)) stop ("not all test combos have been generated.")

    message("----------------------------------------------------------------------")
    message("Result: SUCCESS.")
    message("Returning test combinations of arguments within an environment")
    message("----------------------------------------------------------------------")
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
            get_leafs(idx, start_branch_id = i+1, accum_leafs=c(accum_leafs,j), storage.env = storage.env)
        } else {
            # spew out all the leafs
            printable_leafs=c(accum_leafs,j)
            if(verbose) print(printable_leafs) #TODO: use 'verbose' later

            # TODO: store the leaf set in a specially prepared environment
            # just use function's environment one level up (not 'frame' !!!)
            if(!is.null(storage.env)) {
                store_test_set(env = storage.env,test_set = printable_leafs)
            }
        }
    }
}

# store an indivisual test set (one argument set for one test)
# (service function)
store_test_set <- function(env=stop("storage environment must be provided"),
                           test_set=stop("test set must be provided"))
{
    e <- env
    ls(envir = e)

    if( is.null(e$result_slot_next) )
        stop("Data store does not have any more preallocated memory for storage.")

    i <- e$result_slot_next
    e$container_test_args[i,] <- test_set # TODO: check size !
    e$result_slot_next= i+1

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
        # for(i in 2) {
        if(verbose) message(rep("-",70))
        if(verbose) message("Argument number: ", i)

        arg_name <- names(r[i])
        if(verbose) message("Argument name: '", arg_name, "'" )
        if(verbose) message("Option choice number for argument id ", i, ": ", arg_ids.vct[i])

        choice <- arg_ids.vct[i]
        arg_value <- r[[i]][[choice]]
        # tmp <-  r[i]
        # arg_value <- tmp[choice]

        if(verbose) message("Argument value chosen: '", arg_value, "'")

        # print(i)
        # print( r[[i]][choice] )
        # print( arg_ids.vct[i] )

        # final_arg[['test']] <- NULL # remove from the list
        # final_arg[[arg_name]] <- arg_value # remove from the list
        # final_arg[arg_name] <- arg_value # remove from the list

        # str(unlist(arg_value))

        # arg_value <- unlist(arg_value)
        if(verbose) print("str(arg_value):")
        if(verbose) print(str(arg_value))

        # http://stackoverflow.com/questions/27491637/r-switch-statement-with-varying-outputs-throwing-error/27491753#27491753
        # choose b/n numeric vs character version
        switch_value <- arg_value

        #----------------------------------------------------------------------#
        # mode E {numeric, character, list, function}
        # typeof -- usually the same info as -- mode(storage.mode)
        #----------------------------------------------------------------------#
        #        typeof returns the type of an R object
        #        mode gives information about the mode of an object in the
        #             sense of Becker, Chambers & Wilks (1988), and is more
        #             compatible with other implementations of the S language
        #----------------------------------------------------------------------#
        # class {abstract type}
        #----------------------------------------------------------------------#
        # R types: http://stackoverflow.com/questions/6258004/r-types-and-classes-of-variables

        if(mode(arg_value)!="character") {
            # so numeric, list, and function are assigned as is
            switch_value <- "DEFAULT" # the "AS IS" assignment
        }

        # dealing with character types (with the non-character mode objects
        # fall through to the 'bottom':
        switch(switch_value,
               #TODO: consider changing this to "__val.NULL__"
               "NULL"={
                   # ref: http://stackoverflow.com/questions/7944809/assigning-null-to-a-list-element-in-r
                   if(verbose) print(paste0("Arg: ", arg_name," : assigning NULL") )
                   # ATTENTION: using '[' and NOT '[[' !
                   final_arg[arg_name]=list(NULL)
               },
               #TODO: consider changing this to "__val.NA__"
               # ref: http://stackoverflow.com/questions/7944809/assigning-null-to-a-list-element-in-r
               "NA"={
                   if(verbose) print(paste0("Arg: ", arg_name," : assigning NA") )
                   final_arg[[arg_name]]=NA
               },
               #TODO: consider changing this to "__val.TRUE__"
               "TRUE"={
                   if(verbose) print(paste0("Arg: ", arg_name," : assigning TRUE") )
                   final_arg[[arg_name]]=TRUE
               },
               #TODO: consider changing this to "__val.FALSE__"
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
        # if any is equal to "__MISSING__" then NULL them !!!
        if(verbose) message("str(final_arg) after this iteration:")
        if(verbose) print(str(final_arg))
    }
    final_arg
    str(final_arg)

    # final_arg$R

    final_arg
}


# the actual testing function (handles FUN and args) and catches exceptions
errorHandlingTest <- function(FUN,args)
{
    rc <- try(do.call(what = FUN,args=args))
    if(inherits(x = rc,what = "try-error")) {
        result <- "FAIL"
    } else {
        result <- "PASS"
    }
    result
}

# produce results {PASS,FAIL} for every argument test set
# function name must be supplied to THIS function as it allows developing
# adjusted versions of functions and test them further
apply.argset <- # alias
stressTest <- function(env=NULL, arg_register=cont.env$arg_register, 
                       FUN, subset=NULL, verbose=FALSE, DEBUG=FALSE)
{
    browser(expr = DEBUG)
    if(DEBUG) { verbose = TRUE }

    if(mode(FUN)!="character") 
        stop (paste0("Wrong argument FUN: please, provide a character ", 
                     "string naming the function to be called"))
    
    if(is.null(env)) {
        cont.env <- .GlobalEnv$cont.env
    } else {
        cont.env <- env
    }
        
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

    message(rep("-",70))
    for(i in selected_argset) {

        message("STRESSTEST: Argument Combination ID ", i)
        arg_ids.vct <- cont.env$container_test_args[i,]

        # prepare a single set of arguments for testing
        final_arg <- prepareArgs(arg_register = r, arg_selection_vector = arg_ids.vct, DEBUG=DEBUG)

        # test error crash/handling
        result <- errorHandlingTest(FUN=FUN, args = final_arg)
        message("Result: ", result)
        message(rep("-",70))

        cont.env$container_test_results[i] <- result
    }
    
    message("Test results were saved in the test environment in 'container_test_results'")
    print(ls(envir = cont.env))
    
    # cont.env$container_test_results # throw the results out (a la 'foreach')
    
    #--------------------------------------------------------------------------#
    # TODO: use a variable for the name of a tested function
    fname <- paste0("StressTest_", FUN, "_", gsub("[\\ :]","-",Sys.time()), ".RData")
    # save(list="bound_test_data", envir = cont.env, file = fname)
    # TODO: ASAP: rename cont.env() to .stress and create such an environment
    # if any custom env. was used (rename within the function that saves data)
    save(list="cont.env", envir = cont.env, file = fname)
    message("Test data was saved in the work directory ", getwd(), " as ", fname)
        
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





