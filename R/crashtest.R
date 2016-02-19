#------------------------------------------------------------------------------#
# Package: "crashtest"
# Test functions aimed at improving the quality of R code
#------------------------------------------------------------------------------#
# Description: a set of functions to verify that none of 
#              the combinations of input arguments
#              causes a function undergoing crash test (stress test)
#              to produce an error
# 
# Author: cloudcell
# Date: 2016-02-19
# License: GPL-3
#------------------------------------------------------------------------------#

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


# TODO: rename to prepareTestParamIds
# or "setupTestContainer"
# (returns environment with test combos of args (their ids within the register))
getTestParamIds <- function(register, verbose=FALSE, DEBUG=FALSE)
{
    # create a container
    # with meta data explicitly stated:
    # capacity (number of possible records)
    # next available slot within the matrix

    e <- new.env() # storage.env
    # size <- getComboQty(register=idx, verbose=TRUE)
    # size <- getComboQty(register=zz$idx, verbose=TRUE)
    combos <- getComboQty(register=register, verbose=FALSE)
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

    get_leafs(idx=zz$idx,storage.env = e)
    if(!is.null(e$result_slot_next)) stop ("not all test combos have been generated.")

    message("------------------------------")
    message("Result: SUCCESS.")
    message("Returning test combinations of arguments within an environment")
    message("------------------------------")
    print(ls(envir = e))
    e
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
prepareArgs <- function(arg_register, arg_selection_vector, verbose=FALSE)
{
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
        str(unlist(arg_value))

        # arg_value <- unlist(arg_value)
        if(verbose) print(arg_value)

        # http://stackoverflow.com/questions/27491637/r-switch-statement-with-varying-outputs-throwing-error/27491753#27491753
        # choose b/n numeric vs character version
        switch_value <- arg_value
        if(class(arg_value)!="character")
            switch_value <- "DEFAULT" # the

        switch(switch_value,
               "NULL"={
                   if(verbose) print("assigning NULL")
                   final_arg[[arg_name]]=NULL
               },
               "TRUE"={
                   if(verbose) print("assigning NULL")
                   final_arg[[arg_name]]=TRUE
               },
               "FALSE"={
                   if(verbose) print("assigning NULL")
                   final_arg[[arg_name]]=FALSE
               },
               '__MISSING__'={
                   # final_arg[[arg_name]] <- NULL
               },
               # DEFAULT:
               # use get() or assign()
               # final_arg[[arg_name]] <- unlist(arg_value)
               {
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


# TODO: bring out the environment out to be able to save it easily
# crashTest <- function(env,arg_register, FUN, verbose=FALSE)#, test_set_container)
crashTest <- function(env, FUN, verbose=FALSE)#, test_set_container)
{

    # r=arg_register
    # 
    # cont.env <- getTestParamIds(register = r)
    # ls(envir = cont.env)
    cont.env$container_test_args
    
    # arg_ids.vct <- cont.env$container_test_args[10000,]
    # store_test_set(env = )
    # loop thru all the test arg. set

    for(i in 1:cont.env$result_slot_max) {
        message("test #", i)
        arg_ids.vct <- cont.env$container_test_args[i,]
        
        # prepare arguments for testing
        final_arg <- prepareArgs(arg_register = r, arg_selection_vector = arg_ids.vct)
        
        # test error handling / crash
        result <- errorHandlingTest(FUN=FUN, args = final_arg)
        message("result #", result)
        
        cont.env$container_test_results[i] <- result

    }

    cont.env$container_test_results
}


#------------------------------------------------------------------------------#
if(0) { # the main test

    if(0) {
        ## Title: Error tests for ES()
        ## Description: tests ES() for producing errors based on all possible combinations of options
        
        ES(R = ,
           p = 0.95,
           # ... = ,
           method           = c("modified", "gaussian", "historical"),
           clean            = c("none", "boudt", "geltner"),
           portfolio_method = c("single", "component"),
           weights          = NULL,
           mu               = NULL,
           sigma            = NULL,
           m3               = NULL,
           m4               = NULL,
           invert           = TRUE,
           operational      = TRUE)
        
        
        if(0){
            errorHandlingTest("+",args = list(1,1))
            errorHandlingTest("+",args = list(1,1,3))
        }
        
        
    }
    
    ## set up test for a function
    
    # arguments:
    # * set up argument register with all possible arg. value assignments
    r <- list()
    r$R                = list( pr ) # variable name as character string ?
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
    


    require(PerformanceAnalytics)
    # 1. create register of possible options (values/missing) for each argument
    # 
    # 2. produce an index to the register
    # zz <- getComboQty(r)
    # zz <- getComboQty(r) # creates an index
    
    # 3. create an environment
    #    as a container for the combos & results & fill it & provide a ref.
    cont.env <- getTestParamIds(register = r) # TODO: rename to setupTestContainer
    # ls(envir = cont.env)

        # 4. performTesting
    results <- crashTest(env=cont.env, FUN=ES)
    
    plot(which(results=="PASS"))
    
    #results <- .Last.value
    save(list="results", file = "ES_test1-20160220-0235.RData")
    getwd()
    
}


#------------------------------------------------------------------------------#

# str(r)
# length(r)
# r
# 
# unlist(r$p[1]  )
# names(r[1])


# TESTS:
if(0) {
    
    
    # get_next_branch(zz$idx, branch_id=1,accum_leafs=c(0))
    
    getArg(zz$idx,10)
    
    zz$idx[10,"qty"]
    
    # for(i in 1:)
    
    #
    args <- list(1,2,3,NULL)
    args
    args[[1]] <- NULL
    
    
    args
    args[['test']]=TRUE
    args
    args[['test']] <- NULL # remove from the list
    args[['test']] = NULL # remove from the list
    args
    args
    
    args$invert=TRUE
    args
}

# do.call() reference:
if(0) {
    # If quote is FALSE, the default, then the arguments are evaluated (in the
    # calling environment, not in envir). If quote is TRUE then each argument is
    # quoted (see quote) so that the effect of argument evaluation is to remove the
    # quotes – leaving the original arguments unevaluated when the call is
    # constructed.
    
    do.call()
    
    do.call
    function (what, args, quote = FALSE, envir = parent.frame())
    {
        if (!is.list(args))
            stop("second argument must be a list")
        if (quote)
            args <- lapply(args, enquote)
        .Internal(do.call(what, args, envir))
    }
    
}

# TESTS: get_leafs
if(0) {
    test_idx <- get_next_branch(zz$idx, branch_id=1, accum_leafs=c())#, DEBUG=TRUE)
    get_next_branch(zz$idx, branch_id=8)#, DEBUG=TRUE)
    get_next_branch(zz$idx)
    get_next_branch(zz$idx, branch_id=11, accum_leafs=c())#, DEBUG=TRUE)
    get_next_branch(zz$idx, branch_id=11, accum_leafs=c(), DEBUG=TRUE)
}

#------------------------------------------------------------------------------#






