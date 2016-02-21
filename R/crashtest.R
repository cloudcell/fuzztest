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
# Licensing Reference: http://choosealicense.com/
#------------------------------------------------------------------------------#

# TODOs (in the order of priority) ----
# 0. create a dendrogram of input parameters for tests with the status == 'FAIL'
# 1. consider using variable names as character strings and numeric values:
#    variable names as strings could be displayed more easily
#    during assignment phase, they could simply be stripped of technical tags
#    to be used with 'assign()' i.e. __var.pr would mean that
#    a variable 'pr' will be used during assignment
#    At the same time, displaying analytics will be much easier as no special
#    treatment will be required
# 2. start using parfor() to speed up the test
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

# TODO: rename to prepareTestParamIds
# or "setupTestContainer" setupTestCombos generateArgSet argset.generate
# (returns environment with test combos of args (their ids within the register))
generate.argset <- # alias
getTestParamIds <- function(register, verbose=FALSE, DEBUG=FALSE)
{
    message(rep("-",70))
    message("Generating argset")
    message(rep("-",70))

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

    message("----------------------------------------------------------------------")
    message("Result: SUCCESS.")
    message("Returning test combinations of arguments within an environment")
    message("----------------------------------------------------------------------")
    message("The following objects are available within the testing environment:")
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


# TODO: bring out the environment out to be able to save it easily
# crashTest <- function(env,arg_register, FUN, verbose=FALSE)#, test_set_container)
apply.argset <- # alias
crashTest <- function(env, arg_register, FUN, verbose=FALSE, subset=NULL, DEBUG=FALSE)#, test_set_container)
{
    browser(expr = DEBUG)
    if(DEBUG) { verbose = TRUE }

    r=arg_register # to be able to run function code "in the global env."
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

        message("CRASHTEST: Argument Combination ID ", i)
        arg_ids.vct <- cont.env$container_test_args[i,]

        # prepare a single set of arguments for testing
        final_arg <- prepareArgs(arg_register = r, arg_selection_vector = arg_ids.vct, DEBUG=DEBUG)

        # test error handling / crash
        result <- errorHandlingTest(FUN=FUN, args = final_arg)
        message("Result: ", result)
        message(rep("-",70))

        cont.env$container_test_results[i] <- result
    }
    cont.env$container_test_results # throw the results out (a la 'foreach')
}


#---sandbox--------------------------------------------------------------------#

if(0) { # the main test

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
    str(r)


    require(PerformanceAnalytics)
    cont.env <- generate.argset(register = r) # TODO: rename to setupTestContainer
    results <- apply.argset(env=cont.env, arg_register = r,
                            FUN=ES
                            # ,
                            # subset=c(1,5,222,333,444,555,666,777,888,999,41472)
                            )

    test_summary()
    
    # build data necessary for a dendrogram

    # make a table of cross-correlation
    # for every combination of argument options
    #
    #

    # 1. create register of possible options (values/missing) for each argument
    #
    # 2. produce an index to the register
    # zz <- getComboQty(r)
    # zz <- getComboQty(r) # creates an index

    # 3. create an environment
    #    as a container for the combos & results & fill it & provide a ref.
    # ls(envir = cont.env)

    # 4. apply.argset(argset_container.env, FUN)
    # results <- apply.argset(env=cont.env, arg_register = r,  FUN=ES, subset=c(1), DEBUG=TRUE)

    plot(which(results=="PASS"))

    #results <- .Last.value
    save(list="results", file = "ES_test2-20160220-0900.RData")
    getwd()

}

test_summary <- function(env=cont.env, DEBUG=FALSE, verbose=FALSE)
{
    # create the following table:
    # ========================================================
    #     ARG~OPT        Arg Name        PASS    FAIL    FAIL%
    # --------------------------------------------------------
    
    bound_test_data <- (cbind(as.data.frame(cont.env$container_test_args),
                              results=cont.env$container_test_results))
    
    if(verbose) head(bound_test_data)
    if(verbose) str(bound_test_data)
    if(verbose) by(bound_test_data, bound_test_data[,"results"], summary)
    if(0) {   
        field_to_remove_nbr <- length(r)
        bound_test_data_pass <- bound_test_data[bound_test_data[,'results']=="PASS",-field_to_remove_nbr]
        bound_test_data_fail <- bound_test_data[bound_test_data[,'results']=="FAIL",-field_to_remove_nbr]
    }
    
    total_results_nbr <- cont.env$result_slot_max
    
    summary_full <- list()
    for(i in 1:length(r)) {
        summary_full[[i]] <- 
            tapply( X=bound_test_data[,i],
                    INDEX=list(bound_test_data[,i],
                               bound_test_data[,'results']),
                    length)
    }
    summary_full
    
    if(verbose) str(summary_full[[1]])
    
    summary_ext <- list()
    summary_short <- list()
    for(i in 1:length(r)) {
        
        # calculate percentages of failure
        pct_failed <- 100 * summary_full[[i]][,"FAIL"] / 
            (summary_full[[i]][,"PASS"]+summary_full[[i]][,"FAIL"])
        pct_failed_min <- min(pct_failed)
        pct_failed_max <- max(pct_failed)
        pct_failed_diff <- pct_failed_max - pct_failed_min
        
        
        summary_ext[[i]] <- cbind(summary_full[[i]], fail_pct=pct_failed)
        summary_short[[i]] <- pct_failed_diff
    }
    
    
    
    # ------------------------------------------------------------------------ #
    txt_width <- 0
    for(i in 1:length(r)) {
        tmp <- nchar(names(r[i]))
        if(txt_width<tmp) txt_width <- tmp
    }
    
    argName_title <- "Arg Name"
    argName_title_width <- nchar(argName_title)
    txt_width <- max(txt_width, argName_title_width)
    # ------------------------------------------------------------------------ #
    
    
    # create the following table:
    #----------------------------------------------------------------
    #   PASS   :   FAIL   :  FAIL % : ARG  :  OPTION  : Argument Name
    #----------------------------------------------------------------
    #   Qty    :   Qty    :    1    :  1     : R
    #   Qty    :   Qty    :    1    :  1     : R
    #   Qty    :   Qty    :    1    :  1     : R
    #----------------------------------------------------------------
    #   Qty    :   Qty    :    1    :    2     : R
    #----------------------------------------------------------------
    # i=3
    
    pad_width=2
    pad.txt <- rep(" ",pad_width)
    # browser()
    argOpt_title="ARG~OPT"
    argOpt_title_width <- nchar(argOpt_title)
    tail_title="PASS    FAIL    FAIL%"
    head_p2 <- format(x=argName_title, justify='centre',width=txt_width + pad_width*2)
    
    # TODO: calculate dynamically
    table_width=56
    
    message(rep("=",table_width))
    message(pad.txt, argOpt_title, 
            pad.txt, head_p2, 
            pad.txt, tail_title)
    message(rep("-",table_width))
    
    for(i in 1:length(r)) {
        for(j in 1:nrow(summary_ext[[i]])) {
            message(
                pad.txt,
                format(x=i,width=2, justify='right'), 
                " ~ ", 
                format(x=j,width=2, justify='left'), 
                pad.txt,
                pad.txt,
                format(names(r[i]), width = txt_width),
                pad.txt,
                format(summary_ext[[i]][,"PASS"][j],width=6, justify='right'),
                pad.txt,
                format(summary_ext[[i]][,"FAIL"][j],width=6, justify='right'),
                pad.txt,
                pad.txt,
                format(summary_ext[[i]][,"fail_pct"][j],digits = 3, nsmall = 1)
            )
        }
    }
    message(rep("=",table_width))
    message() # empty line
    
    # ------------------------------------------------------------------------ #
    # | Failure Rate Contribution, (Max % - Min %)       | Argument Name
    # [***********************                           ] 
    # [**************************************************] 
    #  
    summary_short
    
    
    # ------------------------------------------------------------------------ #
    
    # pad_width <- 2
    bar_width <- 50
    bar_width_all <- bar_width + 2
    
    # a field of the table displying the percentage (of FAILs) variability
    percentage_width <- 4
    
    # ------------------------------------------------------------------------ #
    
    table_width <- txt_width + pad_width*3 + bar_width_all + percentage_width
    
    head_p1 <- format(x=argName_title, justify='centre',width=txt_width + pad_width*2)
    head_p2_width <- percentage_width + pad_width*1 + bar_width_all 
    head_p2 <- format(x="Failure Rate Contribution, % (Max - Min)", width=head_p2_width, justify='centre' )
    
    message(rep("=",table_width))
    message(head_p1, head_p2)
    message(rep("-", table_width))
    
    for(i in 1:length(r)) {
        
        bar_fill <-  paste0(rep("*", round( bar_width/100 * summary_short[[i]], digits = 0)))
        bar_blank <- paste0(rep(" ", bar_width - round( bar_width/100 * summary_short[[i]], digits = 0)))
        
        txt_name <- names(r[i])
        txt_name <- format(x=txt_name, justify='right', width=txt_width)
        
        message(pad.txt, 
                txt_name,
                pad.txt,
                format(summary_short[[i]],digits = 3,justify = 'right',nsmall=1, width=percentage_width),
                pad.txt,
                c("'",bar_fill,bar_blank,"'") #,
        )
        
    }
    message(rep("=",table_width))
    
}


#----junkyard------------------------------------------------------------------#
if(0) {

# str(r)
# length(r)
# r
#
# unlist(r$p[1]  )
# names(r[1])



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

}
#------------------------------------------------------------------------------#






