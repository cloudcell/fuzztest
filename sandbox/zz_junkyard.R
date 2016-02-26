



# ------------------------------------------------------------------------ #
# -= supplementary analysis =-

if(0) {
    pairs(failure_map)
    pairs(failure_map.fct)
    
    require(cluster)
    heatmap(
        as.matrix(
            # produces correct distances for categorical (factor) data    
            daisy(failure_map.fct
                  # as.matrix(
                  #     cont.env$bound_test_data[cont.env$bound_test_data[,'results']=="FAIL",2:4]),
                  # type=c('factor','factor','factor')
            )),
        col=rainbow(n=4,start = 0.05)
    )
    dist(failure_map.fct) # wrong for factor class
    
    plot(daisy(failure_map.fct
               # as.matrix(
               #     cont.env$bound_test_data[cont.env$bound_test_data[,'results']=="FAIL",2:4]),
               # type=c('factor','factor','factor')
    ))
    
    
    
    
    # TODO: build a dendrogram for small tests (much fewer than 40K records)
    # within failed tests only
    clusters <- hclust(dist(cont.env$bound_test_data[ cont.env$bound_test_data['results']=='FAIL',]))
    plot(clusters)
    
    
    # use cluster on categorical data
    # clusters <- hclust(dist(cont.env$bound_test_data[ cont.env$bound_test_data['results']=='FAIL',]))
    clusters <- hclust(daisy(failure_map.fct))
    plot(clusters)
    
    heatmap(failure_map.fct)
    heatmap(as.matrix(daisy(failure_map.fct)), col=rainbow(17*17))
    
    require(rpart)
    bound_test_data <- cont.env$bound_test_data
    fit <- rpart(results ~ V1 + V2 + V3 + V4, data=bound_test_data, method="class")
    plot(fit)
    text(fit)
    
    
    # TODO: make a table of correlations among combinations of argument options
    # within failed tests only
    
    
    
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





