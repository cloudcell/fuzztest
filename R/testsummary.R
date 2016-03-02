#------------------------------------------------------------------------------#
# Author: cloudcell
# Date: 2016-03
# License: GPL-3
#------------------------------------------------------------------------------#




#' Aggregates all the test results by argument 'state' and provides statistics
#' 
#' The Function splits statistics into detailed and short summaries.
#' The 'detailed statistics': PASS/FAIL count, percentage of failed tests.
#' The short summary contains only key data for determining a degree to which 
#' some argument 'state' is associated with whether a function completes
#' its operations with a 'PASS' or 'FAIL' state. Use this statistics to isolate
#' those arguments analyze how code 'misbehaves' by looking up a more detailed
#' output from the first 'detailed statistics' table.
#' 
#' @param env work environment, set only if default environment is not used
#' @param verbose provides additional text output during processing
#' @param DEBUG enters the debug mode on function entry
#' 
#' @aliases test_summary, summary.fuzz
#' @author cloudcello
#' 
#' @export
#TODO: make sure all functions have DEBUG listed as the _last_ argument
summary.fuzz <- # alias
test_summary <- function(env=cont.env, verbose=FALSE, DEBUG=FALSE) 
{
    if(DEBUG) browser()
    
    # prepare data for summary tables
    cont.env=env
    
    # to simplify handling of the register
    r <- cont.env$arg_register
    
    bound_test_data <- (cbind(as.data.frame(cont.env$container_test_args),
                              results=cont.env$container_test_results))
    
    # save in the environment
    cont.env$bound_test_data <- bound_test_data
    
    if(verbose) print(head(bound_test_data))
    if(verbose) str(bound_test_data)
    if(verbose) by(bound_test_data, bound_test_data[,"results"], summary)
    
    # ------------------------------------------------------------------------ #
    field_to_remove_nbr <- length(r)+1 # the one last field with 'results' added later
    
    # needed for small size tests dendrogram 
    bound_test_data_fail <- bound_test_data[bound_test_data[,'results']=="FAIL",-field_to_remove_nbr]

    if(0) {   
        bound_test_data_pass <- bound_test_data[bound_test_data[,'results']=="PASS",-field_to_remove_nbr]
    }
    # ------------------------------------------------------------------------ #

    total_results_nbr <- cont.env$result_slot_max
    
    summary_full <- list()
    for(i in 1:length(r)) {
        summary_full[[i]] <- 
            tapply( X=bound_test_data[,i],
                    INDEX=list(bound_test_data[,i],
                               bound_test_data[,'results']),
                    length)
        
        # replacing NA with zeros
        summary_full[[i]][is.na(summary_full[[i]])]<-0
        if(verbose) print(summary_full[[i]])
    }
    
    # summary_full
    if(verbose) str(summary_full[[1]])
    
    summary_ext <- list()
    summary_short <- list()
    for(i in 1:length(r)) {


        # make sure both PASS and FAIL columns are present
        # NB! in no particular order
        if( "PASS" %in% colnames(summary_full[[i]])) {
            summary_passed <- summary_full[[i]][,"PASS"]
        } else {
            summary_full[[i]] <- cbind(summary_full[[i]],PASS=0)
            # summary_passed <- 0 # will be recycled!
        }

        if( "FAIL" %in% colnames(summary_full[[i]])) {
            summary_failed <- summary_full[[i]][,"FAIL"]
        } else {
            summary_full[[i]] <- cbind(summary_full[[i]],FAIL=0)
            # summary_failed <- 0 # will be recycled!
        }


        # calculate percentages of failure
        pct_failed <- 100 * summary_full[[i]][,"FAIL"] / 
            (summary_full[[i]][,"PASS"]+summary_full[[i]][,"FAIL"])

        # pct_failed <- 100 * summary_failed / (summary_failed + summary_passed)

        pct_failed_min <- min(pct_failed)
        pct_failed_max <- max(pct_failed)
        pct_failed_diff <- pct_failed_max - pct_failed_min
        
        
        summary_ext[[i]] <- cbind(summary_full[[i]], fail_pct=pct_failed)
        summary_short[[i]] <- pct_failed_diff
    }
    
    
    # <<-- at this point, all the summary data has been prepared -->>
    
    # save summaries in the environment
    cont.env$summary_short <- summary_short
    cont.env$summary_ext <- summary_ext
    
    # ######################################################################## #
    # Prepare Common Parameters for the Two Summary Tables
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
    
    message("Fuzztest: Argument-Option Combination Results")
    # ######################################################################## #
    # Summary Table: Extended
    # ======================================================== #
    # ARG~OPT        Arg Name        PASS    FAIL    FAIL%
    # -------------------------------------------------------- #
    # browser()
    
    pad_width=2
    pad.txt <- rep(" ",pad_width)
    
    argOpt_title="ARG~OPT"
    argOpt_title_width <- nchar(argOpt_title)+2
    
    tail_title="PASS    FAIL    FAIL%"
    tail_title_width=nchar(tail_title)
    
    head_p2 <- format(x=argName_title, justify='centre',width=txt_width + pad_width*2)
    head_p2_width <- nchar(head_p2)
    
    table_width <- argOpt_title_width + head_p2_width + tail_title_width +
                   pad_width    + pad_width      + pad_width*2 
    
    # <-- start drawing -->
    message(rep("=",table_width))
    message(pad.txt, 
            format(x=argOpt_title,width=argOpt_title_width, justify='centre'), 
            pad.txt, head_p2, 
            pad.txt, tail_title)
    message(rep("-",table_width))
    
    for(i in 1:length(r)) {
        for(j in 1:nrow(summary_ext[[i]])) {
            message(
                pad.txt,
                format(x=i,width=2, justify='right'), 
                " ~ ", 
                format(x=j,width=4, justify='left'), 
                pad.txt,
                pad.txt,
                format(names(r[i]), width = txt_width),
                pad.txt,
                format(summary_ext[[i]][,"PASS"][j],width=6, justify='right'),
                pad.txt,
                format(summary_ext[[i]][,"FAIL"][j],width=6, justify='right'),
                pad.txt,
                pad.txt,
                format(summary_ext[[i]][,"fail_pct"][j],digits = 3, nsmall = 1, width=5, justify='left')
            )
        }
    }
    message(rep("=",table_width))
    message() # empty line
    
    # ############################################################################## #
    # Summary Table: Short
    # ============================================================================== #
    #       Arg Name               Failure Rate Contribution, % (Max - Min)         
    # ------------------------------------------------------------------------------ #
    #                  R   0.0  '                                                  '
    #                  p   0.0  '                                                  '
    #             method  33.3  '*****************                                 '
    #              clean   0.0  '                                                  '
    #   portfolio_method  41.7  '*********************                             '
    #            weights  66.7  '*********************************                 '
    #            
    # ------------------------------------------------------------------------------ #
  
    if(verbose) str(summary_short)
    
    
    # pad_width <- 2
    bar_width <- 50
    bar_width_all <- bar_width + 2
    
    # a field of the table displying the percentage (of FAILs) variability
    percentage_width <- 5 # to accommodate "100.0"
    
    # ------------------------------------------------------------------------ #
    
    table_width <- txt_width + pad_width*3 + bar_width_all + percentage_width
    
    head_p1 <- format(x=argName_title, justify='centre',width=txt_width + pad_width*2)
    head_p2_width <- percentage_width + pad_width*1 + bar_width_all 
    head_p2 <- format(x="Failure Rate Contribution, % (Max - Min)", width=head_p2_width, justify='centre' )
    
    
    message("Fuzztest: Summary")
    
    # <-- start drawing -->
    message(rep("=",table_width))
    message(head_p1, head_p2)
    message(rep("-", table_width))
    
    for(i in 1:length(r)) {
        
        bar_fill_size <- as.integer( round( bar_width/100 * summary_short[[i]], digits = 0) )
        bar_fill <-  paste0(rep("*", bar_fill_size))
        bar_blank <- paste0(rep(" ", bar_width - bar_fill_size))
        
        txt_name <- names(r[i])
        txt_name <- format(x=txt_name, justify='right', width=txt_width)
        
        message(pad.txt, 
                txt_name,
                pad.txt,
                format(summary_short[[i]],digits = 3, justify = 'right', nsmall=1, width=percentage_width),
                pad.txt,
                c("'",bar_fill,bar_blank,"'") #,
        )
        
    }
    message(rep("=",table_width))
    message() # blank line
    
    if(verbose) message("summaries were saved in the testing environment as")
    if(verbose) message("'summary_short' and 'summary_ext'")
       
    if(verbose) print(ls(envir = cont.env))
}


