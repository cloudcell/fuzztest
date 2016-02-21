


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


