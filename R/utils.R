############################################################################## #
# Description: Some Utility Function(s)
# Author: cloudcell
# Date: 2016-03
# License: GPL-3
############################################################################## #

#' @export
waitForUserInput <- function() 
{ 
    message(" ")
    message("-= Press 'Enter' when ready =-")
    message(" ")
    invisible(readline()) 
}


#' displays a message and stores it in a log file
#' 
#' @param x value to be submitted as a message to message()
#' @param logger logger handle which must be set up prior to running 
#'        lmessage
#' @param env container-environment of the testing framework
#' @param verbose (default==TRUE) whether to call message
#' 
#' @export
lmessage <- function(x, logger, env=cont.env, verbose=TRUE)
{
    if(!exists(logger)) stop(paste0("trying to write to logger '", logger, 
                                    "', which has not been set up / 
                                    initialized yet."))
    if(verbose) message(x)
    zz_logger <- env$loggers[lgr_name]
    
    cat(x, file = zz_logger, sep = "\n")
    
}

#' creates a new logger
#' 
#' @param lgr_name logger name
#' @param dest destination directory (defaults to the current work directory)
#' @param env work (container) environment (default 'cont.env')
#' 
#' @export
logger_new <- function(lgr_name, dest=getwd(), fname=NULL, env=cont.env)
{
    if(is.null(fname)) {stop("log file name must be provided!")}
    full_path_and_fname <- paste0(dest,"/",fname)
    
    if(!exists(envir = env, "loggers")) {
        env$loggers <- list()
    }
    
    # TODO: check whether a logger with the same name already exists
    env$loggers[lgr_name] <- file(full_path_and_fname, "w") 
    
}

#' removes a specified logger after closing the log file
#' 
#' @param lgr_name logger name
#' @param env work (container) environment (default 'cont.env')
#' 
#' @export
rm_logger <- function(lgr_name, env=cont.env)
{
    # TODO: first, check a logger exists
    close( env$loggers[lgr_name] )
}
