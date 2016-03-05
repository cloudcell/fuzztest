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


#' creates a new logger
#' 
#' @param fname log file name
#' @param logger_name logger name (character identifier / handle)
#' @param dest destination directory (defaults to the current work directory)
#' @param env work (container) environment (default 'cont.env')
#' 
#' @export
logger_new <- function( fname=NULL, logger_name="fuzzlog", dest=getwd(), env=cont.env)
{
    cont.env <- env
    # fname="test-debug.log"
    # dest=getwd()
    # logger_name="log-debug"
    if(is.null(fname)) {stop("log file name must be provided!")}
    full_path_and_fname <- paste0(dest,"/",fname)
    
    if(!exists(envir = cont.env, "loggers")) {
        cont.env$loggers <- list()
        message("Created a register of loggers.")
    }
    
    if(!is.null(cont.env$loggers[[logger_name]])) 
        stop("A logger with this handle already exists: remove it or create a different one.")
    
    cont.env$loggers[[logger_name]] <- file(full_path_and_fname, "w")
}

#' displays a message and stores it in a log file
#' 
#' @param x value to be submitted as a message to message()
#' @param logger logger name/handle (as a character string), which must 
#'        be set up prior to running lmessage()
#' @param env container-environment of the testing framework
#' @param verbose (default==TRUE) whether to call message
#' 
#' @export
lmessage <- function(x, logger_name="fuzzlog", env=cont.env, verbose=TRUE)
{
    cont.env <- env # to make it easy running this code in the global env.
    
    # logger_objs <- cont.env$loggers
    if(is.na(cont.env$loggers[logger_name])) {
        stop(paste0("trying to write to logger '", logger_name, 
                    "', which has not been set up / initialized yet."))
    }
    
    if(verbose) message(x)
    
    logger_obj <- cont.env$loggers[[logger_name]]
    
    cat(x, file = logger_obj, sep = "\n", append = TRUE)
}

#' removes a specified logger after flushing and closing its log file
#' 
#' @param logger_name logger name
#' @param env work (container) environment (default == 'cont.env')
#' 
#' @export
rm_logger <- function(logger_name="fuzzlog", env=cont.env)
{
    cont.env <- env
    con = cont.env$loggers[[logger_name]]
    
    # if(is.null(con)) { stop("Logger '", logger_name,"' does not exist.")}
    if(!(logger_name %in% ls_loggers() )) { 
        stop("Logger '", logger_name,"' does not exist.")
    }
    
    flush(con=con)
    close(con=con, type="w")
    
    cont.env$loggers[[logger_name]] <- NULL
    message("Log file flushed, closed, and logger '", logger_name, "' deleted.")
}

#' lists existing loggers
#' 
#' @param env work (container) environment (default == 'cont.env')
#' 
#' @export
ls_loggers <- function(env=cont.env)
{
    cont.env <- env
    # message(ls(cont.env$loggers))
    # cat(ls(cont.env$loggers))
    objects(cont.env$loggers)
}




