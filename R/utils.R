############################################################################## #
# Description: Some Utility Function(s)
# Author: cloudcell
# Date: 2016-03
# License: GPL-3
############################################################################## #

#' @export
waitForUserInput <- function() 
{ 
    # message(" ")
    message(">>>~~~> press 'enter' to continue...")
    # message(" ")
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
new_logger <- function( fname=NULL, logger_name="fuzzlog", dest=getwd(), env=cont.env)
{
    # default env. name
    cont.env <- env
    
    # fname="test-debug.log"
    # dest=getwd()
    # logger_name="log-debug"
    if(is.null(fname)) {stop("log file name must be provided!")}
    full_path_and_fname <- paste0(dest,"/",fname)
    
    if(!exists(x="loggers", envir = cont.env)) {
        cont.env$loggers <- list()
        message("Created a register of loggers.")
    }
    
    if(!is.null(cont.env$loggers[[logger_name]])) 
        stop("A logger with handle '",logger_name,"' already exists: remove it or create a different one.")
    
    cont.env$loggers[[logger_name]] <- file(full_path_and_fname, "w")
}

#' displays a message and stores it in a log file
#'
#' No matching of partially spelled arguments is performed in this function.
#' 
#' @param ... values to be concatenated and submitted as text to message()
#' @param .logger logger name/handle (as a character string), which must 
#'        be set up prior to running lmessage()
#' @param .env container-environment of the testing framework
#' @param .verbose (default==TRUE) whether to call message
#' 
#' @export
lmessage <- function(x, .logger_name="fuzzlog", .env=cont.env, verbose=TRUE)
# lmessage <- function(...)
{
    # .logger_name="fuzzlog"
    # .env=cont.env
    # .verbose=TRUE
    # TODO: take partial arg. matching from lm()
    cont.env <- .env # to make it easy running this code in the global env.
     
    # x= as.list(match.call(expand.dots = TRUE)[-1])
    # # x= match.call()[-1]
    # str(x)
    
    # # delete these before concatenating
    # x$.verbose <- NULL
    # x$.logger_name <- NULL
    # x$.env <- NULL

    # merge all parts of the message for printing    
    # x <- paste0(lapply(x,eval))
    # x <- paste0(unlist(x))
    # x <- paste0(unlist(x))
    # if(verbose) str(x)
        
    if(verbose) message(x)
    
    logger_obj <- cont.env$loggers[[.logger_name]]
    
    if( !inherits(logger_obj ,"connection") ) {
        stop(paste0("Trying to write to logger '", .logger_name, 
                    "', which has not been set up / initialized yet."))
    }
        
    cat(x, file = logger_obj, sep = "\n", append = TRUE)
}

#' removes a specified logger after flushing and closing its log file
#' 
#' @param logger_name logger name
#' @param env work (container) environment (default == 'cont.env')
#' @param silent allows prevent messages when they are least expected in the
#'        program, e.g.: in the beginning of the code when a logger 'slot'
#'        is being cleaned and created anew. The default is FALSE.
#' 
#' @export
rm_logger <- function(logger_name="fuzzlog", env=cont.env, silent=FALSE)
{
    cont.env <- env
    con = cont.env$loggers[[logger_name]]
    
    # catch the error here as well!
    if(is.null(con)) {
        if(!silent) message("Logger '", logger_name,"' does not exist.")
        return()
    }
    
    # if(is.null(con)) { stop("Logger '", logger_name,"' does not exist.")}
    # # the following line produces 
    # # "Error in as.environment(pos) : using 'as.environment(NULL)' is defunct"
    # browser()
    if(!(logger_name %in% ls_loggers(env=env) )) { 
        if(!silent) message("Logger '", logger_name,"' does not exist.")
        return()
    }
    
    flush(con=con)
    close(con=con, type="w")
    
    cont.env$loggers[[logger_name]] <- NULL
    if(!silent) {
        message("Log file flushed, closed, and logger '",
                logger_name, "' deleted.")
    }
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




