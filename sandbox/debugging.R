set.seed(0)
r <- list()
r$x <- c(0)
r$y <- c(0)
r$option <- c("a")
r$suboption <- c("a")
# r$option <- c("a", "b", "c")
# r$suboption <- c("a", "b", "c","d")

# both must be run: generate.argset & apply.argset to see the error:
generate.argset(arg_register = r, display_progress=TRUE)
apply.argset(FUN="fuzzdemofunc")#, DEBUG = T)


test_summary()


base_fname <- paste0("StressTest_", "FUN", "_", gsub("[\\ :]","-",Sys.time()))
env_fname <- paste0(base_fname, ".RData")
log_fname <- paste0(base_fname, ".log")

#--------------------------------------------------------------------------#
# create a logger

message("apply.argset(): creating a logger")

try(rm_logger()) # flush & delete default logger w/o warnings

new_logger(log_fname) # with a default logger 'handle'

#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
#
FUN="fuzzdemofunc"
args <- list(x=0,y=10,option="a", suboption="a")
errorHandlingTest_dbg <- function(FUN,args)
{
    #--------------------------------------------------------------------------#
    # libraries
    require(evaluate) # in the test function invoked from apply.argset
    
    #--------------------------------------------------------------------------#
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
        },
        graphics = function(x) {
            return(0)
        }
    )#, value=function(x){eval_value<<-})
    
    #--------------------------------------------------------------------------#
    # test
    # message(paste0("before ",eval_result))
    
    res <- evaluate::evaluate("do.call(what=FUN, args=args)", 
                              output_handler = oh, 
                              # new_device = FALSE,
                              new_device = TRUE,
                              debug=FALSE) #saves to much data if true
    # res
    
    if(is.null(eval_result)) { eval_result <- "PASS"}
    
    # a hack to stop dumping the latest generated graphics
    dev.new(); dev.off(which = dev.cur())
    
    str(res)
    
    deparse(res)
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

#--------------------------------------------------------------------------#





