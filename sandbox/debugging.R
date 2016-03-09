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



