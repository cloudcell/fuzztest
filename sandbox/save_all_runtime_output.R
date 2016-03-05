



# src:  http://stackoverflow.com/questions/4948361/how-do-i-save-warnings-and-errors-as-output-from-a-function/4963132#4963132
require(evaluate)


# http://stackoverflow.com/questions/3903157/how-can-i-check-whether-a-function-call-results-in-a-warning/4947528
output_captured <- capture.output( dump(list = "t2", file="") )
# output_captured <- dump(list = "t2", file="")
# output_captured <- capture.output( t2)

logger_new(fname = "laylaylay.log")
ls_loggers()
lmessage(output_captured) # works perfectly !!!!!

message(output_captured)
rm_logger()

# http://stackoverflow.com/questions/4948361/how-do-i-save-warnings-and-errors-as-output-from-a-function/4963132#4963132
# Try the evaluate package.
# 
# library(evaluate)
# test <- function(i)
#     switch(i, "1"=stop("oops"), "2"={ warning("hmm"); i }, i)
# 
# t1 <- evaluate("test(1)")
# t2 <- evaluate("test(2)")
# t3 <- evaluate("test(3)")
# It currently lacks a nice way of evaluating expression though - this is mainly because it's targetted towards reproducing exactly what R output's given text input at the console.
# 
# replay(t1)
# replay(t2)
# replay(t3)
# It also captures messages, output to the console, and ensures that everything is correctly interleaved in the order in which it occurred.
# 
# shareeditflag
# answered Feb 10 '11 at 21:47
# 
# hadley
# 48.9k1393160
