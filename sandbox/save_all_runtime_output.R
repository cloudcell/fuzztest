



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

#------------------------------------------------------------------------------#
evaluate
function (input, envir = parent.frame(), enclos = NULL, debug = FALSE, 
          stop_on_error = 0L, keep_warning = TRUE, keep_message = TRUE, 
          new_device = TRUE, output_handler = default_output_handler, 
          filename = NULL) 
{
    parsed <- parse_all(input, filename)
    stop_on_error <- as.integer(stop_on_error)
    stopifnot(length(stop_on_error) == 1)
    if (is.null(enclos)) {
        enclos <- if (is.list(envir) || is.pairlist(envir)) 
            parent.frame()
        else baseenv()
    }
    if (new_device) {
        if (identical(grDevices::pdf, getOption("device"))) {
            dev.new(file = NULL)
        }
        else dev.new()
        dev.control(displaylist = "enable")
        dev <- dev.cur()
        on.exit(dev.off(dev))
    }
    on.exit(assign("last_plot", NULL, envir = environment(plot_snapshot)), 
            add = TRUE)
    out <- vector("list", nrow(parsed))
    for (i in seq_along(out)) {
        expr <- parsed$expr[[i]]
        if (!is.null(expr)) 
            expr <- as.expression(expr)
        out[[i]] <- evaluate_call(expr, parsed$src[[i]], envir = envir, 
                                  enclos = enclos, debug = debug, last = i == length(out), 
                                  use_try = stop_on_error != 2L, keep_warning = keep_warning, 
                                  keep_message = keep_message, output_handler = output_handler)
        if (stop_on_error > 0L) {
            errs <- vapply(out[[i]], is.error, logical(1))
            if (!any(errs)) 
                next
            if (stop_on_error == 1L) 
                break
        }
    }
    unlist(out, recursive = FALSE, use.names = FALSE)
}
<environment: namespace:evaluate>

#------------------------------------------------------------------------------#
rc <- try(do.call(what = FUN,args=args))
if(inherits(x = rc,what = "try-error")) {
    result <- "FAIL"
} else {
    result <- "PASS"
}
result


FUN=test
args=1

rc <- try(  t1 <- evaluate("do.call(what = FUN,args=args)"  ) )

rc <-replay(t1)

if(inherits(x = rc,what = "try-error")) {
    result <- "FAIL"
} else {
    result <- "PASS"
}
result


str(t1)

t1[[2]]

if(inherits(t1, what = "error")) {
    result <- "FAIL"
} else {
    result <- "PASS"
}
result


new_output_handler(error=function(){result<<-"FAIL"})

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#
eval_result=NULL
# eval_value=NULL
# output handler
oh <- new_output_handler(
    error=function(x)
    {
        # evaluate:::identity
        eval_result<<-"FAIL"
    }
)#,
# value=function(x){eval_value<<-})

FUN=log
args=list(x="test_text")

fuzztest_res <- evaluate("do.call(what=FUN, args=args)",output_handler = oh, debug=TRUE)
fuzztest_res
eval_result
# replay(fuzztest_res)

# purely log-related code
output_captured <- capture.output( dump(list = "fuzztest_res", file="") )

cat(output_captured)

output_captured
replay(output_captured)

cat(output_captured)
zz <- cat(paste(output_captured, collapse = ""))
replay(zz)

ls_loggers()
new_logger("kokokorokoko.log")
ls_loggers()
new
eval_result

replay(paste0(dump(list = "fuzztest_res", file=""), collapse = ""))


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
upper_result=NULL
# output handler
oh <- new_output_handler(error=function(x){upper_result<<-"FAIL"})
t1 <- evaluate("test(1)",output_handler = oh)
# t1 <- evaluate(test(1),output_handler = oh)
upper_result

replay(t1)

upper_result=NULL
t2 <- evaluate("test(2)",output_handler = oh)
upper_result


output_captured <- capture.output( dump(list = "t2", file="") )
output_captured






