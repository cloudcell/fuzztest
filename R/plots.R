
# References:
# 
# 
# TODO: consider using prp() plot from rpart package
#       http://www.milbo.org/rpart-plot/prp.pdf


# generates analytics for plotting
generate.analytics <- function(env=cont.env, DEBUG=FALSE, verbose=FALSE) 
{
    # use this name to debug the function running its code outside the function
    cont.env=env 
    
    # for plotting on parallel axes (using parallelplot())
    failure_map <- as.matrix( cont.env$bound_test_data[
        cont.env$bound_test_data[,'results']=="FAIL",-ncol(cont.env$bound_test_data)])
    cont.env$failure_map <- failure_map
    
    # for categorical analysis
    failure_map.fct <- data.frame(apply(failure_map,2,as.factor))
    cont.env$failure_map.fct <- failure_map.fct
    
    message("Transformed test results saved in the test environment")
}

# uses parallelplot with added 'jitter' to diplay better view of combinations of
# argument options that lead to failure of a function
# TODO: allow to save as a *.PDF file for further analysis 
# (pdf allows for increased magnification)
plot.tests <- function(env=cont.env, DEBUG=FALSE, verbose=FALSE) 
{

    # prepare data for plotting, if data does not yet exist
    if(is.null(cont.env$failure_map)) {
        message("Data for plotting has not yet been generaged. Generating data.")
        generate.analytics(env=cont.env)
    }

    message("Plotting combinations of argument options...")   
    
    require(lattice)
    
    nr <- nrow(failure_map)
    failure_map_parplot <- failure_map
    for(i in 1:nr){
        # The max value one can add and still avoid wrong association among
        # adjacent argument option numbers is 1: the only necessary condition is
        # ( jitter < 1 ) as factors/option numbers are separated by a distance 1
        jitter <- (i / nr) / 5 # also shrink by 3
        # The max shift must NOT depend on the max number of options per
        # variable as the scaling is done by the plot function itself !
        failure_map_parplot[i,] <- failure_map_parplot[i,] + jitter 
    }
    failure_map_parplot
    
    xlab.str <- names(cont.env$arg_register)
    
    # setnames(data, old=c() new=c()) in library(data.table)
    colnames(failure_map_parplot) <- xlab.str
    
    # TODO: add arg. names as labels to the graph
    chart <- parallelplot(failure_map_parplot, horizontal.axis=FALSE, col=rainbow(nr),
                 main="Argument-Option Combinations Correlated with Function Failure")
    print(chart)
    # splom(failure_map_parplot, horizontal.axis=FALSE, col=rainbow(nr))
    message("Done.")       
}




