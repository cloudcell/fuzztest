
# References:
# 
# 
# TODO: consider using prp() plot from rpart package
#       http://www.milbo.org/rpart-plot/prp.pdf

# TODO: plot all the 'PASS' tests with a transparent ink so that the vertical
#       scale of the axes correctly corresponds to the category ordered position

# generates analytics for plotting
generate.analytics <- function(env=cont.env, DEBUG=FALSE, verbose=FALSE) 
{
    # use this name to debug the function running its code outside the function
    cont.env=env

    #--------------------------------------------------------------------------#
    # for tests with 'FAIL' outcomes:

    # for plotting on parallel axes (using parallelplot())
    failure_map <- as.matrix( cont.env$bound_test_data[
        cont.env$bound_test_data[,'results']=="FAIL",-ncol(cont.env$bound_test_data)])
    cont.env$failure_map <- failure_map
    
    # for categorical analysis
    failure_map.fct <- data.frame(apply(failure_map,2,as.factor))
    cont.env$failure_map.fct <- failure_map.fct

    #--------------------------------------------------------------------------#
    # now the same for tests with 'PASS' outcomes:
    success_map <- as.matrix( cont.env$bound_test_data[
        cont.env$bound_test_data[,'results']=="PASS",-ncol(cont.env$bound_test_data)])
    cont.env$success_map <- success_map

    # for categorical analysis
    success_map.fct <- data.frame(apply(success_map,2,as.factor))
    cont.env$success_map.fct <- success_map.fct


    #--------------------------------------------------------------------------#
    # done
    message("Transformed test results saved in the test environment")
}


# a quote from https://en.wikipedia.org/wiki/Parallel_coordinates
# "
# When used for statistical data visualisation there are three important
# considerations: the order, the rotation, and the scaling of the axes.
#
# The order of the axes is critical for finding features, and in typical data
# analysis many reorderings will need to be tried. Some authors have come up
# with ordering heuristics which may create illuminating orderings.
# "
#


# uses parallelplot with added 'jitter' to diplay better view of combinations of
# argument options that lead to failure of a function
# TODO: allow to save as a *.PDF file for further analysis 
# (pdf allows for increased magnification)
plot.tests <- function(env=cont.env, DEBUG=FALSE, verbose=FALSE) 
{

    cont.env=env
    r <- cont.env$arg_register
    
    # prepare data for plotting, if data does not yet exist
    # if(is.null(cont.env$failure_map)) {
        # message("Data for plotting has not yet been generated. Generating data.")
        message("Generating data for plotting.")
        generate.analytics(env=cont.env)
    # }

    # if(is.null(cont.env$success_map)) {
        # message("Data for plotting has not yet been generated. Generating data.")
        message("Generating data for plotting.")
        generate.analytics(env=cont.env)
    # }

    message("Plotting combinations of argument options...")
    
    require(lattice)

    #--------------------------------------------------------------------------#
    if(verbose) message("preparing to draw 'failure map'")
    failure_map <- cont.env$failure_map

    nr_f <- nrow(failure_map)
    failure_map_parplot <- failure_map
    if(verbose) message(str(failure_map))

    browser(expr=DEBUG)

    i=0; while (i<nr_f) { i=i+1;
    # for(i in seq_along(nr)){
        # for(j in 1:ncol(failure_map_parplot)) {
            # opt_nbr_max <- length(r[[j]])
            # cat(opt_nbr_max)
            # The max value one can add and still avoid wrong association among
            # adjacent argument option numbers is 1: the only necessary condition is
            # ( jitter < 1 ) as factors/option numbers are separated by a distance 1
            
            # jitter <- (i / nr) / (9 - opt_nbr_max) #5 # also shrink by 3
            jitter <- (i / nr_f) / 4 # also shrink by 3

            # The max shift must NOT depend on the max number of options per
            # variable as the scaling is done by the plot function itself !
            # failure_map_parplot[i,j] <- failure_map_parplot[i,j] + jitter 
            failure_map_parplot[i,] <- failure_map_parplot[i,] + jitter 
        # }
    }
    failure_map_parplot
    #--------------------------------------------------------------------------#
    #--------------------------------------------------------------------------#
    if(verbose) message("preparing to draw 'success map'")
    success_map <- cont.env$success_map

    nr_s <- nrow(success_map)
    success_map_parplot <- success_map
    if(verbose) message(str(success_map))

    i=0; while (i<nr_s) { i=i+1;
    # for(i in seq_along(nr)){
        # for(j in 1:ncol(failure_map_parplot)) {
        # opt_nbr_max <- length(r[[j]])
        # cat(opt_nbr_max)
        # The max value one can add and still avoid wrong association among
        # adjacent argument option numbers is 1: the only necessary condition is
        # ( jitter < 1 ) as factors/option numbers are separated by a distance 1

        # jitter <- (i / nr) / (9 - opt_nbr_max) #5 # also shrink by 3
        jitter <- (i / nr_s) / 4 # also shrink by 3

        # The max shift must NOT depend on the max number of options per
        # variable as the scaling is done by the plot function itself !
        # failure_map_parplot[i,j] <- failure_map_parplot[i,j] + jitter
        success_map_parplot[i,] <- success_map_parplot[i,] + jitter
        # }
    }
    success_map_parplot
    #--------------------------------------------------------------------------#


    xlab.str <- names(cont.env$arg_register)
    
    # setnames(data, old=c() new=c()) in library(data.table)
    colnames(failure_map_parplot) <- xlab.str

    # TODO: print everything on the same chart:
    # reference: http://stackoverflow.com/questions/6853204/plotting-multiple-curves-same-graph-and-same-scale
    # par(new = TRUE)

    ## plotting several classes in various colors
    ## Ref: http://www.public.iastate.edu/~maitra/stat501/Rcode/parallelplot.R
    ##      see at the bottom !

    ## also, see argument 'groups'
    #  groups A variable or expression to be
    #  evaluated in data, expected to act as a grouping variable within each
    #  panel, typically used to distinguish different groups by varying graphical
    #  parameters like color and line type. Formally, if groups is specified, then
    #  groups along with subscripts is passed to the panel function,
    #-> good example for the 'group' parameter
    #   http://www.r-bloggers.com/conditioning-and-grouping-with-lattice-graphics/
    #
    #-> how to use groups:
    #    http://science.nature.nps.gov/im/datamgmt/statistics/r/graphics/lattice.cfm
    #    read that page, which includes the following, among other things:
    #    #---------------------------------------------------------------------#
    #    | y ~ x | b, groups=a    separate plots for each value of b of y ~ x, |
    #    | with dfferent colors or symbols for different values of a           |
    #    #---------------------------------------------------------------------#
    #

    if(verbose) message("drawing 'success map'")
    # draw 'success' parallelplot to properly set the scale for all axes
    if(0) {
    # par(new = TRUE)
        chart_success <-
            parallelplot(success_map_parplot, horizontal.axis=FALSE, col=rainbow(nr_s))
        print(chart_success)
    }

    if(verbose) message("drawing 'failure map'")
    # TODO: add arg. names as labels to the graph
    chart <- parallelplot(failure_map_parplot, horizontal.axis=FALSE, col=rainbow(nr_f),
                 main="Argument-Option Combinations Correlated with Function 'Error' Output",
                 scales=list(cex=1))
                 # main="Argument-Option Combinations Correlated with Function Failure")
    # print(chart_success)
    # par(new = TRUE)
    # plot(chart)
    print(chart)

    # splom(failure_map_parplot, horizontal.axis=FALSE, col=rainbow(nr))
    message("Done.")       
}




