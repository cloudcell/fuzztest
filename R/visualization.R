
# References:
#
# TODO: add viridis colormap
#       http://matplotlib.org/users/colormaps.html
#
# TODO: consider using prp() plot from rpart package
#       http://www.milbo.org/rpart-plot/prp.pdf

# TODO: plot all the 'PASS' tests with a transparent ink so that the vertical
#       scale of the axes correctly corresponds to the category ordered position

# TODO: use this for shuffling columns & other neat stuff !
#       https://syntagmatic.github.io/parallel-coordinates/

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
    if(verbose) message("Transformed test results saved in the test environment")
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
plot.tests <- function(env=cont.env, suppress_pass=FALSE, DEBUG=FALSE, verbose=FALSE)
{

    cont.env=env
    r <- cont.env$arg_register

    # prepare data for plotting, if data does not yet exist
    # if(is.null(cont.env$failure_map)) {
    # message("Data for plotting has not yet been generated. Generating data.")
    if(verbose) message("Generating data for plotting.")
    generate.analytics(env=cont.env)
    # }

    # if(is.null(cont.env$success_map)) {
    # message("Data for plotting has not yet been generated. Generating data.")
    if(verbose) message("Generating data for plotting.")
    generate.analytics(env=cont.env)
    # }

    if(verbose) message("Plotting combinations of argument options...")

    require(lattice)

    #--------------------------------------------------------------------------#
    if(verbose) message("preparing to draw 'failure map'")
    failure_map <- cont.env$failure_map

    nr_f <- nrow(failure_map)
    failure_map_parplot <- failure_map
    if(verbose) message(str(failure_map))

    i=0; while (i<nr_f) { i=i+1;
    # for(i in seq_along(nr)){
        # for(j in 1:ncol(failure_map_parplot)) {
            # opt_nbr_max <- length(r[[j]])
            # cat(opt_nbr_max)
            # The max value one can add and still avoid wrong association among
            # adjacent argument option numbers is 1: the only necessary condition is
            # ( jitter < 1 ) as factors/option numbers are separated by a distance 1

            # jitter <- (i / nr) / (9 - opt_nbr_max) #5 # also shrink by 3
            jitter <- ((i-1) / nr_f) / 4 # also shrink by 3 ( beginning with 0!)

            # The max shift must NOT depend on the max number of options per
            # variable as the scaling is done by the plot function itself !
            # failure_map_parplot[i,j] <- failure_map_parplot[i,j] + jitter
            failure_map_parplot[i,] <- failure_map_parplot[i,] + jitter
        # }
    }
    if(verbose) str(failure_map_parplot)
    failure_map_parplot <- as.data.frame(failure_map_parplot)
    cont.env$failure_map_parplot <- failure_map_parplot # TODO delete later (debug only)

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
        jitter <- ((nr_s - (i - 1) / nr_s) / 4 # also shrink by 3 
        # shifted up a bit 

        # The max shift must NOT depend on the max number of options per
        # variable as the scaling is done by the plot function itself !
        # failure_map_parplot[i,j] <- failure_map_parplot[i,j] + jitter
        success_map_parplot[i,] <- success_map_parplot[i,] - jitter
        # }
    }
    if(verbose) str(success_map_parplot)
    success_map_parplot <- as.data.frame(success_map_parplot)
    cont.env$success_map_parplot <- success_map_parplot # TODO delete later (debug only)
    #--------------------------------------------------------------------------#


    xlab.str <- names(cont.env$arg_register)
    
    # setnames(data, old=c() new=c()) in library(data.table)
    colnames(failure_map_parplot) <- xlab.str
    colnames(success_map_parplot) <- xlab.str

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
# if(0) {
#     if(verbose) message("drawing 'success map'")
#     # draw 'success' parallelplot to properly set the scale for all axes
#     if(1) {
#     # par(new = TRUE)
#         chart_success <-
#             parallelplot(success_map_parplot, horizontal.axis=FALSE, col="grey",
#                          main="Argument-Option Combinations Correlated with Test Result 'PASS'", #(nr_s)
#                          drop.unused.levels=FALSE)
#         # print(chart_success)
#         print(chart_success, more=TRUE)
#     }
# 
#     if(verbose) message("drawing 'failure map'")
#     # TODO: add arg. names as labels to the graph
#     chart <- parallelplot(failure_map_parplot, horizontal.axis=FALSE, col=rainbow(nr_f),
#                  main="Argument-Option Combinations Correlated with Test Result 'FAIL'",
#                  drop.unused.levels=FALSE)
#                  # ,                 scales=list(cex=1))
#                  # main="Argument-Option Combinations Correlated with Function Failure")
#     # par(new = TRUE)
#     # plot(chart)
#     print(chart)#, more=TRUE) # finalize printing
#     print(chart_success)#, more=TRUE)
# }
    if(verbose) message("drawing 'full test map'")
    # it is important to have this 'stacked' pass/fail approach so PASS always
    # appearth underneath the FAIL. i.e. so that FAIL lines are never 
    # occluded by PASS lines on the graph.
    if(nr_s>0) {
        color_pass <- as.character("#10222222")
        pss <- cbind(success_map_parplot,fld="PASS", 
                     color=color_pass, stringsAsFactors=FALSE) # grey
    }
    
    if(nr_f>0) {
        # subrange: red = 0, yellow = 1/6, green = 2/6, cyan = 3/6, blue = 4/6 and magenta = 5/6.
        color_fail <- rainbow(nrow(failure_map_parplot), start = 5/6, end = 4/6)
        fal <- cbind(failure_map_parplot,fld="FAIL", 
                     color=color_fail, stringsAsFactors=FALSE)
    }
    
    if( nr_s>0 && nr_f>0 ) {
        all <- rbind(pss,fal)
    } else if (nr_f>0) {
        all <- fal
    } else if (nr_s>0) {
        all <- pss
    } else {
        stop("No test results are available for graphing.")
    }
    
    nr_all <- nr_s + nr_f
        
    if(verbose) message( str(all) )
    if(verbose) message( tail(all) )
    # color <- rainbow(nrow(all))
    # color <- rainbow(0)
    # color[which(all[,'fld']=="PASS")] <- "#10222222" # grey
    # str(all)

    browser(expr=DEBUG)
        
    if(nr_all>1) {
    col_qty <- length(xlab.str)
    chart_all <- parallelplot(~all[1:col_qty], horizontal.axis=FALSE, col=all[,'color'],
                              main="Argument Combinations vs Test Results (PASS - grey, FAIL - color)",
                              drop.unused.levels=FALSE)
    # chart_all <- parallelplot(all, horizontal.axis=FALSE, col=rainbow(10))
    print(chart_all)
    } else {
        message("Not enough data: two or more test results are required.")
        # warning("Two or more tests are required for the graph.")
    }
    
    # splom(failure_map_parplot, horizontal.axis=FALSE, col=rainbow(nr))
    if(verbose) message("Done.")       
}




