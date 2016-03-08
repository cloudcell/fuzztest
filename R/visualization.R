############################################################################## #
# Description: a visualization function that uses parallel coordinates 
#              to plot argument options as levels along each axis. 
#              The function plots argument value combinations that are 
#              associated with successful ('PASS') tests first with failing 
#              tests afterwards so failing test settings (i.e. their graph 
#              representation) are never overlapped by successful test settings.
#              It is possible to plot either 'PASS' or 'FAIL' settings alone
#              (see the description of the plot.test() function).
#              
# Author: cloudcell
# Date: 2016-03
# License: GPL-3
############################################################################## #

# References:
#
# a quote from https://en.wikipedia.org/wiki/Parallel_coordinates
# "
# When used for statistical data visualisation there are three important
# considerations: the order, the rotation, and the scaling of the axes.
#
# The order of the axes is critical for finding features, and in typical data
# analysis many reorderings will need to be tried. Some authors have come up
# with ordering heuristics which may create illuminating orderings.
# "

# TODO: consider using prp() plot from rpart package
#       http://www.milbo.org/rpart-plot/prp.pdf
# TODO: use this for shuffling columns & other neat stuff !
#       https://syntagmatic.github.io/parallel-coordinates/


# internal function
# generates analytics for plotting (internal function)
generate.analytics <- function(env=cont.env, verbose=FALSE, DEBUG=FALSE)
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



#' Plots test results
#' 
#' uses parallelplot with added spacing to diplay combinations of argument 
#' values groupped by PASS/FAIL exit state
#' 
#' @param env work environment, set only if default environment is not used
#' @param pass determines whether the chart includes the full test set data.
#'             I.e. whether all of the 'PASS' tests are included (default is 
#'             TRUE). If set to FALSE, all the PASS data will not be drawn, 
#'             which sometimes might speed up the drawing process.)
#' @param fail determines whether the chart includes the full test set data.
#'             I.e. whether all of the 'PASS' tests are included (default is 
#'             TRUE). If set to FALSE, all the PASS data will not be drawn, 
#'             which sometimes might speed up the drawing process.)
#' @param dist the minimum distance between two adjacent lines from different
#'             'levels' within the same 'category' at a point of intersection 
#'             with the category ('argument') axis. The default of 1.0
#'             makes a gap between any two adjacent groups of lines 
#'             sufficient for a maximum possible number of lines to pass through 
#'             without overlapping adjacent lines at the point of intersection
#'             with the axis. Distance of 0 makes no gap (with only a 
#'             regular distance separating any two adjacent lines at a point
#'             of intersection with the vertical axis). A dist of -0.9999 
#'             creates almost complete overlap with an adjacent group.
#'             The value of dist must be greater than -1. 
#'             (TODO: for the parameter dist to work properly, it is necessary
#'             to calculate max_levels as a product of n-1 largest numbers
#'             of options for each argument). 
#' @param verbose provides additional text output during processing
#' @param DEBUG enters the debug mode on function entry
#' 
#' @author cloudcell
#' 
#' @export             
# TODO: allow to save as a *.PDF file for further analysis
# (pdf allows for increased magnification)
plot_tests <- function(env=cont.env, pass=TRUE, fail=TRUE, dist=1.0,
                       verbose=FALSE, DEBUG=FALSE)
{
    browser(expr=DEBUG)
    
    cont.env=env
    r <- cont.env$arg_register

    if(dist<=-1) stop("The value of dist must be greater than -1")
    
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
    if(verbose) message("preparing 'preliminaries'")
    failure_map <- cont.env$failure_map
    nr_f <- nrow(failure_map)

    success_map <- cont.env$success_map
    nr_s <- nrow(success_map)
    
    nr_all <- nr_s + nr_f + 2 # 2 == rn_boundary
    
    # col. names
    xlab.str <- names(cont.env$arg_register)
    
    #--------------------------------------------------------------------------#
    # preparing a laddered shift counter for the graph
    graph_shift <- list()
    max_levels <- 1
    min_levels <- 0
    total_product <- 1
    for (i in 1:length(r)) {
        level_qty <- length(r[[i]])
        if(verbose) message("level_qty= ",level_qty)
        
        # need this to calculate the max number of lines passing
        # through each 'graph' groupping
        total_product <- total_product * level_qty

        if(min_levels<1 && min_levels<level_qty && level_qty>1) {
           min_levels <- level_qty
        }
        if(min_levels>1 && min_levels>level_qty) {
           min_levels <- level_qty
        }

        if(max_levels<level_qty) { 
            max_levels <- level_qty 
        }
        
        graph_shift[[i]] <- vector(mode = "numeric", length = level_qty)
        if(verbose) message("graph_shift= ", graph_shift)
    }
    
    
    # need this to calculate the max number of lines passing
    # through each 'graph' groupping
    max_levels_mult <- total_product/min_levels # min non-1-option levels

        
    level_scale <- vector(mode="numeric",length=length(r))
    for (i in 1:length(r)) {
        level_qty <- length(r[[i]])
        if(verbose) message("level_qty= ",level_qty)
        
        level_scale[i] <- total_product/level_qty
    }
    
    
    axes_names <- names(r)
    graph_shift <- setNames(graph_shift, axes_names)
    
    # duplicate the structure: the duplicate will be used to calculate the
    # size of a shift based on the total number of 'hits' in each 'node'
    graph_shift_base <- graph_shift
    
    # browser()
    #--------------------------------------------------------------------------#
    # preparing data related to exit status 'FAIL'

    if(verbose) message("preparing 'failure map'")
    failure_map_parplot <- failure_map
    if(verbose) message(str(failure_map))

    if(0) {
        # count the number of times each argument option gets 'hit'
        i=0; while (i<nr_f) { 
            i=i+1;
            for(j in 1:ncol(failure_map_parplot)) {  # TODO: take a table with ALL results (better style)
                # take a "clean level" as a coordinate 
                level <- failure_map_parplot[i,j]
                graph_shift_base[[j]][level] <- graph_shift_base[[j]][level] + 1
            }
        }
    }
    
    i=0; while (i<nr_f) { i=i+1;
        for(j in 1:ncol(failure_map_parplot)) {  # TODO: take a table with ALL results (better style)
            
            # take a "clean level" as a coordinate before adjusting it
            # to be come an "adjusted level" to be put on the plot 
            level <- failure_map_parplot[i,j]
            
            failure_map_parplot[i,j] <- failure_map_parplot[i,j] + graph_shift[[j]][level]
            
            # gap <- (1 / (nr_s+nr_f)) / 3 # also shrink by 3 ( beginning with 0!)
            
            # gap <- ( 1 / graph_shift_base[[j]][level] ) / 3
            # gap <- ( 1 / max_levels ) / 3
            # gap <- ( 1 / max_levels ) / (1 + dist) # TODO get it out of the loop!
            
            # gap <- ( 1 / max_levels_mult ) / (1 + dist) # TODO get it out of the loop!
            gap <- ( 1 / level_scale[j] ) / (1 + dist) # TODO get it out of the loop!
            
            # message("col j = ", j, " row i= ", i, " total gap = ", graph_shift[[j]][level])
            
            # plotting starts from with no additional gap at all
            graph_shift[[j]][level] <- graph_shift[[j]][level] + gap # prepares coord. for the next point
        }
    }
    parplot_boundary_f <- failure_map_parplot[i,]
    
    if(verbose) str(failure_map_parplot)
    failure_map_parplot <- as.data.frame(failure_map_parplot)
    cont.env$failure_map_parplot <- failure_map_parplot # TODO delete later (debug only)

    colnames(failure_map_parplot) <- xlab.str
    
    
    #--------------------------------------------------------------------------#
    # preparing data related to exit status 'PASS'

    if(verbose) message("preparing 'success map'")
    success_map_parplot <- success_map
    if(verbose) message(str(success_map))

    if(0) {
        # count the number of times each argument option gets 'hit'
        i=0; while (i<nr_s) { 
            i=i+1;
            for(j in 1:ncol(success_map_parplot)) {  # TODO: take a table with ALL results (better style)
                # take a "clean level" as a coordinate 
                level <- success_map_parplot[i,j]
                graph_shift_base[[j]][level] <- graph_shift_base[[j]][level] + 1
            }
        }
    }
    
    i=0; while (i<nr_s) { i=i+1;
        for(j in 1:ncol(success_map_parplot)) { # TODO: take a table with ALL results (better style)
            
            # take a "clean level" as a coordinate before adjusting it
            # to be come an "adjusted level" to be put on the plot 
            level <- success_map_parplot[i,j]
            
            success_map_parplot[i,j] <- success_map_parplot[i,j] + graph_shift[[j]][level] 

            # gap <- (1 / (nr_s+nr_f)) / 3 # also shrink by 3 ( beginning with 0!)
            
            # gap <- ( 1 / graph_shift_base[[j]][level] ) / 3
            # gap <- ( 1 / max_levels ) / 3
            # gap <- ( 1 / max_levels ) / (1 + dist) # TODO get it out of the loop!
            
            # gap <- ( 1 / max_levels_mult ) / (1 + dist) # TODO get it out of the loop!
            gap <- ( 1 / level_scale[j] ) / (1 + dist) # TODO get it out of the loop!
            
            # plotting starts from with no additional gap at all
            graph_shift[[j]][level] <- graph_shift[[j]][level] + gap # prepares coord. for the next point
            
            # message("col j = ", j, " row i= ", i, " total gap = ", graph_shift[[j]][level])
            
        }
    }
    parplot_boundary_p <- success_map_parplot[i,] # 'p' == 'pass'
    
    if(verbose) str(success_map_parplot)
    success_map_parplot <- as.data.frame(success_map_parplot)
    cont.env$success_map_parplot <- success_map_parplot # TODO delete later (debug only)

    colnames(success_map_parplot) <- xlab.str
    #--------------------------------------------------------------------------#

    if(verbose) message("drawing 'full test map'")
    
    # browser()
    
    #--------------------------------------------------------------------------#
    # determine the two 'line coordinates' for the boundary (top & bottom):

    # min/max boundaries based on actual data
    # tmp <- rbind(parplot_boundary_p,parplot_boundary_f)
    # parplot_boundary_f <- apply(X = failure_map_parplot, MARGIN = 2, FUN = max) # could be taken from arg_register
    # parplot_boundary_p <- apply(X = success_map_parplot, MARGIN = 2, FUN = max) # could be taken from arg_register
    
    # tmp <- rbind(parplot_boundary_f, parplot_boundary_p)
    tmp <- rbind(failure_map_parplot, success_map_parplot)
    
    parplot_boundary_max <- apply(X = tmp, MARGIN = 2, FUN = max) # could be taken from arg_register
    
    parplot_boundary_max <- t(parplot_boundary_max)
    parplot_boundary_max <- as.data.frame(parplot_boundary_max, row.names = "0")
    # colnames(parplot_boundary_max) <- xlab.str
    
    # simply copy to use as a ready template
    parplot_boundary_min <- parplot_boundary_max
    
    # the minimum group number should always == 1, for all args !
    parplot_boundary_min[1,] <- 1
    
    
    # do not change this order
    parplot_boundaries <- rbind(parplot_boundary_min, parplot_boundary_max)
    
    # add 2 col's - status(pass/fail/'TECH'-nical) & color:
    
    # color_technical <- as.character("#FF999999") # TODO: change to all 'F' - this is for debugging only
    # color_technical <- as.character("#00000000") # TODO: change to all 'F' - this is for debugging only
    # color_technical <- as.character("red") # TODO: change to all 'F' - this is for debugging only
    # color_technical <- as.character("#FcFcFcFc") # TODO: change to all 'F' - this is for debugging only
    # color_technical <- as.character("#05110505") # TODO: change to all 'F' - this is for debugging only
    
    # close to pure white not to be confused with a test 'line' !
    color_technical <- as.character("#fefefefe") # TODO: change to all 'F' - this is for debugging only
                                                 # TODO: find out what the first #XX stands for, alpha ???
    # order: {min, max} color
    tech <- cbind(parplot_boundaries, fld="TECH", 
                 color=color_technical, stringsAsFactors=FALSE) # grey
        
    
    #--------------------------------------------------------------------------#
    # assigning technical fields for pass & fail data
    
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
    
    #--------------------------------------------------------------------------#
    # binding all data into one chunk for plotting
    
    if( nr_s>0 && nr_f>0 ) {
        if(pass && fail) {
            # all <- rbind(tech[1,], pss, fal, tech[2,])
            all <- rbind(tech, pss, fal)
        } else if (pass) {
            # all <- rbind(tech[1,], pss, tech[2,])
            all <- rbind(tech, pss)
        } else if (fail) {
            # all <- rbind(tech[1,], fal, tech[2,])
            all <- rbind(tech, fal)
        } else {
            all <- tech
        }
    } else if (nr_f>0) {
        if(pass && fail) {
            # all <- rbind(tech[1,], fal, tech[2,])
            all <- rbind(tech, fal)
        } else if (pass) {
            all <- tech
        } else if (fail) {
            # all <- rbind(tech[1,], fal, tech[2,])
            all <- rbind(tech, fal)
        } else {
            all <- tech
        }
    } else if (nr_s>0) {
        if(pass && fail) {
            # all <- rbind(tech[1,], pss, tech[2,])
            all <- rbind(tech, pss)
        } else if (pass) {
            # all <- rbind(tech[1,], pss, tech[2,])
            all <- rbind(tech, pss)
        } else if (fail) {
            all <- tech
        } else {
            all <- tech
        }
    } else {
        stop("No test results are available for graphing.")
    }
    
    # browser()
    
    # nr_all <- nr_s + nr_f + 2 # 2 == rn_boundary
        
    if(verbose) message( str(all) )
    if(verbose) message( tail(all) )
    # color <- rainbow(nrow(all))
    # color <- rainbow(0)
    # color[which(all[,'fld']=="PASS")] <- "#10222222" # grey
    # str(all)

    
    #--------------------------------------------------------------------------#
    # plotting
        
    if(nr_all>1) { # TODO consider removing !!! due to added boundaries (min/max)
    col_qty <- length(xlab.str)
    chart_all <- parallelplot(~all[1:col_qty], horizontal.axis=FALSE, col=all[,'color'],
                              main="parameter combinations vs test results: grey=pass, colored=fail",
                              drop.unused.levels=FALSE) # lvls of factors
    # chart_all <- parallelplot(all, horizontal.axis=FALSE, col=rainbow(10))
    print(chart_all)
    } else {
        message("Not enough data: two or more test results are required.")
        # warning("Two or more tests are required for the graph.")
    }
    
    # splom(failure_map_parplot, horizontal.axis=FALSE, col=rainbow(nr))
    if(verbose) message("Done.")       
}


#---sandbox--------------------------------------------------------------------#
#
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


