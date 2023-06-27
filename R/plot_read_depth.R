#### plt_read_depth ------------------------------------------------------------

#' View read depth
#'
#' @section Details: Creates a read depth boxplot
#'
#' @section Value: A [ggplot2] object
#'
#' @param physeq A phyloseq object
#' @param xvar `NULL` A variable (column name) from `sample_data(physeq)` by
#'   which to group samples on the x-axis. Must be categorical. If `NULL`, no
#'   grouping will be performed.
#' @param lin `FALSE` If `TRUE`, the y-axis will be on a linear scale. By
#'   default it is log-scaled.
#' @param cvar `NULL` A variable (column name) from `sample_data(physeq)` by
#'   which the points should be coloured. If `NULL`, all points will be black.
#'   May be continuous or categorical. See the `clrs` argument if you want to
#'   specify your colours.
#' @param clrs `NULL` If the `cvar` parameter is set and this is unset, colours
#'   will be taken from [tax_colours] if `cvar` is categorical and use
#'   [ggplot2]'s default (blue) if `cvar` is continuous. If `cvar` is
#'   categorical, you may provide a named colour vector to use instead of
#'   [tax_colours] and if it is continuous you may provide a two-colour vector.
#'   The vector must have names "low" and "high". If `cvar` is NOT set, you may
#'   use this parameter to provide a single colour name or hex code that will be
#'   applied to all points.
#'
plt_read_depth = function(physeq, xvar = NULL, lin = FALSE, cvar = NULL,
                          clrs = NULL){
    # Check inputs
    sample_sum_df = data.frame(Total = sample_sums(physeq),
                               sample_data(physeq))

    # Are cvar and xvar columns?
    if (!is.null(xvar) & !(xvar %in% colnames(sample_sum_df))){
        stop(paste('"xvar" must be one of the columns in sample_data(physeq).'))
    }

    if (!is.null(cvar) & !(cvar %in% colnames(sample_sum_df))){
        stop(paste('"cvar" must be one of the columns in sample_data(physeq).'))

    # Deal with the colour vector
    }

    if (is.null(cvar) & !is.null(clrs)){
        if (length(cvar) > 1){
            warn(paste('With no cvar specified, only the first colour in "clrs"',
                       'will be used.'))
            clrs = clrs[1]
        }
    } else if (!is.null(cvar)){
        # Built-in colour vector
        if (is.null(clrs)){
            # Categorical
            if (!is.numeric(sample_sum_df[[cvar]])){
                n = n_distinct(sample_sum_df[[cvar]])
                nx = ceiling(n/length(tax_colours))
                clrs = rep(tax_colours, nx)
            } # continuous requires no action
        # User-specified colour vector
        } else {
            # Categorical
            if (!is.numeric(sample_sum_df[[cvar]])){
                if (is.null(names(clrs))){
                    NULL
                    # nothing to do here. use it as is
                } else if (!all(unique(sample_sum_df) %in% names(clrs))){
                    stop(paste('If "clrs" is named, its names must contain',
                               'all the values in the "cvar" column'))
                }
                # if we get this far, the colour vector is fine.

            # Continuous
            } else {
                if (is.null(names(clrs)) ||
                    !all(c('high','low') %in% names(cvar))){
                    stop(paste('With a continuous "cvar", the provided colour',
                              'vector "clr" must have names "high" and "low".'))
                }
                # if we get this far, the colour vector is fine.
            }
        }
    }




    }

    # Does clrs go with cvar?



    # Construct the plot foundation with or without a grouping variable
    if (is.null(xvar)){
        depth_plot = ggplot(sample_sum_df, aes(x = 'All samples',y = Total))
    } else {
        depth_plot = ggplot(sample_sum_df, aes(x = .data[[xvar]], y = Total))
    }

    # Add the jittered points, taking colour into account
    # No colouring variable
    if (is.null(cvar)){
        # And no specified colour
        if (is.null(clrs)){
            depth_plot = depth_plot + geom_jitter(width = 0.2)
            # And colour is too long
        } else if (length(clrs) > 1){
            warn(paste('With no cvar specified, only the first colour in "clrs"',
                       'will be used.'))
            depth_plot = depth_plot + geom_jitter(width = 0.2, colour = clrs[1])
            # And colour is specified
        } else {
            depth_plot = depth_plot + geom_jitter(width = 0.2, colour = clrs)
        }
        # colouring variable is specified and continuous
    } else if (is.numeric(sample_sum_df[[cvar]])) {
        depth_plot = depth_plot + geom_jitter(width = 0.2,
                                              aes(colour = .data[[cvar]]))
        # if clrs not specified, do nothing. defaults will do.
        if (is.null(clrs)){
            NULL
            # if clrs is specified, check that it is correctly named
        } else if (all(c('low','high') %in% names(clrs))){
            depth_plot = depth_plot + scale_colour_gradient(low = clrs['low'],
                                                            high = clrs['high'])
            # if clrs is mis-specified, stop
        } else {
            stop(paste('The colour vector does not conform to specifications.',
                       'See ?plt_read_depth for more information.'))
        }
     # if colouring variable is specified but not continuous, check if clrs is specified
    } else if (is.null(clrs)){
        depth_plot = depth_plot + geom_jitter(width = 0.2,
                                              aes(colour = .data[[cvar]])) +
            scale_colour_manual(values = tax_colours)

    # If the colour vector is unnamed, just use it
    } else if (is.null(names(clrs))){
        depth_plot = depth_plot + geom_jitter(width = 0.2,
                                              aes(colour = .data[[cvar]])) +
            scale_colour_manual(values = clrs)
    }
    if (!lin){
        depth_plot = depth_plot + scale_y_log10()
    }
    return(depth_plot)
}

#### plot_read_depth -----------------------------------------------------------

#' Plot read depth
#'
#' Creates a read depth histogram.
#' @param physeq A phyloseq object
#' @export
plot_read_depth = function(physeq){
    sample_sum_df = data.frame(Total = sample_sums(physeq),
                               sample_data(physeq))
    depth_plot = ggplot(sample_sum_df, aes(x = Total)) +
        geom_histogram(colour = 'black', fill = 'grey57') +
        scale_x_log10() +
        xlab('Log10 read depth') +
        ylab('Number of samples') +
        ggtitle('Distribution of read depths') +
        theme_bw()
    return(depth_plot)
}
