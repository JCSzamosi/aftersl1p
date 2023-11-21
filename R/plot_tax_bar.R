## plot_tax_bar --------------------------------------------------------------

#' Create a Taxa Bar Chart
#'
#' `plot_tax_bar()` creates a taxa bar chart from the data frame generated
#' by [make_phy_df()].
#'
#' @section Details: This function generates a ggplot object that is a
#'   first-pass, reasonable attempt at a taxon bar chart. The taxa are ordered
#'   by mean abundance and the 'Other' category is at the top. Because it's a
#'   regular ggplot object, you can add more geoms or facet it or manipulate it
#'   however you like using ggplot functions.
#'
#' @section Value: A ggplot object.
#'
#' @param tax_df The data frame used for plotting. Unless you really know what
#'   you're doing, use the data frame output by [make_phy_df()].
#' @param rank The taxonomic rank at which to view the data. Must be one of
#'   'Genus', 'Family', 'Order', 'Class', 'Phylum'. May not be a lower rank than
#'   the rank given to [make_phy_df()].
#' @param colours A character vector of colour names or hex values. Must have
#'   enough colours to accommodate all your taxa at the appropriate rank. If you
#'   don't provide one, there are a few internal vectors that have 21, 31, 60,
#'   or 70 colours that the function will try to use.
#' @param sample `'X.SampleID'` The name of the sample column in the data
#'   frame.
#' @param abund `'Abundance'` The name of the abundance column in the data
#'   frame.
#' @param legloc `'right'` Location of the legend. Passed directly to
#'   `ggplot2::theme(legend.position = legloc)` and can be any of "none",
#'   "left", "right", "top", or "bottom". If it is anything else,
#'   [ggplot2::theme()] will default to "none".
#' @param yscale `r lifecycle::badge("deprecated")` This function no longer
#'   supports y-axis scales other than linear.
#' @param means `FALSE` If `TRUE`, sets `position = fill` in the
#'   [ggplot2::geom_bar()] to constrain the abundances to sum to 1.
#'   Good to use if your `sample = ` parameter is not actually sample
#'   names, but rather larger categories, to produce a plot of category means.
#' @param r_ticks `FALSE` If `TRUE` x-axis tickmark text is rotated 90
#'   degrees and read bottom-to-top.
#' @export
plot_tax_bar = function(taxa_df,rank,colours = NULL,
					 sample = 'X.SampleID', abund = 'Abundance',
					 legloc = 'right', yscale = 'lin',
					 means = FALSE, r_ticks = FALSE, leglen = NULL){
   # Lifecycle Management
    if (yscale != 'lin'){
        lifecycle::deprecate_warn('0.1.0', 'plot_tax_bar(yscale)',
                        details = paste('The ability to set non-linear y-axis',
                                        'scaling will be removed soon.'))
    }

	# Check the inputs
	if (!(sample %in% names(taxa_df))){
		stop('sample argument must be one of the columns in your data frame')
	}

	if (!(abund %in% names(taxa_df))){
		stop('abund argument must be one of the columns in your data frame')
	}

	if (!(yscale %in% c('lin','log','sqrt'))){
		stop('yscale argument must be one of \'lin\', \'log\', or \'sqrt\'')
	}
    if (!(rank %in% names(taxa_df))){
        stop('rank argument must be one of the columns in your data frame')
    }
	# Pick colours
	num = length(unique(taxa_df[,rank]))
	if (is.null(colours)){
        n = num/length(tax_colours)
        colours = c('grey69',rev(rep(tax_colours, ceiling(n))[1:num-1]))
        names(colours) = levels(taxa_df[,rank])
	} else if (is.null(names(colours))) {
	    if (length(colours) < (dplyr::n_distinct(taxa_df[,rank])-1)){
	        warn(paste('The supplied colour vector is shorter than the number',
	                   'of distinct taxa.'))
	    }
		colours = c('grey69',rev(colours[1:(num-1)]))
		names(colours) = levels(taxa_df[,rank])
	} else {
	    if (length(colours) < dplyr::n_distinct(taxa_df[,rank])){
	        warn(paste('The supplied colour vector is shorter than the number',
	                   'of distinct taxa.'))
	    }
	}

	# Check legend length
	if (is.null(leglen)){
	    leglen = num
	} else if (!is.numeric(leglen)) {
	    stop(paste("leglen must be an integer"))
	} else if (leglen > num){
	    warn(paste("leglen is greater than the number of unique taxa.",
	                "showing all taxa."))
	    leglen = num
	} else if (leglen == 0){
        legloc = 'none'
        leglen = num
	} else if (leglen < 0){
        warn(paste('leglen can not be negative. treating it like 0.'))
	    legloc = 'none'
	    leglen = num
	}

	# Check for means
	mn_chk = (taxa_df
	          %>% dplyr::group_by(.[,sample])
	          %>% dplyr::summarize(.chksms = sum(Abundance, na.rm = TRUE)))
	tol = 1e-5
	if (any(mn_chk$.chksms - 1 > tol) & !means){
	    warn(paste('Your per-sample abundances sum to >1. Did you mean to',
	               'specify \'means = TRUE\'?'))
	}

	# Make sure the x axis is categorical
	taxa_df[,sample] = factor(taxa_df[,sample])

	# If the colour vector is named, make sure it is ordered like the factor
	# it's colouring
	if (!is.null(names(colours))) {
	    if ((length(colours) < dplyr::n_distinct(taxa_df[,rank])) &
	        all(names(colours) %in% droplevels(taxa_df[,rank]))){
	        NULL
	    } else {
	        taxa_df[,rank] = droplevels(taxa_df[,rank])
	        if (!all(levels(taxa_df[,rank]) %in% names(colours))){
	            stop('The colour vector names need to match the rank levels')
	        }
	    }

	    colours = colours[levels(taxa_df[,rank])]
	}

	brks = c('Other', rev(rev(levels(taxa_df[,rank]))[1:(leglen-1)]))
	# Generate the plot
	indiv = ggplot2::ggplot(taxa_df,
	                        ggplot2::aes(x = .data[[sample]],
	                                            y = .data[[abund]],
	                                            fill = .data[[rank]],
	                                            colour = .data[[rank]])) +
	    ggplot2::theme_bw() +
	    # adjust the title and axis text
	    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
	                   # position the legend
	                   legend.position = legloc) +
	    # use the right colours
	    ggplot2::scale_fill_manual(values = colours,
	                               breaks = brks,
	                           guide = ggplot2::guide_legend(reverse = TRUE)) +
	    ggplot2::scale_colour_manual(values = colours,
	                                 breaks = brks,
	                            guide = ggplot2::guide_legend(reverse = TRUE)) +
	    # label the axis
	    ggplot2::ylab(paste("Relative Abundance (",rank,")\n",sep=''))
	if (means){
	    # use mean abundance across a group
	    indiv = indiv + ggplot2::geom_bar(stat = 'identity', position = 'fill')
	} else {
	    # individual samples
	    indiv = indiv + ggplot2::geom_bar(stat = "identity")
	}

	# rotate the ticks
	if (r_ticks){
	    indiv = indiv + rotate_ticks()
	}

	if (yscale == 'sqrt') {
	    # square-root transform the y-axis. You probably don't want this. It should
	    # be deprecated.
		indiv = indiv + ggplot2::scale_y_sqrt()
	}


	return(indiv)
}

