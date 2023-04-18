## plot_tax_bar --------------------------------------------------------------

#' Create a Taxa Bar Chart
#'
#' \code{plot_tax_bar()} creates a taxa bar chart from the data frame generated
#' by \code{\link{make_phy_df}}.
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
#'   you're doing, use the data frame output by \code{\link{make_phy_df}}.
#' @param rank The taxonomic rank at which to view the data. Must be one of
#'   'Genus', 'Family', 'Order', 'Class', 'Phylum'. May not be a lower rank than
#'   the rank given to \code{\link{make_phy_df}}.
#' @param colours A character vector of colour names or hex values. Must have
#'   enough colours to accommodate all your taxa at the appropriate rank. If you
#'   don't provide one, there are a few internal vectors that have 21, 31, 60,
#'   or 70 colours that the function will try to use.
#' @param sample \code{'X.SampleID'} The name of the sample column in the data
#'   frame.
#' @param abund \code{'Abundance'} The name of the abundance column in the data
#'   frame.
#' @param legloc \code{'right'} Location of the legend. Can be 'right'
#'   (default), 'bottom', or 'none' (absent)
#' @param yscale \code{'lin'} Can be either 'lin' or 'sqrt'. The 'sqrt' plot can
#'   look weird.
#' @param means \code{FALSE} If \code{TRUE}, sets \code{position = fill} in the
#'   \code{geom_bar()} to constrain the abundances to sum to 1. Good to use if
#'   your \code{sample = } parameter is not actually sample names, but rather
#'   larger categories, to produce a plot of category means.
#' @export
plot_tax_bar = function(taxa_df,rank,colours = NULL,
					 sample = 'X.SampleID', abund = 'Abundance',
					 legloc = 'right', yscale = 'lin', means = FALSE){
    # Fed by make_phy_df()

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

	# Pick colours
	num = length(unique(taxa_df[,rank]))
	if (is.null(colours)){
        n = num/length(tax_colours)
        colours = c('grey69',rev(rep(tax_colours, ceiling(n))[1:num-1]))
	} else if (is.null(names(colours))) {
		colours = c('grey69',rev(colours[1:(num-1)]))
	}

	# Make sure the x axis is categorical
	taxa_df[,sample] = factor(taxa_df[,sample])

	# If the colour vector is named, make sure it is ordered like the factor
	# it's colouring
	if (!is.null(names(colours))) {
	    taxa_df[,rank] = droplevels(taxa_df[,rank])
	    if (!all(levels(taxa_df[,rank]) %in% names(colours))){
	        stop('The colour vector names need to match the rank levels')
	    }

	    colours = colours[levels(taxa_df[,rank])]
	}

	indiv = ggplot2::ggplot(taxa_df,
	                        ggplot2::aes(x = .data[[sample]],
	                                            y = .data[[abund]],
	                                            fill = .data[[rank]],
	                                            colour = .data[[rank]])) +
	    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
			axis.text.x = ggplot2::element_text(size = 10,
			                                    angle = 90,
			                                    hjust = 1,
			                                    vjust = 0.5)) +
	    ggplot2::scale_fill_manual(values = colours,
	                      guide = ggplot2::guide_legend(reverse = TRUE)) +
	    ggplot2::scale_colour_manual(values = colours,
	                        guide = ggplot2::guide_legend(reverse = TRUE)) +
	    ggplot2::ylab(paste("Relative Abundance (",rank,")\n",sep=''))
	if (means){
	    indiv = indiv + ggplot2::geom_bar(stat = 'identity', position = 'fill')
	} else {
	    indiv = indiv + ggplot2::geom_bar(stat = "identity")
	}

	if (yscale == 'sqrt') {
		indiv = indiv + ggplot2::scale_y_sqrt()
	}

	if (legloc == 'bottom'){
	    indiv = indiv + ggplot2::theme(legend.position = 'bottom')
	} else if (legloc == 'none') {
	    indiv = indiv + ggplot2::guides(fill = FALSE, colour = FALSE)
	}
	indiv + ggplot2::theme_bw()

	return(indiv)
}

