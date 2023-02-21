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
#' @param sample The name of the sample column in the data frame.
#' @param abund The name of the abundance column in the data frame.
#' @param legloc Location of the legend. Can be 'right' (default), 'bottom', or
#'   'none' (absent)
#' @param yscale Can be either 'lin' or 'sqrt'. The 'sqrt' plot can look weird.
#' @param means If \code{TRUE}, sets \code{position = fill} in the
#'   \code{geom_bar()} to constrain the abundances to sum to 1. Good to use if
#'   your \code{sample = } parameter is not actually sample names, but rather
#'   larger categories, to produce means.
#' @export
plot_tax_bar = function(taxa_df,rank,colours = NULL,
					 sample = 'X.SampleID', abund = 'Abundance',
					 legloc = 'right', yscale = 'lin', means = FALSE){
	## taxa_df:	The data frame produced by taxa_other_df()
	## rank: The taxonomic rank to plot by
	## colours:	A character vector with the right number of colours. If you
	## don't provide one it uses my 21-colour vector, which might not be enough
	## left NULL, the bars will be ordered alphabetically by sample ID.
	## sample: the name of the sample ID column.
	## abund: the name of the abundance column

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
	if (is.null(colours)){
		num = length(unique(taxa_df[,rank]))
		if (num <= 22){
			colours = c('grey69',rev(cols_21[1:(num-1)]))
		} else if (num <= 31) {
			colours = c('grey69',rev(cols_31[1:(num-1)]))
		} else if (num <= 61) {
			colours = c('grey69',rev(cols_60[1:(num-1)]))
		} else if (num <= 71) {
			colours = c('grey69',rev(cols_70[1:(num-1)]))
		} else {
			stop('I can\'t handle more than 71 ranks. Please provide a colour vector.')
		}
		# colours = case_when(
							#num <= 22 ~ c('grey69',cols_21),
							#num <= 32 ~ c('grey69',cols_31),
							#num <= 61 ~ c('grey69',cols_60),
							#num > 61 ~ c('grey69',cols_70))
	} else if (is.null(names(colours))) {
	    num = length(unique(taxa_df[,rank]))
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

	indiv = ggplot(taxa_df, aes_string(x = sample, y = abund, fill = rank,
									   colour = rank)) +
	    theme(axis.title.x = element_blank(),
			axis.text.x = element_text(size = 10,
									   angle = 90,
									   hjust = 1,
									   vjust = 0.5)) +
	    scale_fill_manual(values = colours,
	                      guide = guide_legend(reverse = TRUE)) +
	    scale_colour_manual(values = colours,
	                        guide = guide_legend(reverse = TRUE)) +
	    ylab(paste("Relative Abundance (",rank,")\n",sep=''))
	if (means){
	    indiv = indiv + geom_bar(stat = 'identity', position = 'fill')
	} else {
	    indiv = indiv + geom_bar(stat = "identity")
	}

	if (yscale == 'sqrt') {
		indiv = indiv + scale_y_sqrt()
	}

	if (legloc == 'bottom'){
	    indiv = indiv + theme(legend.position = 'bottom')
	} else if (legloc == 'none') {
	    indiv = indiv + guides(fill = FALSE, colour = FALSE)
	}
	indiv + theme_bw()

	return(indiv)
}

