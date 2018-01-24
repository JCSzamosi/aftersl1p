################################################################################
### Functions to generate summarizing graphics
################################################################################

### Deal with data -------------------------------------------------------------
#### Deal with taxa ------------------------------------------------------------

##### prop_tax_row -------------------------------------------------------------
#' Propagate taxon information in a single row of a \code{tax_table} object.
#'
#' \code{prop_tax_row} Takes the taxon assignment from the lowest assigned level
#' in an OTU's assignment and fills it in to all the lower, unresolved fields.
#'
#' @section Details:
#' Often taxon cannot be assigned all the way down to genus from the limited
#' information available from a given variable region of 16S (or other marker
#' gene). If an OTU has been assigned to Order Enterobacteriales, but the Family
#' and genus cannot be resolved, then the Family and Genus fields are left
#' blank by QIIME. This function fills those fields with the string
#' 'o_Enterobacteriales', so they are not parsed as NAs by ggplot and other
#' functions.
#'
#' @param taxrow A single row in a taxon table
#'
#' @keywords internal
prop_tax_row = function(taxrow){
	## taxrow: a row from a tax_table of a phyloseq object

	ranks = c('Phylum','Class','Order','Family','Genus')

	hasNA = FALSE
	for (i in 1:length(ranks)){

	    if (!is.na(taxrow[,ranks[i]])){
	        tax = taxrow[,ranks[i]]
	    } else {
	        hasNA = TRUE
	        break
	    }
	}

	if (!hasNA){
		return (taxrow)
	} else {
		init = tolower(substring(ranks[i-1],1,1))
		assn = paste(init,tax,sep = '_')
		taxrow[,ranks[i:length(ranks)]] = assn
		return(taxrow)
	}
}

##### prop_tax_tab -------------------------------------------------------------
#' Propagate taxon information down an entire \code{tax_table} object
#'
#' \code{prop_tax_tab} Takes the taxon assignment from the lowest assigned
#' level in an OTU's assignment and fills it in to all the lower, unresolved
#' fields.
#'
#' @section Details:
#' Often taxon cannot be assigned all the way down to genus from the limited
#' information available from a given variable region of 16S (or other marker
#' gene). If an OTU has been assigned to Order Enterobacteriales, but the Family
#' and genus cannot be resolved, then the Family and Genus fields are left
#' blank by QIIME. This function fills those fields with the string
#' 'o_Enterobacteriales', so they are not parsed as NAs by ggplot and other
#' functions.
#'
#' @param taxtab a \code{tax_table} object
#'
#' @keywords internal
prop_tax_tab = function(taxtab){
    # taxtab: a taxonomy table to propogate taxa down in

    ## I don't know why I can't use apply for this, but I can't.
    for (r in 1:nrow(taxtab)){
        taxtab[r,] = prop_tax_row(taxtab[r,])
    }

    return(taxtab)
}

#' Propagate taxon information down an entire \code{tax_table} in a phylose
#' object
#'
#' \code{prop_tax_down} Takes the taxon assignment from the lowest assigned
#' level in an OTU's assignment and fills it in to all the lower, unresolved
#' fields.
#'
#' @section Details: Often taxon cannot be assigned all the way down to genus
#'   from the limited information available from a given variable region of 16S
#'   (or other marker gene). If an OTU has been assigned to Order
#'   Enterobacteriales, but the Family and genus cannot be resolved, then the
#'   Family and Genus fields are left blank by QIIME. This function fills those
#'   fields with the string 'o_Enterobacteriales', so they are not parsed as NAs
#'   by ggplot and other functions.
#'
#' @param physeq a phyloseq object with a filled \code{tax_table} slot.
prop_tax_down = function(physeq){
    tax_tab(physeq) = prop_tax_tab(tax_tab(physeq))
    return(physeq)
}

#### Create the data frame for plotting ----------------------------------------

##### make_phy_df --------------------------------------------------------------
#' Generate a Data Frame for Taxon Bar Charts
#'
#' \code{make_phy_df} generates a data frame that is useful for generating taxon
#' bar charts.
#'
#' @section Description: This function takes a phyloseq object and generates a
#'   data frame that is useful for plotting taxon abundance information. By
#'   default it propagates taxon assignment information down the tree into
#'   unassigned leves, and aggregates all taxa below 0.1% into a single 'Other'
#'   category. It is most sensible to use this function with a relative
#'   abundance object, but if you know what you're doing and are careful about
#'   the cutoff parameter, you can use it on normalized or raw counts.
#'
#' @param physeq A phyloseq object.
#' @param rank The rank at which to glom taxa. Must be one of 'Genus', 'Family',
#'   'Order', 'Class', 'Phylum'. TODO: make it possible to do this OTU-wise.
#' @param cutoff The abundance cutoff below which taxa are grouped into 'Other'.
#'   If you don't want anything grouped into 'Other', set this to 0.
taxa_other_df = function(physeq, rank = 'Genus', cutoff = 0.001){

	# Propogate taxonomic assignments down the tree
    physeq = prop_tax_down(physeq)

	# Glom to the correct taxonomic rank
	phyl_glommed = tax_glom(physeq, taxrank = rank)

	# Set all counts < the cutoff to zero
	otu_table(phyl_glommed)[otu_table(phyl_glommed) < cutoff] = 0

	# Filter out all taxa that are zero (<cutoff) everywhere, melt, and sort
	phyl_glommed %>%
		filter_taxa(function(x) sum(x) > 0, prune = TRUE) %>%
		psmelt() %>%
		data.frame() -> abunds

	# Order the rank by mean abundance

	abunds %>%
		group_by(UQ(sym(rank))) %>%
		summarize(Mean = mean(Abundance)) %>%
		data.frame() -> mean_abunds

	lev_ord = levels(mean_abunds[,rank])
	lev_ord = lev_ord[order(mean_abunds$Mean)]

	abunds[,rank] = factor(abunds[,rank], levels = lev_ord)

	# List all the metadata columns so that they are included in the data frame
	metacols = names(abunds)[4:(match('Rank1',names(abunds))-1)]
	# Make an 'Other' row for each sample
	abunds %>%
		group_by_(.dots = metacols) %>%
		summarize(Abundance=remain(Abundance)) -> others

	# Add in the taxonomic data columns
	taxcols = names(abunds)[match('Rank1',names(abunds)):ncol(abunds)]
	others[taxcols] = 'Other'

	# Combine the 'Other' data frame with the original
	newdf = abunds[,metacols]
	newdf$Abundance = abunds$Abundance
	newdf[,taxcols] = abunds[,taxcols]
	newdf = rbind(as.data.frame(others),
				as.data.frame(newdf))
	newdf[,rank] = factor(newdf[,rank], levels = c('Other',lev_ord))

	return(newdf)

}


### Generate Plots -------------------------------------------------------------

#' Create a Taxa Bar Chart
#'
#' \code{plot_tax_bar} creates a taxa bar chart from the data frame generated by
#' \code{make_phy_df()}.
#'
#' @param tax_df The data frame used for plotting. Unless you really know what
#'   you're doing, use the data frame output by \code{make_phy_df()}.
#' @param rank The taxonomic rank at which to view the data. Must be one of
#'   'Genus', 'Family', 'Order', 'Class', 'Phylum'. May not be a lower rank than
#'   the rank given to \code{make_phy_df()}.
#' @param colours A character vector of colour names or hex values. Must have
#'   enough colours to accommodate all your taxa at the appropriate rank. If you
#'   don't provide one, there is an internal one that has 21 colours that the
#'   function will try to use.
#' @param sample The name of the sample column in the data frame.
#' @param abund The name of the abundance column in the data frame.
#' @param yscale Can be either 'lin' or 'sqrt'. The 'sqrt' plot can look weird.
plot_tax_bar = function(taxa_df,rank,colours = NULL,
					 sample = 'X.SampleID', abund = 'Abundance',
					 yscale = 'lin'){
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
		colours = c('grey69',"#4f8579","#783fcc","#69d24d","#cb4bbd","#c6dc46",
					"#542871","#78d792","#cc4472","#83d7d0","#d44d33","#676fcd",
					"#ceb854","#403d57","#b97839","#84a4cb","#588038","#c68ac4",
					"#48472a","#c9c39c","#6e2b34","#c78889")
	} else {
		colours = c('grey69',colours)
	}

	# Make sure the x axis is categorical
	taxa_df[,sample] = factor(taxa_df[,sample])

	indiv = ggplot(taxa_df, aes_string(x = sample, y = abund, fill = rank)) +
	geom_bar(stat = "identity") +
	theme(axis.title.x = element_blank(),
			axis.text.x = element_text(size = 10,
									   angle = 90,
									   hjust = 1,
									   vjust = 0.5)) +
	scale_fill_manual(values = colours, guide = guide_legend(reverse = TRUE)) +
	ylab(paste("Relative Abundance (",rank,")\n",sep=''))

	if (yscale == 'sqrt') {
		indiv = indiv + scale_y_sqrt()
	}

	return(indiv)
}
