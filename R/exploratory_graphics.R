# Imports ----------------------------------------------------------------------

#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import phyloseq
#' @import rlang
#' @import pipeR
NULL

# Objects ----------------------------------------------------------------------

cols_21 = rev(c("#4f8579","#783fcc","#69d24d","#cb4bbd","#c6dc46","#542871",
            "#78d792","#cc4472","#83d7d0","#d44d33","#676fcd","#ceb854",
            "#403d57","#b97839","#84a4cb","#588038","#c68ac4","#48472a",
            "#c9c39c","#6e2b34","#c78889"))
cols_31 = rev(c("#69006f","#62e622","#cf00d0","#00a60c","#8568ff","#01e479",
            "#ff4562","#00dda5","#bc0024","#7cffff","#630039","#daff8b",
            "#00285d","#dbb600","#ff7fc4","#8dae00","#a7d0ff","#9e5000",
            "#bcf2ff","#662000","#f1ffd0","#241500","#ffdfe7","#376b00",
            "#ff8d6d","#00322c","#ff9d44","#005e75","#ffce96","#3c4400",
            "#695000"))
acols_60 = c("#f44400","#6151fe","#66f035","#3015be","#c2ff29","#7d00be",
			"#fbf700","#01189d","#86ff6c","#8d00a6","#3db500","#ff3fd6",
			"#01c145","#b277ff","#809900","#0058ca","#f5ff80","#23005d",
			"#00b765","#f70078","#00be90","#de0081","#00d6d5","#ff343d",
			"#00bbfd","#ba0004","#01badf","#ff496c","#00562a","#ff7fde",
			"#456800","#a391ff","#a67c00","#002368","#ffa74c","#016cb1",
			"#aa4f00","#0091c7","#ffe18e","#970077","#d1ffcb","#5e0044",
			"#bae4e5","#350001","#e6b3ff","#2c4900","#ff96ae","#001d18",
			"#ffccd7","#280020","#ffb3a4","#003559","#786200","#008292",
			"#790036","#00775f","#540d00","#373738","#5b3300","#352f00")
cols_60 = rev(c("#005981","#71e000","#5b4cf8","#d0e400","#6600b2","#01c54e",
               "#da1ada","#dbff76","#003daa","#a6a700","#948dff","#e29700",
               "#003787","#96ff9c","#b4007c","#01cb7d","#f2006a","#01eeb9",
               "#ee3e00","#1dfff2","#b70028","#01b284","#ff52b5","#007b20",
               "#ff86e2","#002e02","#e1a4ff","#525900","#920072","#b0ffc9",
               "#39004c","#ffe689","#001a52","#ffb869","#006abc","#a96800",
               "#64b6ff","#9d2f00","#81efff","#ff545c","#01a8aa","#8b003e",
               "#e7ffd0","#440032","#fefffe","#030017","#ff925b","#0092bd",
               "#ff8463","#028899","#600017","#c8e3ff","#702f00","#f0d4ff",
               "#1f1100","#ffc18a","#003138","#ffbcbd","#01704c","#432b00"))
cols_70 = rev(c("#00cf9b","#fc00bf","#52e62c","#a000b2","#9fff37","#5066ff",
			   "#c2de00","#003ebb","#69c500","#cf69ff","#8dff6e","#1f0063",
			   "#ffed43","#9572ff","#01a30f","#ff64df","#01ce53","#76007c",
			   "#7cff8e","#0057cc","#ffc93a","#4191ff","#de0200","#1af4ff",
			   "#cc003a","#01d57d","#ca0057","#00e5d4","#a3001c","#8dffc7",
			   "#ac0053","#a5ffa9","#003a8a","#d0ff8d","#001f50","#94a600",
			   "#cbaeff","#4e9100","#ffb1f8","#3f8100","#ff9acd","#006b32",
			   "#ff6c62","#00aec9","#dc5f00","#004a72","#c08c00","#260021",
			   "#ffe08c","#001524","#deffd6","#2c0100","#c8f8ff","#660014",
			   "#9fd4ff","#701a00","#dcd0ff","#003c11","#ffd9f4","#181500",
			   "#ffd8a1","#00332c","#ff8763","#018b69","#9d5d00","#006a7f",
			   "#ffae8e","#472700","#ffb4b6","#807100"))

# Functions --------------------------------------------------------------------

## Functions to generate taxa bar charts ---------------------------------------

### Deal with data -------------------------------------------------------------

#### Deal with taxa ------------------------------------------------------------

##### dbig_taxa --------------------------------------------------------------

#' Disambiguate taxa
#'
#' Check for any taxa that have the same name but are in different higher-level
#' classifications, and append the family name
#'
#' @section Details: \code{dbig_genera} takes a phyloseq object with a
#'   taxonomy table, disamibugates the genera by appending family names to any
#'   genus names that are found in multiple families in this data set, and
#'   returns the object with an update tax_table() object. The input phyloseq
#'   object must have 'Genus' and 'Family' columns in its tax_table object. The
#'   output object with have the disambiguated names in the 'Genus' column and
#'   will have a new column called 'AmbigGenus' where the old genus names can be
#'   found.
#'
#' @section Value: A phyloseq object with an updated Genus column and a column
#'   called AmbigGenus containing the old Genus names.
#'
#' @param physeq A phyloseq object with a tax table. Tax table must have 'Genus'
#'   and 'Family' columns.
dbig_genera = function(physeq){
	rank = 'Genus'
	rank_abv = 'Family'
	ambig = paste('Ambig',rank,sep = '')
	nambig = paste('NAmbig',rank,sep = '')

	tt = data.frame(tax_table(physeq), stringsAsFactors = FALSE)
    tt_glom = unique(tt)

	tax_vect = tt_glom[,rank]

	if (any(duplicated(tax_vect))){
	    dup_nms = unique(tax_vect[duplicated(tax_vect)])

	    tt_glom %>%
	        mutate(AmbigGenus = Genus,
	               NAmbigGenus = if_else(Genus %in% dup_nms,
	                                     paste(Genus, ' (', Family,')',
	                                           sep = ''),
	                                     Genus)) %>%
	        right_join(tt) %>%
	        mutate(Genus = NAmbigGenus) %>%
	        select(-NAmbigGenus) %>%
	        mutate_if(is.factor, as.character) %>%
	        as.matrix() -> fixed

	    rownames(fixed) = rownames(tt)

	    tax_table(physeq) = fixed

	}

	return(physeq)
}

##### prop_tax_row -------------------------------------------------------------

#' Propagate taxon information in a single row of a \code{tax_table} object.
#'
#' \code{prop_tax_row} Takes the taxon assignment from the lowest assigned level
#' in an OTU's assignment and fills it in to all the lower, unresolved fields.
#'
#' @section Details: Often taxon cannot be assigned all the way down to genus
#'   from the limited information available from a given variable region of 16S
#'   (or other marker gene). If an OTU has been assigned to Order
#'   Enterobacteriales, but the Family and genus cannot be resolved, then the
#'   Family and Genus fields are left blank by QIIME. This function fills those
#'   fields with the string 'o_Enterobacteriales', so they are not parsed as NAs
#'   by ggplot and other functions.
#'
#' @section Value: A single row \code{tax_table} object to with NAs replaced
#'   with higher-level taxon assignments
#'
#' @param taxrow A single row in a taxon table
#' @param indic a flag to indicate if the taxon names have level indicators.
#'   If FALSE, they are added.
#'
#' @keywords internal
prop_tax_row = function(taxrow,indic){
	## taxrow: a row from a tax_table of a phyloseq object

	ranks = colnames(taxrow)

	hasNA = FALSE
	tax = NA
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
	    if (is.na(tax)){
	        assn = 'Unassigned'
	    } else if (indic){
		    assn = tax
		} else {
		    init = tolower(substring(ranks[i-1],1,1))
		    assn = paste(init,tax,sep = '_')
		}
		taxrow[,ranks[i:length(ranks)]] = assn
		return(taxrow)
	}
}

##### prop_tax_tab -------------------------------------------------------------

#' Propagate taxon information down an entire \code{tax_table} object
#'
#' \code{prop_tax_tab} Takes the taxon assignment from the lowest assigned level
#' in an OTU's assignment and fills it in to all the lower, unresolved fields.
#'
#' @section Details: Often taxon cannot be assigned all the way down to genus
#'   from the limited information available from a given variable region of 16S
#'   (or other marker gene). If an OTU has been assigned to Order
#'   Enterobacteriales, but the Family and genus cannot be resolved, then the
#'   Family and Genus fields are left blank by QIIME. This function fills those
#'   fields with the string 'o_Enterobacteriales', so they are not parsed as NAs
#'   by ggplot and other functions.
#'
#' @section Value: A \code{tax_table} object with NAs replaced with higher-level
#'   taxon assignment
#'
#' @param taxtab a \code{tax_table} object
#' @param indic a flag to indicate if the taxon names have level indicators. If
#'   FALSE, they are added. Just gets passed to \code{\link{prop_tax_row}}.
#'
#' @keywords internal
prop_tax_tab = function(taxtab, indic){

    ## I don't know why I can't use apply for this, but I can't.
    for (r in 1:nrow(taxtab)){
        taxtab[r,] = prop_tax_row(taxtab[r,], indic)
    }

    return(taxtab)
}

##### prop_tax_down ------------------------------------------------------------

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
#' @section Value: A phyloseq object whose \code{tax_table} has had NAs replaced
#'   with the higher level taxon assignments.
#'
#' @param physeq a phyloseq object with a filled \code{tax_table} slot.
#' @param indic a flag to indicate if the taxon names have level indicators. If
#'   FALSE, they are added. Just gets passed to \code{\link{prop_tax_tab}}.
#' @param dbig a flag to indicate whether genus names that exist in multiple
#'   families should be disambiguated by appending the family name.
#' @export
prop_tax_down = function(physeq, indic, dbig = TRUE){

	# Deal with the case where the blanks aren't NAs
    tt = tax_table(physeq)
    sp = rownames(tt)
    ifelse((endsWith(c(tt), '__') | c(tt) == ''),
          	NA,
            c(tt)) %>%
        matrix(ncol = ncol(tt)) -> tt
    colnames(tt) = colnames(tax_table(physeq))
    rownames(tt) = sp
    tax_table(physeq) = tt

    tax_table(physeq) = prop_tax_tab(tax_table(physeq), indic)

    if(dbig){
        physeq = dbig_genera(physeq)
    }

    return(physeq)
}


#### Create the data frame for plotting ----------------------------------------

##### remain -------------------------------------------------------------------

#' Calculate the remainder for the 'Other' section of a phyloseq data frame.
#'
#' @section Value: a numeric vector
#'
#' @param x A numeric vector
#' @param tot The desired total. Default is 1.
remain = function(x, tot = 1){
    tot-sum(x)
}


##### order_taxa ---------------------------------------------------------------

#' Order Taxon Name Factors
#'
#' \code{order_taxa} reorders the taxon names in a taxon column (e.g. 'Class' or
#' 'Phylum') by the taxon's mean abundance (but always makes sure to put Other
#' first).
#'
#' @section Value: A data frame that is identical to the one given, but with the
#'   specified column re-ordered by its mean abundance
#'
#' @param phy_df A data frame of a phyloseq object, as produced by
#'   \code{\link{psmelt}} or \code{\link{make_phy_df}}.
#' @param rank The name of the column to be re-ordered
#' @param abund The name of the abundances column. Defaults to 'Abundance'
#' @param decreasing Specifies whether the taxon order should be based on
#'   decreasing or increasing abundance. Defaults to FALSE.
order_taxa = function(phy_df, rank, abund = 'Abundance', decreasing = FALSE){

    phy_df[,rank] = factor(phy_df[,rank])
	phy_df %>%
        filter(UQ(sym(rank)) != 'Other') %>%
		group_by(UQ(sym(rank))) %>%
		summarize(Tot = sum(UQ(sym(abund)))) %>%
		data.frame() -> total_abunds

	lev_ord = levels(droplevels(total_abunds[,rank]))
	if (decreasing){
	    lev_ord = lev_ord[order(-total_abunds$Tot)]
	} else{
	    lev_ord = lev_ord[order(total_abunds$Tot)]
	}

	phy_df[,rank] = factor(phy_df[,rank], levels = c('Other',lev_ord))

    return(phy_df)
}

##### make_phy_df --------------------------------------------------------------

#' Generate a Data Frame for Taxon Bar Charts
#'
#' \code{make_phy_df} generates a data frame that is useful for generating taxon
#' bar charts.
#'
#' @section Details: This function takes a phyloseq object and generates a data
#'   frame that is useful for plotting taxon abundance information. By default
#'   it propagates taxon assignment information down the tree into unassigned
#'   leves, and aggregates all taxa below 0.1 percent into a single 'Other'
#'   category. This function expects the phyloseq object to be relative
#'   abundance, and weird things will happen if it is not.
#'
#' @section Value: A data frame similar in structure to that generated by
#'   \code{psmelt}, but with an 'Other' category added and taxon levels ordered
#'   for use in plotting.
#'
#' @param physeq A phyloseq object.
#' @param rank The rank at which to glom taxa. Must be one of 'Genus', 'Family',
#'   'Order', 'Class', 'Phylum'. Default is 'Genus'. TODO: make it possible to
#'   do this OTU-wise.
#' @param cutoff The abundance cutoff below which taxa are grouped into 'Other'.
#'   If you don't want anything grouped into 'Other', set this to 0. Default is
#'   0.001.
#' @param indic a flag to indicate if the taxon names have level indicators.
#'   If FALSE, they are added.
#' @param prop Specifies whether taxa need to be propogated down the taxonomy
#'   table (default, TRUE) or if this has already been done.
#' @param count If FALSE (default) the function will expect a relative abundance
#'   table and create an 'Other' category for taxa below the cutoff (and will
#'   raise an error if the table is not relative abundance). If TRUE, the
#'   function will not check for relative abundance and will not create an
#'   'Other' category.
#' @export
make_phy_df = function(physeq, rank = 'Genus', cutoff = 0.001, indic = FALSE,
					   prop = TRUE, count = FALSE){


    # Check that its relab
    if (!count & any(otu_table(physeq) > 1)){
        stop('physeq must be a relative abundance table. You have counts > 1.')
    }
    ranks = colnames(tax_table(physeq))

    # Propogate taxonomic assignments down the tree
    if (prop){
    	physeq = prop_tax_down(physeq, indic)
    }

	# Glom to the correct taxonomic rank
	phyl_glommed = tax_glom(physeq, taxrank = rank)

	# Set all counts < the cutoff to zero
	otu_table(phyl_glommed)[otu_table(phyl_glommed) < cutoff] = 0

	# Filter out all taxa that are zero (<cutoff) everywhere, melt, and sort
	phyl_glommed %>%
		filter_taxa(function(x) sum(x) > 0, prune = TRUE) -> abunds_phy

	abunds_phy %>%
	    psmelt() %>%
		data.frame() -> abunds

	# List all the metadata columns so that they are included in the data frame
	metacols = names(abunds)[4:(match(ranks[1],names(abunds))-1)]
	# Make an 'Other' row for each sample
	abunds %>%
		group_by_(.dots = metacols) %>%
		summarize(Abundance=remain(Abundance)) -> others

	# Add in the taxonomic data columns
	taxcols = names(abunds)[match(ranks[1],names(abunds)):ncol(abunds)]

	if (! count){
	    others[taxcols] = 'Other'

	    # Combine the 'Other' data frame with the original
	    newdf = abunds[,metacols]
	    newdf$Abundance = abunds$Abundance
	    newdf[,taxcols] = abunds[,taxcols]
	    newdf = rbind(as.data.frame(others),
	                  as.data.frame(newdf))
	} else {
	    newdf = abunds
	}

	# Order the tax cols by mean abundance
	for (r in taxcols){
	    newdf = order_taxa(newdf, r)
	}

	return(newdf)

}




##### df_glom ------------------------------------------------------------------

#' Like tax_glom, but for data frames
#'
#' \code{df_glom} take totals within sample at a given taxonomic rank.
#'
#' @param phy_df A phyloseq data frame, as generated by \code{\link{psmelt}} or
#'   \code{\link{make_phy_df}}
#' @param ranks A character vector with the taxon rank names
#' @param IDcol The column name of the sample IDs
#' @param rank The taxonomic rank to glom at
#' @param abunds The column name of the abundances to sum
#' @param tots The desired column name of the summed (glommed) abundances
df_glom = function(phy_df, ranks, IDcol = 'X.SampleID', rank = 'Phylum',
                   abunds = 'Abundance'){

    # Set up the groups for the plotting totals
    phy_df %>%
        group_by_at(vars(IDcol,rank)) %>%
        mutate(TotalAbunds = sum(UQ(sym(abunds)))) %>%
        data.frame() -> glommed_df

    return(glommed_df)
}

##### subset_order -------------------------------------------------------------

#' Subset and generate taxon ordering
#'
#' \code{subset_order} generates a data frame whose taxon column given by
#' \code{rank} has been ranked according to its mean abundance in the
#' \code{abunds} column.
#'
#' @param phy_df A phyloseq data frame, as generated by \code{\link{psmelt}}, but
#'   probably generated by \code{\link{df_glom}}.
#' @param varbs (\code{NULL}) A character vector of grouping variables from
#'   which the baseline values are chosen to define the abundance ordering. If
#'   it is \code{NULL}, the ordering will be based on mean abundances in the
#'   whole data frame.
#' @param bases (\code{NULL}) A character vector of baseline values for the
#'   variables given in \code{vars}. The ordering of the taxa will be given
#'   based only on the samples with these baseline values for these variables.
#'   Must be in the same order as varbs.
#' @param abund (\code{'Abundance'}) The name of the abundance column.
#' @param rank (\code{'Genus'}) The rank to base the ordering on.
subset_order = function(phy_df, varbs = NULL, bases = NULL, rank = 'Genus',
                        abunds = 'TotalAbunds'){
    # Check inputs
    if (is.null(varbs)){
        warn('No grouping variables given. Using whole data set.')
    } else if (is.null(bases)){
        warn(paste('No baseline values given for grouping variables.',
                    'Using whole data set.'))
    } else if (length(varbs) != length(bases)){
        stop('vars and bases must have the same length.')
    }


    # Subset and order
    ranked = phy_df
    for(i in 1:length(varbs)){
        ranked %>% filter(UQ(sym(varbs[i])) == bases[i]) -> ranked
    }
    ranked %>%
        group_by_at(vars(c(varbs,rank))) %>%
        summarize(MetaMean = mean(UQ(sym(abunds)))) %>%
        data.frame() %>%
        order_taxa(rank, 'MetaMean', decreasing = TRUE) -> ranked

    return(ranked)
}

##### rank_abund ---------------------------------------------------------------

#' Rank Taxa by Abundance
#'
#' \code{rank_abund()} generates a data frame that's ready to be used by
#' \code{\link{plot_rank_abund}}
#'
#' @section Value: A data frame whose taxa have been ranked by their mean
#'   abundance in the baseline level of some grouping variable or variables.
#'
#' @param phy_df A dataframe of a phyloseq object, like that generated by
#'   \code{\link{psmelt}} or \code{\link{make_phy_df}}
#' @param varbs (\code{NULL}) A character vector of grouping variables from
#'   which the baseline values are chosen to define the abundance ordering. If
#'   it is \code{NULL}, the ordering will be based on mean abundances in the
#'   whole data frame.
#' @param bases (\code{NULL}) A character vector of baseline values for the
#'   variables given in \code{vars}. The ordering of the taxa will be given
#'   based only on the samples with these baseline values for these variables.
#'   Must be in the same order as vars.
#' @param abunds (\code{'Abundance'}) The name of the abundance column.
#' @param rank (\code{'Genus'}) The rank to base the ordering on.
#' @param IDcol (\code{'X.SampleID'}) The column name of the sample IDs
rank_abund = function(phy_df, varbs = NULL, bases = NULL, abunds = 'Abundance',
                      rank = 'Genus', IDcol = 'X.SampleID'){
    # Set up the groups for the plotting totals
    rank_abs = df_glom(phy_df, IDcol = IDcol, rank = rank, abunds = abunds)

    # Subset and order
    ranked = subset_order(rank_abs, varbs, bases, rank = rank)

    # Order the bigger data frame by the above ordering
    lev_ord = levels(ranked[,rank])
    rank_abs[,rank] = factor(rank_abs[,rank], levels = lev_ord)

    return(rank_abs)
}


#### order_levs

#' Order the levels of one factor by the values of another
#'
#' \code{order_levs()} takes two factors, the first of which has values that are
#' nested within the values of the second, and orders the levels of the first
#' factor such that they are clustered within the second factor.
#'
#' @section Value: A factor that has the same levels as f1, but that has been
#'   re-ordered.
#'
#' @param f1 The factor to re-order. Levels of this factor must be nested within
#'   \code{f2}
#' @param f2 The factor to use when re-ordering \code{f1}.
order_levs = function(f1,...){

    # if (is.numeric(f2)){
    #     ord = order(...)
    # } else {
        ord = order(...) # don't revert this to as-character. It needs to respect
                        # f2's level ordering. Find another way.
    # }

    lev_ord = unique(as.character(f1)[ord])
    f1 = factor(f1, levels = lev_ord)

    return(f1)
}

### Generate Plots -------------------------------------------------------------

#### rotate_ticks --------------------------------------------------------------

#' Rotate the x tick labels 90 degrees and position them correctly.
#'
rotate_ticks = function(){
    theme(axis.text.x = element_text(size = 10,
                                     angle = 90,
                                     hjust = 1,
                                     vjust = 0.5))
}

#### plot_tax_bar --------------------------------------------------------------

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


## Functions to generate distance bar charts -----------------------------------

### lddf_check -----------------------------------------------------------------

#' Check the inputs of \code{long_distance_df()}
#'
#' For internal use only
lddf_check = function(dmat, metadat, idcol = 'X.SampleID', diag = FALSE,
                            suff = c('1','2')){

    # Check inputs
    if (!is.logical(diag)){
        stop('diag must be either TRUE or FALSE')
    }

    if ((length(suff) != 2) | (suff[1] == suff[2]) | (!is.character(suff))){
        stop('suff must be a vector of two non-identical strings')
    }

    if (!(idcol %in% colnames(metadat))){
        stop('idcol must be one of the column names of metadat')
    }

    # Check sample names

    ## If it's a proper distance matrix, it will have a "Labels" attribute
    if (!is.null(attr(dmat,'Labels'))){
        d_samps = attr(dmat, 'Labels')
    } else if (!is.null(colnames(dmat))){
        if (any(colnames(dmat) != rownames(dmat))){
            stop(paste('The row and column names of dmat must be the same,',
                        'and in the same order.'))
        } else {
            d_samps = colnames(dmat)
        }
    }

    if (!all(d_samps %in% as.character(metadat[,idcol]))){
        stop('Some samples missing from metadata')
    } else if (!all(as.character(metadat[,idcol]) %in% d_samps)){
        warning(paste('Metadata samples that are not in the distance matrix',
                        'will be excluded.'))
    }
}


### lddf_work ------------------------------------------------------------------

#' Does the actual gathering and spreading without testing assumptions
#'
#' \code{lddf_work} Does the actual gathering, spreading, and joining associated
#' with making the lddf, but without checking if the distance matrix is sensible
#' or removing diagonals and repeats. This is for when you know what you're
#' doing and have trimmed your distance matrix down to only what you know you
#' need. Good for permutation tests.
#'
#' @param dmat A distance matrix or other diagonal matrix object with sample
#'   names as row and column names.
#' @param metadat A data frame or data frame-like object with the data set's
#'   metadata
#' @param idcol (\code{'X.SampleID'}.) A string. The column in \code{metadat}
#'   that holds the sample names. Sample names should match the row/column namse
#'   of the distance matrix. If there are samples in the metadata data frame
#'   that are missing from the distance matrix, they will be excluded with a
#'   warning. If there are samples in the distance matrix that are missing from
#'   the metadata, you will get an error.
#' @param suff (\code{c('1','2')}.) A character vector of length 2. The suffixes
#'   to be appended to the metadata column names in the output. The two elements
#'   must not be identical.
#' @param distcol (\code{'Distance'}.) A string. The desired column name for the
#'   distance column in your long data frame. Only here to avoid clashes with
#'   existing metadata column names.
lddf_work = function(dmat, metadat, idcol = 'X.SampleID', suff = c('1','2'),
                     distcol = 'Distance'){
    # Make it long
    dmat %>>%
        {data.frame(ID1 = rownames(.),.)} %>%
        gather(ID2, Distance, 2:(ncol(dmat)+1), na.rm = TRUE) -> distlong

    ids = paste(idcol,suff, sep = '')
    names(distlong)[1:2] = ids

    # Add the metadata columns for the first sample
    distlong %>%
        inner_join(metadat, by = setNames(c(idcol), ids[1])) -> distlong

    # Add the suffix to the column names
    cn = colnames(distlong)
    cn = ifelse(cn %in% colnames(metadat),
                paste(cn, suff[1], sep = ''),
                cn)
    colnames(distlong) = cn

    # Add the metadata columns to the second sample
    distlong %>%
        inner_join(metadat, by = setNames(c(idcol), ids[2])) -> distlong

    # Add the suffix to the column names
    cn = colnames(distlong)
    cn = ifelse(cn %in% colnames(metadat),
                paste(cn, suff[2], sep = ''),
                cn)
    colnames(distlong) = cn

    return(distlong)
}

### long_distance_df -----------------------------------------------------------

#' Create a long data frame of among-sample distances
#'
#'
#' \code{long_distance_df} creates a long data frame of all the pairwise
#' distances from a sample distance matrix (e.g. the output of
#' \code{\link{phyloseq::distance}}) with all the metadata listed for each sample.
#' Allows for easy within- and among-group boxplots, or whatever other
#' comparisons are of interest.
#'
#' @section Value: A data frame \eqn{N(N-1)} (or \eqn{N^2} if \code{diag = TRUE}
#'   is set) rows (where N is the number of samples) with sample IDs, metadata,
#'   and pairwise distances listed for each pair of samples. Sample ID and
#'   metadata columns have '1' or '2' appended to them so the user can tell
#'   which column belongs to which sample.
#' @param dmat A distance matrix or other diagonal matrix object with sample
#'   names as row and column names.
#' @param metadat A data frame or data frame-like object with the data set's
#'   metadata
#' @param idcol (\code{'X.SampleID'}.) A string. The column in \code{metadat}
#'   that holds the sample names. Sample names should match the row/column namse
#'   of the distance matrix. If there are samples in the metadata data frame
#'   that are missing from the distance matrix, they will be excluded with a
#'   warning. If there are samples in the distance matrix that are missing from
#'   the metadata, you will get an error.
#' @param diag (\code{FALSE}.) Logical. Whether the diagonal elements (zeros in
#'   a distance matrix) should be included in the long data frame. Defaults to
#'   \code{FALSE} because we almost never want them.
#' @param suff (\code{c('1','2')}.) A character vector of length 2. The suffixes
#'   to be appended to the metadata column names in the output. The two elements
#'   must not be identical.
#' @param distcol (\code{'Distance'}.) A string. The desired column name for the
#'   distance column in your long data frame. Only here to avoid clashes with
#'   existing metadata column names.
#' @param baseline (\code{'NULL'}). A dataframe whose column names must also be
#'   column names in the metadat data frame, and whose rows contain a subset of
#'   the possible values/combinations. If this parameter is used, all the
#'   samples whose metadata matches a row in this data frame will end up in
#'   Sample1 and the rest will end up in Sample2. This means you will _not_ get
#'   all the pairs, because the samples in Sample1 will not get compared to each
#'   other, and neither will the samples in Sample2. If this parameter is not
#'   used, the upper triangle of the distance matrix is used, without regard for
#'   metadata values.
#' @export
long_distance_df = function(dmat, metadat, idcol = 'X.SampleID', diag = FALSE,
                            suff = c('1','2'), distcol = 'Distance',
                            baseline = NULL){

    # The sample data object does not play nice with others
    metadat = data.frame(metadat)

    # Check the inputs
    lddf_check(dmat, metadat, idcol, diag, suff)

    # Turn this into a usable matrix
    dmat = as.matrix(dmat)
    if (is.null(baseline)){
        if (diag) {
            dmat[upper.tri(dmat)]= NA
        } else {
            dmat[!lower.tri(dmat)] = NA
        }
    } else {
        s1 = inner_join(metadat, baseline, by = colnames(baseline))[,idcol]
        s2 = anti_join(metadat, baseline, by = colnames(baseline))[,idcol]
        dmat = dmat[as.character(s1),as.character(s2)]
    }

    lddf = lddf_work(dmat, metadat, idcol = idcol, suff = suff,
                     distcol = distcol)

    return(lddf)
}



## Functions to generate PCoA plots --------------------------------------------

### Make the data frame --------------------------------------------------------
#### axis_num ------------------------------------------------------------------

#' Get the axis number
#' Take a character vector of the form 'Axis.N' where N is a number and return
#' a numeric vector of N in the same order
#' @param AxisX The character vector of axes. Must only contain values of the
#' form 'Axis.N'
axis_num = function(AxisX){
    AxisX %>%
        strsplit('\\.') %>%
        unlist() -> tmp
    axes = as.numeric(tmp[seq(2,length(tmp),2)])

    return(axes)
}

#### make_rda_df----------------------------------------------------------------

#' Make a data frame of the ordination if the ordination method is RDA. Not
#' exported.
#'
#' Take a phyloseq object and generate a data frame of the distance-based
#' ordination of the samples for plotting when the ordination method was
#' RDA. Used internally by \code{make_ord_df()}
#'
#' @param ord The ordination object generated by \code{phyloseq::ordinate()}.
#' @param physeq The phyloseq object we're making things from
#' @param axes A numeric vector of the axes to plot
#' @export
make_rda_df = function(ord, physeq, axes){
    # Calculate the axis weights

    eigs = ord$CA$eig
    weights = round(eigs/sum(eigs), 3) * 100

    # Make the data frame
    ord_df = plot_ordination(physeq, ord, axes = axes, justDF = TRUE)
    ord_long = (ord_df
                %>% gather(AxisX, ValueX, starts_with('PC'))
                %>% left_join(ord_df)
                %>% gather(AxisY, ValueY, starts_with('PC'))
                %>% mutate(AxisX = paste(AxisX, paste(weights[AxisX], '%',
                                                      sep = '')),
                           AxisY = paste(AxisY, paste(weights[AxisY], '%',
                                                      sep = ''))))

    return(ord_long)
}

#### make_pcoa_df---------------------------------------------------------------

#' Make a data frame of the ordination if the ordination method is PCoA. Not
#' exported
#'
#' Take a phyloseq objet and generate a data frame of the distance-based
#' ordination of the samples for plotting when the ordination method was PCoA.
#' Used internally by \code{make_ord_df()}
#'
#' @param ord The ordination object generated by \code{phyloseq::ordinate()}.
#' @param physeq The phyloseq object we're making things from.
#' @param axes A numeric vector of the axes to include.
make_pcoa_df = function(ord, physeq, axes){
    # Calculate the axis weights
    weights = round(ord$values$Relative_eig, 3) * 100

    # Make the data frame
    ord_df = plot_ordination(physeq, ord, axes = axes, justDF = TRUE)
    ord_long = (ord_df
                %>% gather(AxisX, ValueX, starts_with('Axis.'))
                %>% left_join(ord_df)
                %>% gather(AxisY, ValueY, starts_with('Axis.'))
                %>% mutate(AxisX = factor(paste(AxisX,
                                        paste(weights[axis_num(AxisX)], '%',
                                                 sep = ''))),
                           AxisY = factor(paste(AxisY,
                                        paste(weights[axis_num(AxisY)], '%',
                                                 sep = '')))))
    return(ord_long)
}

#### make_ord_df --------------------------------------------------------------

#' Make a data frame of the ordination
#'
#' Take a phyloseq object and generate a data frame of the distance-based
#' ordination of the samples for plotting. This function has been tested with
#' ordination methods 'PCoA' and 'RDA'. I can't make any promises if you use a
#' different ordination method.
#'
#' @param physeq A phyloseq object with an OTU table and sample data. The table
#' should be rarefied (or relative abundance) so that the distance metrics will
#' be meaningful.
#' @param dist_meth The distance method to be use. Must be one of the methods
#' accepted by the \code{phyloseq::distance()} function. Default is 'bray'.
#' @param ord_meth The ordination method. Must be one of the methods accepted
#' by the \code{phyloseq::ordinate()} function. Default is 'PCoA'.
#' @param scree_only If \code{TRUE}, this function will print the scree plot of the
#' requested ordination and then exit. Good for deciding how many axes you
#' care about. Default is \code{FALSE}.
#' @param axes A vector of integers indicating which ordination axes to include
#' in the data frame. Defaults to \code{1:4}.
make_ord_df = function(physeq, dist_meth = 'bray', ord_meth = 'PCoA',
                  scree_only = FALSE, axes = 1:4){
    # Get the distance object and do the ordination
    d = distance(physeq, method = dist_meth)
    ord = ordinate(physeq, ord_meth, d)

    # Scree
    if (scree_only) {
        print(phyloseq::plot_scree(ord))
        return()
    }

    if (ord_meth == 'PCoA'){
        ord_long = make_pcoa_df(ord, physeq, axes)
    } else if (ord_meth == 'RDA'){
        ord_long = make_rda_df(ord, physeq, axes)
    } else {
        ord_long = tryCatch(make_pcoa_df(ord, physeq, axes))
        if (is.data.frame(ord_long)){
            warn('Data frame produced, but untested with this ordination method. Double check before plotting.')
        } else {
            ord_long = tryCatch(make_rda_df(ord, physeq, axes))
            if (is.data.frame(ord_long)) {
                warn('Data frame produced, but untested with this ordination method. Double check before plotting.')
            } else {
                stop('Data frame could not be produced. Try \'PCoA\' or \'RDA\' ordination methods.')
            }
        }

        return(ord_long)
    }

    return(ord_long)

}

### Plot the ordination --------------------------------------------------------
#### plt_ord ------------------------------------------------------------------
#' Plot the ordination
#'
#' Make a multi-axis PCoA plot from the data frame
#' @param ord_long The ordination data frame produced by \code{make_ord_df}
#' @param colour The name of the column to use to colour the points.
#' @param shape The name of the coloumn governing the shape of the points.
#' @param size The size of the points. Passed to the \code{geom_point()} size
#' argument and defaults to 1.
#' @param pt_alph The transparency of the points. Default is 0.7
plt_ord = function(ord_long, colour = NULL, shape = NULL, size = 1,
                   pt_alph = 0.7){
    ord_plt = ggplot(ord_long, aes(ValueX, ValueY))

    if (!is.null(colour) & !(is.null(shape))) {
        ord_plt = ord_plt +
            geom_point(aes_string(colour = colour, shape = shape),
                       size = size,
                       alpha = pt_alph)
    } else if (!is.null(colour)){
        ord_plt = ord_plt +
            geom_point(aes_string(colour = colour), alpha = pt_alph, size = size)
    } else if (!is.null(shape)){
        ord_plt = ord_plt +
            geom_point(aes_string(shape = shape), alpha = pt_alph, size = size)
    } else {
        ord_plt = ord_plt +
            geom_point(alpha = pt_alph, size = size)
    }

    ord_plt = ord_plt +
        facet_grid(AxisY ~ AxisX)

    return(ord_plt)
}


## Miscellaneous Functions -----------------------------------------------------

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

#### plot_rank_ab --------------------------------------------------------------

#' Create a rank-abundance plot from a phyloseq data frame
plot_rank_ab = function(){

}
#### plot_alpha ----------------------------------------------------------------

#' Plot alpha diversity
#'
#' Plot the alpha diversity of a phyloseq data set.
#'
plot_alpha = function(physeq) {
    # rarefy

}


