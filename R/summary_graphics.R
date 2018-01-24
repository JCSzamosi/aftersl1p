################################################################################
### Functions to generate summarizing graphics
################################################################################

#################### Generate appropriate data frame objects -------------------

#### Deal with taxa ------------------------------------------------------------

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


#' Propagate taxon information down in unresolved OTUs
#'
#' \code{prop_tax_down} takes a phyloseq object and returns the object with the
#' unresolved taxon information filled in with higher-level assignments.
#'
#' @param
