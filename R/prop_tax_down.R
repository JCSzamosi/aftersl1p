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
	ranks = colnames(tt)
	rank_n = which(ranks == rank)
	tt_work = tt[,1:rank_n]
    tt_glom = unique(tt_work)

	tax_vect = tt_glom[,rank]

	tt = (tt
	      %>% tibble::rownames_to_column())
	if (any(duplicated(tax_vect))){
	    dup_nms = unique(tax_vect[duplicated(tax_vect)])

	    fixed = (tt_glom
	        %>% dplyr::mutate(AmbigGenus = Genus,
	                   NAmbigGenus = if_else(Genus %in% dup_nms,
	                                         paste(Genus, ' (', Family,')',
	                                               sep = ''),
	                                         Genus))
	        %>% dplyr::right_join(tt)
	        %>% dplyr::mutate(Genus = NAmbigGenus)
	        %>% dplyr::select(-NAmbigGenus)
	        %>% dplyr::select(-AmbigGenus, AmbigGenus)
	        %>% tibble::column_to_rownames()
	        %>% dplyr::mutate_if(is.factor, as.character)
	        %>% as.matrix())

	    phyloseq::tax_table(physeq) = fixed

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

	ranks = names(taxrow)

	hasNA = FALSE
	tax = NA
	for (i in 1:length(ranks)){

	    if (!is.na(taxrow[ranks[i]])){
	        tax = taxrow[ranks[i]]
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
		taxrow[ranks[i:length(ranks)]] = assn
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
    tt = apply(taxtab, 1, prop_tax_row, indic)
    tt = t(tt)
    # for (r in 1:nrow(taxtab)){
    #     taxtab[r,] = prop_tax_row(taxtab[r,], indic)
    # }

    return(tt)
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
    tt = phyloseq::tax_table(physeq)
    sp = rownames(tt)
    tt = (ifelse((endsWith(c(tt), '__') | c(tt) == ''),
          	NA,
            c(tt))
        %>% matrix(ncol = ncol(tt)))
    colnames(tt) = colnames(tax_table(physeq))
    rownames(tt) = sp

    tt = prop_tax_tab(tt, indic)
    phyloseq::tax_table(physeq) = tt

    if(dbig){
        physeq = dbig_genera(physeq)
    }

    return(physeq)
}
