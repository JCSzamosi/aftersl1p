### long_distance_df -----------------------------------------------------------

#' Create a long data frame of among-sample distances
#'
#'
#' \code{long_distance_df} creates a long data frame of all the pairwise
#' distances from a sample distance matrix (e.g. the output of
#' \code{\link[phyloseq]{distance}}) with all the metadata listed for
#' each sample. Allows for easy within- and among-group boxplots, or whatever
#' other comparisons are of interest.
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
        s1 = dplyr::inner_join(metadat, baseline,
                               by = colnames(baseline))[,idcol]
        s2 = dplyr::anti_join(metadat, baseline,
                              by = colnames(baseline))[,idcol]
        dmat = matrix(dmat[as.character(s1),as.character(s2)],
                         nrow = length(s1),
                         ncol = length(s2))
        rownames(dmat) = s1
        colnames(dmat) = s2
    }

    lddf = lddf_work(dmat, metadat, idcol = idcol, suff = suff,
                     distcol = distcol)

    return(lddf)
}

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
#' \code{lddf_work} Used internally by \code{long_distance_df()}. I recommend
#' you use that function unless you really know what you're doing. This function
#' does the actual gathering, spreading, and joining associated with making the
#' lddf, but without checking if the distance matrix is sensible or removing
#' diagonals and repeats. Use this function if you know exactly what you want
#' and have trimmed your distance matrix down to only what you know you need.
#' Good for permutation tests.
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
        tidyr::gather(ID2, Distance, 2:(ncol(dmat)+1), na.rm = TRUE) -> distlong

    ids = paste(idcol,suff, sep = '')
    names(distlong)[1:2] = ids

    # Add the metadata columns for the first sample
    distlong %>%
        dplyr::inner_join(metadat, by = setNames(c(idcol), ids[1])) -> distlong

    # Add the suffix to the column names
    cn = colnames(distlong)
    cn = ifelse(cn %in% colnames(metadat),
                paste(cn, suff[1], sep = ''),
                cn)
    colnames(distlong) = cn

    # Add the metadata columns to the second sample
    distlong %>%
        dplyr::inner_join(metadat, by = setNames(c(idcol), ids[2])) -> distlong

    # Add the suffix to the column names
    cn = colnames(distlong)
    cn = ifelse(cn %in% colnames(metadat),
                paste(cn, suff[2], sep = ''),
                cn)
    colnames(distlong) = cn

    return(distlong)
}

