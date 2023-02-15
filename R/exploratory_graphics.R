# Imports ----------------------------------------------------------------------

#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import phyloseq
#' @import rlang
#' @import pipeR
#' @import tibble

NULL

# Objects ----------------------------------------------------------------------


# Functions --------------------------------------------------------------------

## Functions to generate taxa bar charts ---------------------------------------

### Deal with data -------------------------------------------------------------

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


