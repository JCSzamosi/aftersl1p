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


