#' taxon colour vector
#'
#' A vector of colours for use with taxa bar charts.
#'
#' @section Details: There are 30 colours, and grey will be added by the
#'   [plot_tax_bar()] function for the "Other" category. If a plot
#'   calls for more than 30 colours, this will just recycle. That is usually
#'   fine because low-abundance stuff can't be seen anyway, but if you have a
#'   situation where you have more than 30 things that actually need to be
#'   distinguished, you'll need to provide your own vector. Also, if you are in
#'   that situation, try to find another way to do what you are doing. People
#'   cannot generally distinguish anywhere near 30 colours on a single plot.
#' @export
tax_colours = c("#87c5ab","#eea27c","#a9a8d2","#ffff99","#9999ff","#fb8072",
                "#80b1d3","#fdb462","#b3de69","#fccde5","#bc80bd","#ccebc5",
                "#ffed6f","#a6cee3","#fb9a99","#fdbf6f","#d38d99","#b3b3ff",
                "#d6daba","#72edfb","#d3a280","#80b1d3","#9469de","#cdfce4",
                "#81bd80","#e4c5eb","#6f81ff","#e3bba6","#99fafb","#6fadfd")
