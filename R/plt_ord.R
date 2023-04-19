## Plot the ordination --------------------------------------------------------
### plt_ord ------------------------------------------------------------------

#' Plot the ordination
#'
#' Make a multi-axis PCoA plot from the data frame
#'
#' @section Details:
#'
#' @section Value:
#'
#' @param ord_long The ordination data frame produced by
#'   \code{\link{make_ord_df}}
#' @param colour The name of the column to use to colour the points.
#' @param shape The name of the coloumn governing the shape of the points.
#' @param size The size of the points. Passed to the
#'   \code{\link[ggplot2]{geom_point}} size argument and defaults to 1.
#' @param pt_alph The transparency of the points. Default is 0.7
#' @export
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
