# order_levs ---------------------------------------------------------------

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
#' @export
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

# rotate_ticks --------------------------------------------------------------

#' Rotate the x tick labels 90 degrees and position them correctly.
#'
#' @export
rotate_ticks = function(){
    theme(axis.text.x = element_text(size = 10,
                                     angle = 90,
                                     hjust = 1,
                                     vjust = 0.5))
}
