# order_levs ---------------------------------------------------------------

#' Order the levels of one factor by the values of another
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' This function will be deprecated because it is just recapitulating
#' functionality provided by `ggplot2::facet_grid()`. If you are using this
#' function and don't want it it go away, please get in touch and let me know
#' what you're using it for.
#'
#' `order_levs()` takes two factors, the first of which has values that are
#' nested within the values of the second, and orders the levels of the first
#' factor such that they are clustered within the second factor.
#'
#' @section Value: A factor that has the same levels as f1, but that has been
#'   re-ordered.
#'
#' @param f1 The factor to re-order. Levels of this factor must be nested within
#'   `f2`
#' @param f2 The factor to use when re-ordering `f1`.
#' @export
order_levs = function(f1,...){
    lifecycle::deprecate_warn('0.1.0', 'order_levs()')

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
