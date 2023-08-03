# Quiet R CMD check notes of the type "no visible binding for global variable..."
utils::globalVariables(c(".mask"))

#' Mask person-sensitive counts
#'
#' Masks person-sensitive counts in a 'tblsummary', or more precisely, adds
#' a variable to the input table, that can be used to do the masking.
#'
#' @param x data.table.
#'
#' @return data.table
#' @export
#'
#' @examples
#' x <- data.frame(
#'   var = c("A", "B", "B", rep("C", 5L))
#' )
#' tbl1 <- summarize_df(x, var = "var")
#' tbl2 <- mask_tbl(tbl1)
 mask_tbl <- function(x) {

# Take as input, output from summarize_df (rename to summarize_tbl?). There
# needs to be some sort of input check to see if the input data.frame has the
# right structure. Probably a good idea to make a "validation" utility function
# that checks that a data.frame conforms with a "dfsummary table". Probably best
# to do this as informally as possible in line with the goal not to make the
# tabel objects "complicated". Check for correct variable names, and that
# .n_var_level stuff sums correctly to .n_by_level stuff?

# NOTE: Name reflects probably package renaming to tblsummary
x <- .validate_tblsummary(data.table::copy(x))

# Restructure data.table if necessary. Use to utility function to do this?

# Mask table / make indicator variables that can be used to mask table.
x[, .mask := 0L]

# Primary masking
x[, .mask := fifelse(0 < .n_var_level & .n_var_level < 5, 1L, .mask)]

# Secondary masking


# Restructure back if restructured earlier. Again, utility function for going
# back and forth might be a good idea to not clutter the function with
# potential longer code for formatting data.


x
}

