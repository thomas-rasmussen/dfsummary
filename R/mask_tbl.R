# Quiet R CMD check notes of the type "no visible binding for global variable..."
utils::globalVariables(c(
  ".mask", ".mask_by_level", ".mask_var_level", ".n_by_level_old",
  ".n_var_level_old"
))

#' Mask person-sensitive counts
#'
#' Masks person-sensitive counts in a 'tblsummary', or more precisely, adds
#' a variable to the input table, that can be used to do the masking.
#'
#' @param x data.table.
#'
#' @return data.table
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   x <- data.frame(
#'     var = c("A", "B", "B", rep("C", 5L))
#'   )
#'   tbl1 <- summarize_df(x, var = "var")
#'   tbl2 <- mask_tbl(tbl1)
#' }
 mask_tbl <- function(x) {

# Take as input, output from summarize_df (rename to summarize_tbl?). There
# needs to be some sort of input check to see if the input data.frame has the
# right structure. Probably a good idea to make a "validation" utility function
# that checks that a data.frame conforms with a "dfsummary table". Probably best
# to do this as informally as possible in line with the goal not to make the
# tabel objects "complicated". Check for correct variable names, and that
# .n_var_level stuff sums correctly to .n_by_level stuff?

# NOTE: Name reflects probably package renaming to tblsummary
x1 <- .validate_tblsummary(data.table::copy(x))

x1[, `:=`(
    .n_var_level_old = .n_var_level,
    .n_by_level_old = .n_by_level
  )]

all_vars <- unique(x1$.var_name)
x2 <- data.table::copy(x1)

for (i in seq_along(all_vars)) {
  i_var <- all_vars[i]
  cnt <- 0L
  cnt_no_change <- 0L
  stop <- FALSE
  while (!stop) {
    x2_prev_ite <- data.table::copy(x2)

    # Alternative between masking .n_var_level in strata of by level and var level
    if (cnt %% 2L == 0) {
      all_by_level <- unique(x2$.by)
      for (j in seq_along(all_by_level)) {
        j_by_level <- all_by_level[j]
        new_vector <- .mask_vector_element(
          x = x2[.var_name == i_var & .by == j_by_level]$.n_var_level,
          x_sum = x2[.var_name == i_var & .by == j_by_level]$.n_by_level_old[1]
        )
        x2[.var_name == i_var & .by == j_by_level]$.n_var_level <- new_vector
      }
    } else {
      all_var_level <- unique(x2$.var_level)
      for (j in seq_along(all_var_level)) {
        j_var_level <- all_var_level[j]
        new_vector <- .mask_vector_element(
          x = x2[.var_name == i_var & .var_level == j_var_level]$.n_var_level,
          x_sum = sum(x2[.var_name == i_var & .var_level == j_var_level]$.n_var_level_old)
        )
        x2[.var_name == i_var & .var_level == j_var_level]$.n_var_level <- new_vector
      }
    }

    if (identical(x2_prev_ite, x2)) {
      cnt_no_change <- cnt_no_change + 1L
    }
    cnt <- cnt + 1L
    if (cnt > 100L) stop <- TRUE
    if (cnt_no_change > 2L) stop <- TRUE
  }
}


### Ad-hoc masking of .n_by_level ###

# TODO: probably something that can/should be done differently than some ad hoc
# fix as done right now? Feels like the masking procedure could be further
# generalized. Solution right now is also super hackish, just to make current
# unit tests pass. Not certain this is a general solution that always work.

tmp <- unique(x2[, .(.var_name, .by, .n_by_level_old = .n_by_level, .n_by_level)])

all_vars <- unique(tmp$.var_name)
for (i in seq_along(all_vars)) {
  i_var <- all_vars[i]
  cnt <- 0L
  cnt_no_change <- 0L
  stop <- FALSE
  while (!stop) {
    tmp_prev_ite <- data.table::copy(tmp)

    new_vector <- .mask_vector_element(
      x = tmp[.var_name == i_var & .by != ".all"]$.n_by_level,
      x_sum = tmp[.var_name == i_var & .by == ".all"]$.n_by_level_old
    )
    tmp[.var_name == i_var & .by != ".all"]$.n_by_level <- new_vector

    if (identical(tmp_prev_ite, tmp)) {
      cnt_no_change <- cnt_no_change + 1L
    }
    cnt <- cnt + 1L
    if (cnt > 100L) stop <- TRUE
    if (cnt_no_change > 2L) stop <- TRUE
  }
}

# Mask grand total if needed
# Also very ad-hoc and hackish
tmp[.by == ".all"]$.n_by_level <- fifelse(
  tmp[.by == ".all"]$.n_by_level < 5,
  NA_real_,
  tmp[.by == ".all"]$.n_by_level
)


tmp <- tmp[, .n_by_level_old := NULL]
x2 <- x2[, .n_by_level := NULL]

x3 <- tmp[x2, on = c(".by", ".var_name")]

### Make masking indicators ###

x4 <- x3[
    , `:=`(
      .mask_by_level = fifelse(is.na(.n_by_level), 1L, 0L),
      .mask_var_level = fifelse(is.na(.n_var_level), 1L, 0L)
    )
  ]

  x4[, `:=`(.n_by_level = NULL, .n_var_level = NULL)]
  setnames(
    x4,
    c(".n_by_level_old", ".n_var_level_old"),
    c(".n_by_level", ".n_var_level")
  )

  x4
}


#### Helpers ####

 # TODO: Rename to .mask_vector and add looping to make it possible to
 # mask vector in one go.


#' TODO
#'
#' @param x a
#' @param x_sum a
#' @param min_mask a
#' @param max_mask a
#' @param avg_mask a
#'
#' @return a
#' @keywords internal
#'
#' @examples
#' \dontrun{TODO}
.mask_vector_element <- function(
    x,
    x_sum,
    min_mask = 1L,
    max_mask = 4L,
    avg_mask = 1L) {

  n_masked <- sum(is.na(x))
  if (all(is.na(x))) {
    min_val_idx <- NA
  } else {
    min_val_idx <- which(x == min(x, na.rm = TRUE))[1]
  }

  if (is.na(min_val_idx)) {
    return(x)
  }
  ### Primary masking ###
  # If lowest non-masked value is deemed person-sensitive, mask it.
  if (min_mask <= x[min_val_idx] & x[min_val_idx] <= max_mask) {
    x[min_val_idx] <- NA_real_

  ### Secondary masking ###
  # If exactly one value is masked, mask the lowest non-masked value
  } else if (n_masked == 1L) {
    x[min_val_idx] <- NA_real_
  # If the average of masked counts is lower than the given threshold,
  # mask the lowest non-masked count.
  } else if (
      n_masked != 0L
      && (((x_sum - sum(x, na.rm = TRUE)) / n_masked <= avg_mask))) {
    x[min_val_idx] <- NA_real_
  }

  return(x)
}

