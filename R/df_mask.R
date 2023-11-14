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
#'   tbl2 <- df_mask(tbl1)
#' }
df_mask <- function(x) {

  x1 <- data.table::copy(x)

  # Initialize masking variables.
  x1[, `:=`(
      .n_var_level_mask_flag = FALSE,
      .n_by_level_mask_flag = FALSE
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
        new_mask_flag <- .update_mask_flags(
          x = x2[.var_name == i_var & .by == j_by_level]$.n_var_level,
          mask_flags = x2[.var_name == i_var & .by == j_by_level]$.n_by_level_mask_flag
        )
        x2[.var_name == i_var & .by == j_by_level]$.n_var_level_mask_flag <- new_mask_flag
      }
    } else {
      all_var_level <- unique(x2$.var_level)
      for (j in seq_along(all_var_level)) {
        j_var_level <- all_var_level[j]
        new_mask_flag <- .update_mask_flags(
          x = x2[.var_name == i_var & .var_level == j_var_level]$.n_var_level,
          mask_flag = x2[.var_name == i_var & .var_level == j_var_level]$.n_var_level_mask_flag
        )
        x2[.var_name == i_var & .var_level == j_var_level]$.n_var_level_mask_flag <- new_mask_flag
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

tmp <- unique(x2[
  , .(.by, .n_by_level, .n_by_level_mask_flag)
  ])
new_mask_flag <- .update_mask_flags(
      x = tmp$.n_by_level,
      mask_flag = tmp$.n_by_level_mask_flag
    )

tmp$.n_by_level <- new_mask_flag

x3 <- tmp[x2[, .n_by_level_mask_flag := NULL], on = ".by"]

x3
}


#### Helpers ####

#' title
#'
#' TODO:
#' - NA's in x should be considered invaild input? There is no case where that
#'   makes sense? and that assumption is also used in code.
#'- Loop until nothing more is flagged for masking.
#' - Change function so that input is a vector and a corresponding
#'   vector with flags for which elements are do be flagged (so far).
#'   the function then updates the flag vector and returns that updated
#'   vector of flags
#' - Function should also be able to take a vector with flags for values that
#'   are protected from masking, eg zeros that are known to be zeros and therefore are not valid to mask
#'   (very relevant, something we want to be able to control)
#' - Protect ruins things, if protected cell NEEDS to be masked, eg if count under 5?
#'   function will loop until max iteration reached? Abandon approach? If the protected
#'   cells leads to problems then function can return warning/error?
#'
#' @param x a
#' @param mask_flags a
#' @param mask_min a
#' @param mask_max a
#' @param mask_avg a
#'
#' @return a
#' @keywords internal
#'
#' @examples
#' \dontrun{TODO}
.update_mask_flags <- function(
    x,
    mask_flags = rep(FALSE, length(x)),
    protect = rep(FALSE, length(x)),
    mask_min = 1L,
    mask_max = 4L,
    mask_avg = 1L,
    max_ite = 100L) {

  if (any(is.na(x))) {
    stop("x has NA values", call. = FALSE)
  }
  if (!is.logical(mask_flags)) {
    stop("mask_flags must be a logical vector", call. = FALSE)
  }
  if(any(is.na(mask_flags))) {
    stop("mask_flags has NA values", call. = FALSE)
  }
  if (!is.logical(protect)) {
    stop("protect must be a logical vector", call. = FALSE)
  }
  if (any(is.na(protect))) {
    stop("protect has NA values", call. = FALSE)
  }
  if (length(x) != length(mask_flags)) {
    stop("x and mask_flags does not have the same length", call. = FALSE)
  }
  if (length(x) != length(protect)) {
    stop("x and protect does not have the same length", call. = FALSE)
  }

  x_sum <- sum(x)
  ite <- 0L
  continue <- TRUE

  # Flag elements for masking until no new flags are added

  while(continue) {

    mask_flags_current <- mask_flags
    ite <- ite + 1L

    # Identify minimum count
    n_masked <- sum(mask_flags)
    if (n_masked == length(x)) {
      return(mask_flags)
    } else {
      # pick first min value in table if multiple cells have the same minimum
      min_val_idx <- which(x == min(x[!mask_flags]) & !mask_flags)[1]
    }

    ### Primary masking ###
    # If lowest non-masked value is deemed person-sensitive, flag it for masking
    if (mask_min <= x[min_val_idx] & x[min_val_idx] <= mask_max) {
      mask_flags[min_val_idx] <- TRUE

    ### Secondary masking ###
    # If exactly one value is masked, flag the lowest non-masked value for masking
    } else if (n_masked == 1L) {
      mask_flags[min_val_idx] <- TRUE
    # If the average of masked counts is lower than the given threshold,
    # mask the lowest non-masked count.
    } else if (
        n_masked != 0L
        && ((sum(x[mask_flags]) / n_masked <= mask_avg))) {
      mask_flags[min_val_idx] <- TRUE
    }

    if (identical(mask_flags_current,mask_flags)) continue <- FALSE
    if (ite > max_ite) continue <- FALSE
  }

  return(mask_flags)
}

