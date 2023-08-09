#' Tests if an object is a binary vector.
#'
#' Tests if an object is a vector of type "logical", or a numeric vector
#' where each element is indistinguishable from zero or one. If `x` is a factor
#' then FALSE is returned.
#'
#' @param x  Object to test.
#' @param tol A numeric value. Tolerance used to determine if numerical value
#'   is indistinguishable from zero or one.
#' @param allow_na A logical value. Allow NA values in binary vector?
#'
#' @return A logical.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   .is_binary(c(1,0))
#' }
.is_binary <- function(x,
                       tol = .Machine$double.eps^0.5,
                       allow_na = FALSE) {

  if (!is.numeric(tol)) {
    stop(
      paste0(
        "`tol` must be numeric value. ",
        '`tol` has type "', typeof(tol), '".'
      )
      ,call. = FALSE
    )
  }
  if (length(tol) != 1L) {
    stop(
      paste0(
        "`tol` must have length 1. ",
        "`tol` has length ", length(tol), "."
      )
      , call. = FALSE
    )
  }
  if (!is.logical(allow_na)) {
    stop(paste0(
        "`allow_na` must be a logical value",
        '`allow_na` has type "', typeof(allow_na), '".'
      )
      , call. = FALSE
    )
  }
  if (length(allow_na) != 1L) {
    stop(
      paste0(
        "`allow_na` must have length 1. ",
        "`allow_na` has length ", length(allow_na), "."
      )
      , call. = FALSE
    )
}

  if (!(is.numeric(x) || is.logical(x))) {
    return(FALSE)
  } else if (!allow_na && any(is.na(x))) {
    return(FALSE)
  } else {
    x <- x[!is.na(x)]
    return(all(abs(x - 0L) < tol | abs(x - 1L) < tol))
  }
}

#' Check common arguments
#'
#' Check that input to common arguments of internal weighted statistical
#' summary functions are valid.
#'
#' @param x A numeric or logical vector.
#' @param w A numeric vector of non-negative numbers. Weights associated with
#'  `x`. Default is NULL, ie no weights are used.
#' @param na.rm A logical value. Should missing values (including NaN) in `x`
#'   be removed?
#'
#' @return invisible(NULL) or an error if an argument is invalid
#' @keywords: internal
#'
#' @examples
#' \dontrun{
#'  .winputcheck(1:5, NULL, TRUE)
#' }
.winputcheck <- function(x, w, na.rm) {
  if (is.factor(x)) {
    stop(
      paste0(
        "`x` must be a numeric or logical vector. ",
        "`x` is a factor."
      )
      , call. = FALSE
    )
  } else if (!(is.numeric(x) || is.logical(x))) {
    stop(
      paste0(
        "`x` must be a numeric or logical vector. ",
        '`x` has type "', typeof(x), '".'
      )
      , call. = FALSE
    )
  } else if (is.factor(w)) {
    stop(
      paste0(
        "`w` must be a numeric vector. ",
        "`w` is a factor."
      )
      , call. = FALSE
    )
  } else if (!(is.numeric(w) || is.null(w))) {
    stop(
      paste0(
        "`w` must be a numeric vector. ",
        '`w` has type "', typeof(w), '".'
      )
      , call. = FALSE
    )
  } else if (!is.null(w) && length(x) != length(w)) {
    stop(
      paste0(
        "`x` and `w` must have the same length. ",
        "`x` has length ", length(x), " and ",
        "`w` has length ", length(w), "."
      )
      , call. = FALSE
    )
  } else if (any(is.na(w))) {
    stop(
      paste0(
        "`w` must have non-negative elements. ",
        "One or more element of `w` is NA/NaN."
      )
      , call. = FALSE
    )
  } else if (any(w < 0)) {
    stop(
      paste0(
        "`w` must have non-negative elements. ",
        "One or more element of `w` is < 0."
      )
      , call. = FALSE
    )
  } else if (!is.logical(na.rm)) {
    stop(
      paste0(
        "`na.rm` must be a logical value. ",
        '`na.rm` has type "', typeof(na.rm), '".'
      )
      , call. = FALSE
    )
  } else if (length(na.rm) != 1L) {
    stop(
      paste0(
        "`na.rm` must have length 1. ",
        '`na.rm` has length "', length(na.rm), '".'
      )
      , call. = FALSE
    )
  }

  invisible(NULL)
}


#' Weighted arithmetic mean
#'
#' Calculates the weighted arithmetic mean of a numeric/logical vector `x` with
#' associated weights `w`.
#'
#' @inheritParams .winputcheck
#'
#' @return A numeric value.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   .wmean(x = 1:2, w = 1:2)
#' }
.wmean <- function(x,
                   w = NULL,
                   na.rm = FALSE) {
  .winputcheck(x, w, na.rm)
  if (is.null(w)) {
    w <- rep(1, length(x))
  }
  if (na.rm) {
    w <- w[!is.na(x)]
    x <- x[!is.na(x)]
  }

  sum(w*x)/sum(w)
}



#' Weighted quantile
#'
#' Calculates weighted type 7 quantile. Simplified version of code from
#' https://arxiv.org/pdf/2304.07265.pdf.
#' TODO: need to read up on this to make sure everything makes sense.
#'
#' @inheritParams .wmean
#' @param prob Numeric value. Quantile to estimate.
#'
#' @return A numeric.
#' @keywords: internal
#'
#' @examples
#' \dontrun{TODO}
.wquantile <- function(x,
                       w,
                       prob,
                       na.rm = FALSE) {
  .winputcheck(x, w, na.rm)
  n <- length(x)
  if (is.null(w)) {
    w <- rep(1L, n)
  }

  if (na.rm & any(is.na(x))) {
    w <- w[!is.na(x)]
    x <- x[!is.na(x)]
  }

  # kish_ess
  nw <- sum(w)**2 / sum(w**2)

  indexes <- order(x)
  x <- x[indexes]
  w <- w[indexes]
  w <- w / sum(w)
  t <- cumsum(c(0, w))

  cdf_values <- (nw - 1) * prob + 1
  cdf_values <- max(min(cdf_values, nw), 1)
  cdf_values <- pmax(0, pmin(1, t * nw - cdf_values + 1))
  W <- utils::tail(cdf_values, -1) - utils::head(cdf_values, -1)
  sum(W * x)
}


#' Weighted standard deviation
#'
#' Calculates the weighted standard deviation(sd) of a vector `x` with
#' associated weights `w`.
#'
#' The standard deviation of a length-one or zero-length vector is NA.
#'
#' @inheritParams .winputcheck
#'
#' @return A numeric value.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   .wsd(x = 1:5)
#' }
.wsd <- function(x,
                 w = NULL,
                 na.rm = FALSE) {
  .winputcheck(x, w, na.rm)
  if (is.null(w)) {
    w <- rep(1L, length(x))
  }
  if (na.rm) {
    w <- w[!is.na(x)]
    x <- x[!is.na(x)]
  }

  if (length(x) %in% c(0L, 1L)) {
    return(NA_real_)
  } else {
    m <- length(w[w != 0])
    sd <- sqrt(sum(w*(x - .wmean(x, w))**2) / ((m - 1) / m*sum(w)))
    return(sd)
  }
}


#' Weighted sum of vector elements
#'
#' Calculates the weighted sum of a numeric vector `x` with associated
#' weights `w`.
#'
#' @inheritParams .wmean
#'
#' @return A numeric.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'    .wsum(x = 1:2, w = rep(2, 2))
#' }
.wsum <- function(x,
                  w = NULL,
                  na.rm = FALSE) {

  .winputcheck(x, w, na.rm)
  if (is.null(w)) {
    w <- rep(1, length(x))
  }
  if (na.rm) {
    w <- w[!is.na(x)]
    x <- x[!is.na(x)]
  }

  sum(w*x)
}


#' Validates a 'tblsummary' object
#'
#' Validate a 'tblsummary' object. Invisibly returns `x` or throws an error.
#' @param x Object to validate.
#'
#' @return invisible(`x`).
#' @keywords: internal
#'
#' @examples
#' \dontrun{
#'   x <- data.frame(var = 1:5)
#'   tbl <- .summarize_var(x, var = "var")
#'   .validate.tblsummary(tbl)
#'   .validate_tblsummary(data.frame())
#' }
.validate_tblsummary <- function(x) {

  if(!is.data.table(x)) {
    stop("'x' is not a data.table", call. = FALSE)
  }

  # TODO: implement checking that data.table has correct columns

  # TODO: implement cheking that .n_var_level and .n_by_level sums correctly

  # TODO: implement cheking thtt .by has a ".all" category. Can only be done
  # if other functions remove by_total_val flexibility?



  invisible(x)
}
