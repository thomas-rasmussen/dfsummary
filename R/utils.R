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
#'  `x`.
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
  } else if (!is.numeric(w)) {
    stop(
      paste0(
        "`w` must be a numeric vector. ",
        '`w` has type "', typeof(w), '".'
      )
      , call. = FALSE
    )
  } else if (length(x) != length(w)) {
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
#' Calculates the weighted arithmetic mean of a numeric vector `x` with
#' associated weights `w`.
#'
#' Formula used:
#' \deqn{\bar{x} = \frac{\sum_{i = 1}^n{w_i*x_i}}{\sum_{i = 1}^n{w_i}}}
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
                   w = rep(1L, length(x)),
                   na.rm = FALSE) {
  .winputcheck(x, w, na.rm)
  if (na.rm) {
    w <- w[!is.na(x)]
    x <- x[!is.na(x)]
  }
  sum(w*x)/sum(w)
}


#' Weighted quantile
#'
#' Estimates quantiles using the weighted Harrell-Davis quantile estimator.
#'
#' Implemented based on https://arxiv.org/pdf/2304.07265.pdf.
#'
#' @inheritParams .winputcheck
#' @param prob Numeric vector of probabilities with values in \verb{[0;1]}
#'   corresponding to quantiles to estimate.
#'
#' @return A numeric vector.
#' @keywords: internal
#'
#' @examples
#' \dontrun{
#'   x <- 1:5
#'   w <- c(1, 0, 1, 0, 1)
#'   .wquantile(x, w, prob = 0.5)
#' }
.wquantile <- function(x,
                       w = rep(1L, length(x)),
                       prob,
                       na.rm = FALSE) {

  # Input checks
  .winputcheck(x, w, na.rm)
  if (is.factor(prob)) {
    stop(
      paste0(
        "`prob` must be a numeric vector. ",
        "`prob` is a factor."
      )
      , call. = FALSE
    )
  } else if (!is.numeric(prob)) {
    stop(
      paste0(
        "`prob` must be a numeric vector. ",
        '`prob` has type "', typeof(prob), '".'
      )
      , call. = FALSE
    )
  } else if (any(prob < 0 | prob > 1)) {
    stop(
      paste0(
        "`prob` must take values in [0;1]. ",
        '`prob` takes values outside [0;1].'
      )
      , call. = FALSE
    )
  }

  if (na.rm & any(is.na(x))) {
    w <- w[!is.na(x)]
    x <- x[!is.na(x)]
  }

  # Calculate Kish's effective sample size
  n <- sum(w)**2 / sum(w**2)

  idx<- order(x)
  x <- x[idx]
  w <- w[idx]


  w <- w / sum(w)
  cdf_probs <- cumsum(c(0, w))

  sapply(prob, function(p) {
    q <- stats::pbeta(cdf_probs, (n + 1) * p, (n + 1) * (1 - p))
    wni <- utils::tail(q, - 1) - utils::head(q, - 1)
    sum(wni * x)
  })
}


#' Weighted standard deviation
#'
#' Calculates the weighted standard deviation(sd) of a vector `x` with
#' associated weights `w`.
#'
#' The standard deviation of a length-one or zero-length vector is NA.
#' Formula used:
#' \deqn{sd =
#'   \sqrt{
#'    \frac{
#'      \sum_{i = 1}^n w_i(x_i - \bar{x})^2
#'    }
#'    {
#'      \frac{(m-1)\sum_{i = 1}^n w_i}{m}
#'    }
#'   }
#' }
#' where \eqn{m} is the number of non-zero weights, and \eqn{\bar{x}} is the
#' weighted mean of the observations.
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
                 w = rep(1L, length(x)),
                 na.rm = FALSE) {
  .winputcheck(x, w, na.rm)
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
#' Formula used:
#' \deqn{x. = \sum_{i = 1}^n{w_i*x_i}}
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
                  w = rep(1L, length(x)),
                  na.rm = FALSE) {

  .winputcheck(x, w, na.rm)
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
