#' Checks if a vector is binary(-ish)
#'
#' Check if each element of a vector can be interpreted as binary.
#'
#' A vector element is deemed to be binary if it is of type "logical", or
#' if it is a numeric value that is indistinguishable from zero or one based
#' on a tolerence given in `tol`.
#'
#' @param x  A numeric or logical vector
#' @param tol Numeric value. Tolerance.
#' @param allow_na A logical. Should NA values in `x` be accepted as binary?
#'
#' @return A logical
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   .is_binary(c(1,0,1))
#' }
.is_binary <- function(x,
                       tol = .Machine$double.eps^0.5,
                       allow_na = FALSE) {

  if (!(is.vector(x) || is.null(x))) {
    stop("`x` is not a vector or NULL", call. = FALSE)
  }
  if (!is.numeric(tol)) {
    stop("`tol` is not a numeric value", call. = FALSE)
  }
  if (!is.logical(allow_na)) {
    stop ("`na.rm` is not a logical", call. = FALSE)
  }

  return_val <- FALSE
  if(is.numeric(x) || is.logical(x)) {
    return_val <- all(abs(x - 0L) < tol | abs(x - 1L) < tol, na.rm = allow_na)
    # If return_val is NA at this point, this always means that `x` is not
    # binary, regardless of the value of `allow_na`.
    if (is.na(return_val)) {
      return_val <- FALSE
    }
  }
  return_val
}

#' Check input for internal functions
#'
#' Utility function for checking input to internal functions of weighted
#' versions of statistical functions
#' @param x A numeric or logical vector.
#' @param w A numeric vector of non-negative numbers or NULL
#' @param na.rm Boolean.
#'
#' @return NULL
#' @keywords: internal
#'
#' @examples
#' \dontrun{
#'  .w_inputcheck(1:5, NULL, TRUE)
#' }
.w_inputcheck <- function(x, w, na.rm) {
  if(!(is.numeric(x) || is.logical(x))) {
    stop("`x` must be a numeric or logical vector", call. = FALSE)
  } else if(!(is.numeric(w) || is.null(w))) {
    stop("`w` must be a numeric vector", call. = FALSE)
  } else if (!is.null(w) && length(x) != length(w)) {
    stop("`x` and `w` must have the same length", call. = FALSE)
  } else if (any(w[!is.na(w)] < 0)) {
    stop("`w` must have non-negative elements", call. = FALSE)
  } else if (!is.logical(na.rm)) {
    stop("`na.rm` is not a logical", call. = FALSE)
  }
  invisible(NULL)
}

#' Weighted arithmetic mean
#'
#' Calculates the weighted arithmetic mean of a vector `x` with
#' associated weights `w`.
#'
#' @param x A numeric or logical vector.
#' @param w A numeric vector of non-negative numbers. Weights associated with `x`.
#' @param na.rm Boolean. Should NA values in `x` and values in `x` with
#'   NA weights in `w` be removed?
#'
#' @return A numeric.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'    x <- 1:5
#'    w <- 1:5
#'   .wmean(x, w)
#' }
.wmean <- function(x,
                   w = NULL,
                   na.rm = FALSE) {
  .w_inputcheck(x, w, na.rm)
  if (is.null(w)) {
    w <- rep(1, length(x))
  }
  if (na.rm) {
    any_na <- is.na(x) | is.na(w)
    x <- x[!any_na]
    w <- w[!any_na]
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
  .w_inputcheck(x, w, na.rm)
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
#' @inheritParams .wmean
#'
#' @return A numeric.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'    x <- 1:5
#'    w <- rep(2, 5)
#'   .wsd(x, w)
#' }
.wsd <- function(x,
                 w = NULL,
                 na.rm = FALSE) {
  .w_inputcheck(x, w, na.rm)
  if (is.null(w)) {
    w <- rep(1L, length(x))
  }
  if (na.rm) {
    any_na <- is.na(x) | is.na(w)
    x <- x[!any_na]
    w <- w[!any_na]
  }

  m <- length(w[w != 0])
  sqrt(sum(w*(x - .wmean(x, w))**2) / ((m - 1) / m*sum(w)))
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
#'    x <- 1:5
#'    w <- rep(2, 5)
#'    .wsum(x, w)
#' }
.wsum <- function(x,
                  w = NULL,
                  na.rm = FALSE) {
  .w_inputcheck(x, w, na.rm)
  if (is.null(w)) {
    w <- rep(1, length(x))
  }
  if (na.rm) {
    any_na <- is.na(x) | is.na(w)
    x <- x[!any_na]
    w <- w[!any_na]
  }

  sum(w*x)
}
