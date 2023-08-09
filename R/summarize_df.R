# Quiet R CMD check notes of the type "no visible binding for global variable..."
utils::globalVariables(c(
  ".", ".var", ".weight", ".by", ".var_level", ".var_name", ".n_var_level"
))


#' Summarizes variables in a data.frame
#'
#' Returns a descriptive summary of variables in a data.frame.
#'
#' @param vars Character vector. Names of variables in `x` to summarize.
#'   Variables types can be specified by naming elements of `var`. Variable
#'   types are: "bin" (binary), "cont" (continuous), "cat" (categorical).
#'   If ny variable type is specified, the type is automatically deduced from
#'   the data.
#' @inheritParams .summarize_var
#'
#' @return A data.table containing a descriptive summary of `vars`.
#' @import data.table
#' @export
#'
#' @examples
#' x <- data.frame(var1 = 1:10, var2 = c(rep("A", 5), rep("B", 5)))
#' summarize_df(
#'   x,
#'   vars = c(cont = "var1", "var2")
#' )
summarize_df <- function(x,
                         vars,
                         weight = NULL,
                         by = NULL,
                         by_total_val = ".all") {

  ### Input checks ###

  if (!is.data.frame(x)) {
    stop(
      paste0(
        "`x` must be a data.frame. ",
        '`x` has type "', typeof(x), '".'
      )
      , call. = FALSE
    )
  }
  if (!is.character(vars)) {
    stop(
      paste0(
        "`vars` must be a character vector",
        '`x` has type "', typeof(x), '".'
      )
      , call. = FALSE
    )
  }


  ### Summarize data ###

  types <- names(vars)
  if (is.null(types)) {
    types <- rep("", length(vars))
  }
  vars <- as.character(vars)

  for (i in seq_along(vars)) {
    i_var <- vars[i]
    i_type <- types[i]
    if (i_type == "") i_type <- NULL

    tmp <- .summarize_var(
      x = x,
      var = i_var,
      type = i_type,
      weight = weight,
      by = by,
      by_total_val = by_total_val
    )

    if (i == 1L) {
      out <- tmp
    } else {
      out <- rbindlist(list(out, tmp))
    }
  }

  data.table::setcolorder(out, ".by")
  out[order(.by, .var_name, .var_level)]
}


#### Helpers ####

#' Checks if variable exists in data.frame.
#'
#' Checks that a given variable name is a variable name in input data.frame.
#'
#' @param x. Input data.frame.
#' @param var String. Variable name.
#'
#' @returns invisible(NULL) or an error if an input argument is invalid.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   x <- data.frame(var1 = 1:2)
#'   .is_var_name(x, "var1")
#' }
.is_var_name <- function(x, var) {
  if (!is.data.frame(x)) {
    stop(
      paste0(
        "`x` must be a data.frame. ",
        '`x` has type "', typeof(x), '".'
      )
      , call. = FALSE
    )
  } else if (!is.character(var)) {
    stop(
      paste0(
        "`var` must be a string. ",
        '`var` has type "', typeof(var), '".'
      )
      , call. = FALSE
    )
  } else if (length(var) != 1L) {
    stop(
      paste0(
        "`var` must have length 1. ",
        "`var` has length ", length(var), "."
      )
      , call. = FALSE
    )
  } else if (!var %in% names(x)) {
    stop(
      paste0(
        "`var` must be the name of a variabe in `x`. ",
        '"', var, '" is not a variable name in `x`.'
      )
      , call. = FALSE
    )
  }

  invisible(NULL)
}

#' Summarizes a variable
#'
#' Creates a descriptive summary of a variable in a data.frame.
#'
#' If `type` is NULL, the variable type of `var` will be deduced from the data:
#' 1) If the variable takes "binary" values, eg is a logical vector, or a numeric
#'    vector with zero and one's, the `type` is set to "bin".
#' 2) Else, if the variable is numeric then `type` is set to "cont".
#' 3) Else, `type` is set to "cat".
#'
#' @param x Input data.frame.
#' @param var String. Name of variable in `x` to summarize.
#' @param type String. Variable type of `var`: "bin" (binary),
#'   "cont" (continuous), "cat" (categorical). Default is NULL, meaning that
#'   the type is automatically deduced from the data. See details for more
#'   information.
#' @param weight String. Name of variable in `x` that contains
#'   observation weights. Must be a numeric variable, with non-negative
#'   numbers. Default is NULL, ie no weights are used.
#' @param by String. Name of variable to group by while summarizing. Default is
#' NULL, ie no grouping variable is used.
#' @param by_total_val String. Name given to overall by-group.
#'
#' @return A data.table containing a descriptive summary of `var`.
#' @keywords internal
#' @import data.table
#'
#' @examples
#' \dontrun{
#'   x <- data.frame(var = 1:10)
#'   .summarize_var(x, "var")
#' }
.summarize_var <- function(x,
                           var,
                           type = NULL,
                           weight = NULL,
                           by = NULL,
                           by_total_val = ".all") {

  ### Input checks ###

  # `x`
  if (!is.data.frame(x)) {
    stop(
      paste0(
        "`x` must be a data.frame. ",
        '`x` has type "', typeof(x), '".'
      )
      , call. = FALSE
    )
  }
  # `var`
  .is_var_name(x, var)
  # `type`
  if (is.null(type)) {
  } else if (!is.character(type)) {
    stop(
      paste0(
        "`type` is not a string. ",
        '`type` has type "', typeof(type), '".'
      )
      , call. = FALSE
    )
  } else if (length(type) != 1L) {
    stop(
      paste0(
        "`type` must have length 1. ",
        "`type` has length ", length(type), "."
      )
      , call. = FALSE
    )
  } else if (!type %in% c("bin", "cat", "cont")) {
    stop(
      paste0(
        "`type` does not have a valid value. ",
        '`type` must have one of the following values: "bin", "cat", "cont". ',
        '`type` has value "', type, '".'
      )
      , call. = FALSE
    )
  }
  # `weight`
  if (is.null(weight)) {
  } else {
    .is_var_name(x, weight)
    if (any(x[[weight]] < 0)) {
      stop(
        paste0(
          "`weight` must be a numeric vector with non-negative elements. ",
          "`weight` has one or more elements < 0"
        )
        , call. = FALSE
      )
    }
  }
  # `by`
  if (!is.null(by)) {
    .is_var_name(x, by)
  }
  # `by_total_val`
  if (!is.character(by_total_val)) {
    stop(
      paste0(
        "`by_total_val` must be a string. ",
        '`by_total_val` has type "', typeof(by_total_val), '".'
      )
      , call. = FALSE
    )
  } else if (length(by_total_val) != 1L) {
    stop(
      paste0(
        "`by_total_val` must have length 1. ",
        "`by_total_val` has length ", length(by_total_val), "."
      )
      , call. = FALSE
    )
  }

  ### Process input ###

  # Standardize variable names
  dt  <- data.table::as.data.table(x)
  setnames(dt, var, ".var")
  if (is.null(weight)) {
    dt[, .weight := 1L]
  } else {
    setnames(dt, weight, ".weight")
  }
  if (!is.null(by)) {
    setnames(dt, by, ".by")
    dt[, .by := as.character(.by)]
    dt <- dt[, .(.by, .var, .weight)]
  } else {
    dt <- dt[, .(.var, .weight)]
  }


  # If type is given, check that it is compatible with '.var'
  if (!is.null(type)) {
    if (type == "bin" && !.is_binary(dt$.var)) {
      stop(
        paste0(
          '`var` must be compatible with `type` = "bin". ',
          "`var` is not a logical vector, or a numeric vector with zero and ",
          "one values."
        )
        , call. = FALSE
      )
    } else if (type == "cont" && !is.numeric(dt$.var)) {
      stop(
        paste0(
          '`var` is not compatible with `type` = "cont". ',
          "`var` is not a numeric vector."
        )
        , call. = FALSE
      )
    }
  }

  # If type is not explicitly given, deduce from input
  if (is.null(type)) {
    type <- fcase(
      .is_binary(dt$.var), "bin",
      is.numeric(dt$.var), "cont",
      default = "cat"
    )
  }

  # Restructure data depending on variable type
  if (type %in% c("bin", "cat")) {
    dt[
        , `:=`(.var_level = as.character(.var), .var = NULL)
      ][
        , .var := 1L
      ]
  } else if (type == "cont") {
    dt <- rbindlist(list(
      dt[!is.na(.var)][, .var_level := ""],
      dt[
          is.na(.var)
        ][
          , `:=`(.var_level = NA_character_, .var = NULL)
        ][
          , .var := 1L
        ]
      ),
      use.names = TRUE
    )
  }


   ### Summarize variable ###

  # Summarize overall
  dt_summary <- dt[
    , .(
      .n_var_level = .wsum(x = .weight, na.rm = TRUE),
      .sum = .wsum(.var, .weight, na.rm = TRUE),
      .stddev = .wsd(.var, .weight, na.rm = TRUE),
      .p25 = .wquantile(.var, .weight, prob = 0.25, na.rm = TRUE),
      .p50 = .wquantile(.var, .weight, prob = 0.5, na.rm = TRUE),
      .p75 = .wquantile(.var, .weight, prob = 0.75, na.rm = TRUE)
    ),
    by = ".var_level"
  ]
  dt_summary$.by <- by_total_val

  n_by_level <- dt_summary[
    , .(.n_by_level = sum(.n_var_level, na.rm = TRUE)), by = ".by"
      ]
  dt_summary <- n_by_level[dt_summary, on = ".by"]

  # Summarize in by-strata
  if (!is.null(by)) {
    dt_summary_by <- dt[
      , .(
        .n_var_level = .wsum(x = .weight, na.rm = TRUE),
        .sum = .wsum(.var, .weight, na.rm = TRUE),
        .stddev = .wsd(.var, .weight, na.rm = TRUE),
        .p25 = .wquantile(.var, .weight, prob = 0.25, na.rm = TRUE),
        .p50 = .wquantile(.var, .weight, prob = 0.5, na.rm = TRUE),
        .p75 = .wquantile(.var, .weight, prob = 0.75, na.rm = TRUE)
      ),
      by = c(".by", ".var_level")
    ]

    n_by_level <- dt_summary_by[
      , .(.n_by_level = sum(.n_var_level, na.rm = TRUE)), by = ".by"
    ]
    dt_summary_by <- n_by_level[dt_summary_by, on = ".by"]
    dt_summary <- rbindlist(list(dt_summary, dt_summary_by), use.names = TRUE)
  }


  ### Finalize output ###

  dt_summary$.var_name <- var
  dt_summary$.var_type <- type
  col_order <- c(
    ".var_name", ".var_type", ".by", ".n_by_level", ".var_level", ".n_var_level"
  )
  setcolorder(dt_summary, col_order)
  dt_summary[order(.by, .var_level)]
}


