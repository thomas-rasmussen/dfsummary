# Quiet R CMD check notes of the type "no visible binding for global variable..."
utils::globalVariables(c(
  ".", ".var", ".weight", ".by", ".var_level", ".var_name", ".n_var_level",
  ".var_level_order", ".var_order"
))


#' Summarize variables in a data.frame
#'
#' Returns a descriptive summary of variables in a data.frame.
#'
#' @param vars Character vector. Names of variables in `x` to summarize.
#'   Variables types can be specified by naming elements of `var`. Variable
#'   types are: "bin" (binary), "cont" (continuous), "cat" (categorical).
#'   If no variable type is specified, the type is automatically deduced from
#'   the data.
#' @inheritParams .summarize_var
#'
#' @return A data.table containing a descriptive summary of `vars`. The
#'   data.table contains the following variables:
#'   Group information:
#'   - ".var_name": Variable name.
#'   - ".var_type": Variable type.
#'   - ".by": by-group value.
#'   Summary statistics:
#'   - ".n_by_level": Number of observations in ".by" group.
#'   - ".var_level": Variable level. Missing for continuous variables.
#'   - ".n_var_level": Number of observations in ".var_level" group.
#'   - ".sum": Sum of variable in group. For binary and categorical variables
#'     .n_var_level = .sum.
#'   - .stddev: Standard deviation in group. NA for binary and categorical
#'     variables.
#'   - ".p25": 25th percentile.
#'   - ".p50": 50th percentile / median.
#'   - ".p75": 75th percentile.
#' @import data.table
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   x <- data.frame(var1 = 1:10, var2 = c(rep("A", 5), rep("B", 5)))
#'   df_summarize(
#'    x,
#'    vars = c(cont = "var1", "var2")
#'   )
#' }
df_summarize <- function(x,
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

    tmp[, .var_order := i]

    if (i == 1L) {
      out <- tmp
    } else {
      out <- data.table::rbindlist(list(out, tmp))
    }
  }

  data.table::setcolorder(out, ".by")
  out[order(.by, .var_order)][, .var_order := NULL]
}


#### Helpers ####

#' Check if variable exists in data.frame.
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

#' Summarize a variable
#'
#' Creates a descriptive summary of a variable in a data.frame.
#'
#' If `type` is NULL, the variable type of `var` will be deduced from the data:
#' 1) If the variable takes "binary" values, ie is a logical vector, or a numeric
#'    vector with zero and one's, then `type` is set to "bin".
#' 2) Else, if the variable is numeric then `type` is set to "cont".
#' 3) Else, `type` is set to "cat".
#'
#' Factors are always defined to be categorical, using the labels of the factor
#' as the category names. Any unused factor level is also included in the
#' descriptive summary.
#'
#' @param x Input data.frame.
#' @param var String. Name of variable in `x` to summarize.
#' @param type String. Variable type of `var`: "bin" (binary),
#'   "cont" (continuous), "cat" (categorical). Default is NULL, meaning that
#'   the type is automatically deduced from the data. See details for more
#'   information.
#' @param weight String. Name of variable in `x` that contains
#'   observation weights. Must be a numeric variable, with non-negative
#'   real values. By default (`weight` = NULL), all weights are set to one.
#' @param by String. Name of variable to group by while summarizing. Default is
#'   NULL, ie no grouping variable is used. Note that if no grouping variable is
#'   used, the returned data.table will still contain a ".by" variable, taking
#'   the value given in `by_total_val`
#' @param by_total_val String. Name given to overall by-group.
#'
#' @return A data.table containing a descriptive summary of `var`. The
#'   data.table contains the following variables:
#'   Group information:
#'   - ".var_name": Variable name.
#'   - ".var_type": Variable type.
#'   - ".by": by-group value.
#'   Summary statistics:
#'   - ".n_by_level": Number of observations in ".by" group.
#'   - ".var_level": Variable level. Only used for categorical variables.
#'   - ".n_var_level": Number of observations in ".var_level" group.
#'   - ".sum": Sum of variable in group. For binary and categorical variables
#'     .n_var_level = .sum.
#'   - .stddev: Standard deviation in group. NA for binary and categorical
#'     variables.
#'   - ".p25": 25th percentile.
#'   - ".p50": 50th percentile / median.
#'   - ".p75": 75th percentile.
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

  # Record if variable is a factor and save level ordering
  var_is_factor <- is.factor(x$var)
  var_level_order <- levels(x$var)

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

  # For binary variables we only keep the line corresponding to the
  # "1"/"TRUE" category.
  if (type == "bin") {
    dt_summary <- dt_summary[.var_level %in% c("1", "TRUE")]
    dt_summary$.var_level <- ""
  }


  # For factors we add any unused levels in each .by strata, and then reorder
  # the categories according to the original levels attribute of `var`.
  if (var_is_factor) {
    tmp <- data.table::copy(dt_summary)
    by_strata <- unique(tmp$.by)
    for (i in seq_along(by_strata)) {
      i_by <- by_strata[i]
      i_tmp <- tmp[.by == i_by]
      for (j in seq_along(var_level_order)) {
        j_level <- var_level_order[j]
        if (!j_level %in% i_tmp$.var_level) {
          add_rows <- unique(i_tmp[, c(".var_name", ".var_type", ".by")])
          add_rows[, `:=`(
            .n_by_level = 0L,
            .var_level = j_level,
            .n_var_level = 0L,
            .sum = NA_integer_,
            .stddev = NA_real_,
            .p25 = NA_real_,
            .p50 = NA_real_,
            .p75 = NA_real_
          )]
          i_tmp <- rbindlist(list(i_tmp, add_rows))
        }
      }
      # Rebuild dt_summary strata by strata
      if (i == 1L) {
        dt_summary <- i_tmp
      } else {
        dt_summary <- rbindlist(list(dt_summary, i_tmp))
      }
    }

    # Reorder category levels in table
    dt_summary <- dt_summary[
      , .var_level_order := match(.var_level, var_level_order)
    ][
      order(.by, .var_level_order)
    ][
      , .var_level_order := NULL
    ]
    return(dt_summary)
  } else {
    return(dt_summary[order(.by, .var_level)][])
  }

}


