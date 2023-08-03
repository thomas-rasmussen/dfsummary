# Quiet R CMD check notes of the type "no visible binding for global variable..."
utils::globalVariables(c(
  ".", ".var", ".weight", ".by", ".var_level", ".var_name", ".n_var_level"
))


#' Summarize variables in a data.frame
#'
#' Returns a descriptive summary of variables in a data.frame.
#' @param vars Character vector. Names of variables in `x` to summarize.
#' @param types Optional named character vector. Variable types of variabls in
#'   `vars`: "bin" (binary), "cont" (continuous), "cat" (categorical). Default
#'   is NULL, meaning that the types of `vars` is automatically deduced from
#'   the data. Can be specified for only some variables, implicitly setting the
#'   type for the remaining variables to NULL.
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
#'   vars = c("var1", "var2"),
#'   types = c(var1 = "cont", var2 = "cat")
#' )
summarize_df <- function(x,
                         vars,
                         types = NULL,
                         weight = NULL,
                         by = NULL,
                         by_total_val = ".all") {

  ### Input checks ###

  if (!is.data.frame(x)) {
    stop("'x' is not a data.frame", call. = FALSE)
  }
  if (!is.character(vars)) {
    stop("'vars' is not a character vector", call. = FALSE)
  }


  ### Summarize data ###

  for (i in seq_along(vars)) {
    i_var <- vars[i]
    if (i_var %in% names(types)) {
      i_type <- types[i_var]
    } else {
      i_type <- NULL
    }
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

#' Summarize variable
#'
#' Returns a descriptive summary of a variable in a data.frame.
#'
#'
#' @param x Input data.frame.
#' @param var String. Name of variable in `x` to summarize.
#' @param type String. Variable type of `var`: "bin" (binary),
#'   "cont" (continuous), "cat" (categorical). Default is NULL, meaning that
#'   the type of `var` is automatically deduced from the data.
#' @param weight String. Optional name of variable in `x` that contains
#'   observation weights. The weights must be non-negative numbers.
#' @param by String. Optional name of variable to summarize by.
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

  # 'x'
  if (!is.data.frame(x)) {
    stop("'x' is not a data.frame", call. = FALSE)
  }
  # 'var'
  if (!(is.character(var) && length(var) == 1L)) {
    stop("'var' is not a string", call. = FALSE)
  }
  if (!var %in% names(x)) {
    stop(paste0("'var' is not a variable in 'x'", call. = FALSE))
  }
  # 'type'
  if (!is.null(type) && !(is.character(type) && length(type) == 1L)) {
    stop("'type' is not a string", call. = FALSE)
  }
  if (!is.null(type) && !type %in% c("bin", "cat", "cont")) {
    stop("'type' does not have a valid value", call. = FALSE)
  }
  # 'weight'
  if (!is.null(weight) && !(is.character(weight) && length(weight) == 1L)) {
    stop("'weight' is not a string", call. = FALSE)
  }
  if (!is.null(weight) && !weight %in% names(x)) {
    stop(paste0("'weight' is not a variable in 'x'", call. = FALSE))
  }
  # 'by'
  if (!is.null(by) && !(is.character(by) && length(by) == 1L)) {
    stop("'by' is not a string", call. = FALSE)
  }
  if(!is.null(by) && !by %in% names(x)) {
    stop(paste0("'", by, "' is not a variable in 'x'"), call. = FALSE)
  }
  # 'by_total_val'
  if (!(is.character(by_total_val) && length(by_total_val) == 1L)) {
    stop("'by_total_val' is not a string", call. = FALSE)
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
      stop("'var' is not compatible with 'type' == bin", call. = FALSE)
    } else if (type == "cont" & !(is.numeric(dt$.var) & !is.factor(dt$.var))) {
      stop("'var' is not compatible with 'type' == cont", call. = FALSE)
    }
  }

  # If type is not explicitly given, deduce from input
  if (is.null(type)) {
    type <- fcase(
      .is_binary(dt$.var), "bin",
      is.numeric(dt$.var) & !is.factor(dt$.var), "cont",
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


