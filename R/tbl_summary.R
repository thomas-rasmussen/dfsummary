# Quiet R CMD check notes of the type "no visible binding for global variable..."
utils::globalVariables(c(
  ".n_by_level", ".p25", ".p50", ".p75", ".stat_label", ".stat_value",
  ".sum", ".var_type", ".n", ".stat_num1", ".stat_num2", ".stat_num3"
))

#' Make descriptive summary
#'
#' Makes a descriptive summary of the distribution of variables in a given
#' data.frame.
#'
#' If the type of a variable in `vars` is not specified, the type is deduced
#' from the data using the following algorithm:
#' 1) If the variable takes "binary" values, ie is a logical vector, or a
#' numeric vector with zero and one's, then the variable is assumed to be
#' a binary variable.
#' 2) Else, if the variable is numeric then it is assumed to be a
#' continuous variable.
#' 3) Else, the variable is assumed to be a categorical variable.

#' @param x A data.frame. Data to summarize.
#' @param vars Character vector. Names of variables in `x` to summarize.
#'   Variables types can be specified by naming elements of `var`. Variable
#'   types are: "bin" (binary), "cont" (continuous), "cat" (categorical).
#'   If no variable type is specified, the type is automatically deduced from
#'   the data. See details for more information.
#' @param weight String. Name of variable in `x` that contains
#'   observation weights. Must be a numeric variable, with non-negative
#'   real values. By default (`weight` = NULL), all weights are set to one.
#' @param by String. Name of variable to group by while summarizing. Default is
#'   NULL, ie no grouping variable is used. Note that if no grouping variable is
#'   used, the returned data.table will still contain a ".by" variable, taking
#'   the value given in `by_total_val`.
#' @param by_total_val String. Name given to overall by-group.
#' @param mask A logical value. Mask person-sensitive aggregated counts?
#' @param add_num_stat A logical value. Add numeric columns with statistics?
#'
#' @return A data.table containing a descriptive summary of `vars`. The
#'   data.table contains the following variables:
#'   - ".by": by-group value.
#'   - ".var_name": Variable name.
#'   - ".var_type": Variable type.
#'   - ".var_level": Variable level. Only used for categorical variables.
#'   - ".stat_label": Short-name of calculated statistics.
#'   - ".stat_value": Statistics value.
#'
#' @export
#'
#' @examples
#' x <- data.frame(
#'   var_cont = 1:10,
#'   var_bin = c(rep(1, 5), rep(0, 5)),
#'   var_cat = c(rep(1, 5), rep(2, 5))
#' )
#'
#' dt <- tbl_summary(
#'   x = x,
#'   vars = c("var_cont", "var_bin", cat = "var_cat"),
#' )
tbl_summary <- function(x,
                        vars,
                        weight = NULL,
                        by = NULL,
                        by_total_val = ".all",
                        mask = FALSE,
                        add_num_stat = FALSE
                        ) {

  ### Input checks ###

  # TODO: need to add something here

  ### Summarize variables

  # Add "n" variable to data
  x1 <- data.table::as.data.table(x)[, .n := 1L]
  vars <- c(".n", vars)

  dt <- summarize_df(
    x1,
    vars = vars,
    weight = weight,
    by = by,
    by_total_val = by_total_val
  )

  ### Add masking indicators ###
  if (mask) {
    dt <- mask_tbl(dt)
  } else {
    dt[, `:=`(.mask_var_level = 0L, .mask_by_level = 0L)]
  }

  ### Calculate stats ###

  # TODO: should be put in flexible utility function?

  dt <- dt[
    , `:=`(.stat_label = fcase(
        .var_type == "bin", paste0("n_pct"),
        .var_type == "cat", paste0("n_pct"),
        .var_type == "cont", paste0("median_p25_p75")
      ),
      .stat_value = fcase(
        .var_type == "bin" | .var_type == "cat", paste0(
          formatC(.sum, digits = 0, format = "f", big.mark = ","),
          " (",
          formatC(100 * .sum / .n_by_level, digits = 1, format = "f"),
          "%)"
        ),
        .var_type == "cont", paste0(
          formatC(.p50, digits = 1, format = "f", big.mark = ","),
          " (",
          formatC(.p25, digits = 1, format = "f", big.mark = ","),
          ";",
          formatC(.p75, digits = 1, format = "f", big.mark = ","),
          ")"
        )
      )
    )
  ]

  # Fix ".n" statistic
  dt <- dt[
    , `:=` (
      .var_type = fifelse(.var_name == ".n", "n", .var_type),
      .var_level = fifelse(.var_name == ".n", "", .var_level),
      .stat_label = fifelse(.var_name == ".n", "n", .stat_label ),
      .stat_value = fifelse(
        .var_name == ".n",
        unlist(strsplit(.stat_value, split = " "))[1:(nrow(dt)*2) %% 2 == 1L],
        .stat_value
      )
    )
  ]

  # Add numeric stat columns
  # TODO: Does not feel great to recalculate statistics here. Can this be
  # done in a more natural place?
  dt <- dt[
    , `:=`(
        .stat_num1 = fcase(
          .var_type == "n", as.numeric(.n_var_level),
          .var_type == "bin", as.numeric(.sum),
          .var_type == "cat", as.numeric(.sum),
          .var_type == "cont", as.numeric(.p50),
          default = NA_real_
        ),
        .stat_num2 = fcase(
          .var_type == "bin", as.numeric(100 * .sum / .n_by_level),
          .var_type == "cat", as.numeric(100 * .sum / .n_by_level),
          .var_type == "cont", as.numeric(.p25),
          default = NA_real_
        ),
        .stat_num3 = fcase(
          .var_type == "cont", as.numeric(.p75),
          default = NA_real_
        )
      )
  ]

  # Restrict to relevant columns
  dt <- dt[
    , .(.by, .var_name, .var_type, .n_by_level, .var_level,
        .stat_label, .stat_value, .mask_by_level, .mask_var_level,
        .stat_num1, .stat_num2, .stat_num3)
  ]

  ### Mask person-sensitive statistics ###
  dt[
    , .stat_value := fifelse(
        .mask_by_level == 1L | .mask_var_level == 1L,
        "*",
        .stat_value
      )
    ][
    , `:=`(.mask_by_level = NULL, .mask_var_level = NULL, .n_by_level = NULL)
  ]

  ### Remove numeric stat columns if not requested ###
  if(!add_num_stat) {
    dt[, `:=`(.stat_num1 = NULL, .stat_num2 = NULL, .stat_num3 = NULL)]
  }

  dt[]
}

