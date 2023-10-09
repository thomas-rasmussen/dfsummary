# Quiet R CMD check notes of the type "no visible binding for global variable..."
utils::globalVariables(c(
  ".n_by_level", ".p25", ".p50", ".p75", ".stat_label", ".stat_value",
  ".sum", ".var_type", ".n", ".stat_num1", ".stat_num2", ".stat_num3"
))

#' Summarize variables
#'
#' @param x a
#' @param vars a
#' @param weight a
#' @param by a
#' @param by_total_val a
#' @param mask A logical value. Mask person-sensitive aggregated counts?
#' @param add_num_stat A logical value. Add numeric columns with statistics?
#'
#' @return data.table
#' @export
#'
#' @examples
#' \dontrun{
#'   TODO
#' }
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

