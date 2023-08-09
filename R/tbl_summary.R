# Quiet R CMD check notes of the type "no visible binding for global variable..."
utils::globalVariables(c(
  ".n_by_level", ".p25", ".p50", ".p75", ".stat_label", ".stat_value",
  ".sum", ".var_type"
))

#' Summarize variables
#'
#' @param x a
#' @param vars a
#' @param weight a
#' @param by a
#' @param by_total_val a
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
                        by_total_val = ".all"
                        ) {

  ### Input checks ###

  # TODO: need to add something here

  ### Summarize variables
  dt <- summarize_df(
    x,
    vars = vars,
    weight = weight,
    by = by,
    by_total_val = by_total_val
  )

  ### Add masking indicators ###
  dt <- mask_tbl(dt)

  ### Calculate stats ###

  # TODO: should be put in utility function?

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

  dt <- dt[
    , .(.by, .var_name, .var_type, .n_by_level, .var_level,
        .stat_label, .stat_value, .mask)
  ]

  ### Mask person-sensitive statistics ###
  dt[, .stat_value := fifelse(.mask == 1L, "*", .stat_value)][, .mask := NULL]


  ### Restructure ###

  dt_wide <- dcast(
    dt,
    .var_name + .var_type + .stat_label + .var_level ~ .by,
    value.var = ".stat_value"
  )


  dt_wide
}

