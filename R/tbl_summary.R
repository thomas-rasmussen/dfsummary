#' Summarize variables
#'
#' @param x a
#' @param vars a
#' @param types a
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
                        types = NULL,
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
    types = types,
    weight = weight,
    by = by,
    by_total_val = by_total_val
  )

  ### Add masking indicators ###

  ### Calculate stats ###

  # dt <- dt[
  #   , `:=`(stat_label = fcase(
  #       .var_type == "bin", paste0("n_pct"),
  #       .var_type == "cat", paste0("n_pct"),
  #       .var_type == "cont", paste0("median_p25_p75"),
  #       .var_type == "n", "n"
  #     ),
  #     stat_value = fcase(
  #       .var_type == "bin" | .var_type == "cat", paste0(
  #         formatC(.sum, digits = 0, format = "f", big.mark = ","),
  #         " (",
  #         formatC(100 * .sum / .n_by_level, digits = 1, format = "f"),
  #         "%)"
  #       ),
  #       .var_type == "cont", paste0(
  #         formatC(.p50, digits = 1, format = "f", big.mark = ","),
  #         " (",
  #         formatC(.p25, digits = 1, format = "f", big.mark = ","),
  #         ";",
  #         formatC(.p75, digits = 1, format = "f", big.mark = ","),
  #         ")"
  #       ),
  #       .var_type == "n", paste0(
  #         formatC(.sum, digits = 0, format = "f", big.mark = ",")
  #       )
  #     )
  #   )
  # ]

  dt
}

