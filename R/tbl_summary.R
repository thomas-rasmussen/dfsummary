# Quiet R CMD check notes of the type "no visible binding for global variable..."
utils::globalVariables(c(
  ".n_by_level", ".p25", ".p50", ".p75", ".stat_label", ".stat_value",
  ".sum", ".var_type", ".n"
))

#' Summarize variables
#'
#' @param x a
#' @param vars a
#' @param weight a
#' @param by a
#' @param by_total_val a
#' @param mask A logical value. Mask person-sensitive aggregated counts?
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
                        mask = FALSE
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
  # TODO: this should be handled automatically instead. Mybe add ".n" variable type
  # or is there a better way? That type would be exposed to users...
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

  dt <- dt[
    , .(.by, .var_name, .var_type, .n_by_level, .var_level,
        .stat_label, .stat_value, .mask_by_level, .mask_var_level)
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


  ### Restructure ###

  dt_wide <- dcast(
    dt,
    .var_name + .var_type + .stat_label + .var_level ~ .by,
    value.var = ".stat_value"
  )


  dt_wide
}

