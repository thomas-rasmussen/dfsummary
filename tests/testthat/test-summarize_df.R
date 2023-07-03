#### summarize_df ####

test_that("summary_df works", {
  x <- data.frame(
    var_cont = 1:10,
    var_bin = c(rep(1, 5), rep(0, 5)),
    var_cat = c(rep(1, 5), rep(2, 5))
  )

  dt <- summarize_df(
    x = x,
    vars = c("var_cont", "var_bin", "var_cat"),
    types = c(var_cat = "cat")
  )

  expect_error(
    summarize_df(
      x = x,
      vars = c("var_cont", "var_bin", "var_cat"),
      type = c(var_cat = "cat")
    ),
    NA
  )
})

#### .summarize_var ####

test_that(".summary_var returns a data.table", {
  x <- data.frame(var = 1:10)
  dt <- .summarize_var(x, var = "var")
  expect_true("data.table" %in% class(dt))
})

test_that(".summary_var throws error when input is invalid", {
})

test_that(".summary_var summarizes variable correctly", {
    x <- data.frame(var = 1:10, by = c(rep(1, 5), rep(2, 5)))
    dt <- .summarize_var(x, var = "var", by = "by", type = "cont")
    expect_error(.summarize_var(x, var = "var", by = "by", type = "cont"), NA)

    x <- data.frame(
      var = rep(c(0, 1), 5),
      by = rep(c(0, 1), each = 5)
    )
    dt <- .summarize_var(x, var = "var", by = "by", type = "bin")
})

