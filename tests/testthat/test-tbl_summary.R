test_that("tbl_summary works", {
  x <- data.frame(
    var_cont = 1:10,
    var_bin = c(rep(1, 5), rep(0, 5)),
    var_cat = c(rep(1, 5), rep(2, 5))
  )

  dt <- tbl_summary(
    x = x,
    var = c("var_cont", "var_bin", "var_cat"),
    type = c(var_cat = "cat")
  )
})
