test_that("tbl_summary works", {
  x <- data.frame(
    var_cont = 1:10,
    var_bin = c(rep(1, 5), rep(0, 5)),
    var_cat = c(rep(1, 5), rep(2, 5))
  )

  dt <- tbl_summary(
    x = x,
    vars = c("var_cont", "var_bin", cat = "var_cat"),
  )

  x <- data.frame(
    var = rep(c(0,1), times = 50),
    by = c(rep("A", 5), rep("B", 5), rep("C", 90))
  )
  dt <- tbl_summary(
    x = x,
    vars = c("var"),
    by = "by"
  )
})
