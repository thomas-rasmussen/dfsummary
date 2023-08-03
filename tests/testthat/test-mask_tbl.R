test_that("mask_tbl primary masking works", {
    x <- data.frame(
    var = c("A", "B", "B", rep("C", 5L))
  )
  # x <- data.frame(
  #   var = rep(c(0,1), times = 50),
  #   by = c(rep("A", 5), rep("B", 5), rep("C", 90))
  # )
  tbl1 <- summarize_df(x, var = "var")
  tbl2 <- mask_tbl(tbl1)

  expect_true(all((0 < tbl2$.n_var_level & tbl2$.n_var_level < 5) == tbl2$.mask))
})
