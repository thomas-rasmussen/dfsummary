#### df_mask ####

test_that("df_mask() primary supression on variable level works", {
    x <- data.frame(
    var = c("A", "B", "B", rep("C", 5L))
  )
  tbl1 <- df_summarize(x, var = "var")
  tbl2 <- df_mask(tbl1)

  expect_true(all((0 < tbl2$.n_var_level & tbl2$.n_var_level < 5) == tbl2$.mask_var_level))
})

test_that("df_mask() secondary supression on variable level works", {
  x_a <- data.frame(
    var = c("A", "B", "B", rep("C", 5L)),
    by = "A"
  )
  x_b <- data.frame(
    var = c(rep("A", 5L), rep("B", 5L), rep("C", 82L)),
    by = "B"
  )
  x <- rbind(x_a, x_b)
  tbl1 <- df_summarize(x, var = "var", by = "by")
  tbl2 <- df_mask(tbl1)

  # Table (formatted) before masking
  # group all   A   B
  # n     100   8  92
  # var
  #   A     6   1   5
  #   B     7   2   5
  #   C    87   5  82

  # Table (formatted) after masking
  # group all   A   B
  # n     100   8  92
  # var
  #   A     6   *   *
  #   B     7   *   *
  #   C    87   5  82
  expect_true(
    identical(tbl2$.mask_var_level, c(0L, 0L, 0L, 1L, 1L, 0L, 1L, 1L, 0L))
  )
})

test_that("df_mask() masking on by level works", {
  x_a <- data.frame(
    var = c("A", rep("B", 5L)),
    by = "A"
  )
  x_b <- data.frame(
    var = c(rep("A", 2L), rep("B", 5L)),
    by = "B"
  )
  x <- rbind(x_a, x_b)
  tbl1 <- df_summarize(x, var = "var", by = "by")
  tbl2 <- df_mask(tbl1)

  # Table (formatted) before masking
  # group all   A   B
  # n      13   3  10
  # var
  #   A     6   1   5
  #   B     7   2   5

  # Table (formatted) after masking
  # group all   A   B
  # n      13   *  *
  # var
  #   A     6   *   *
  #   B     7   *   *
  expect_true(
    identical(tbl2$.mask_var_level, c(rep(1L, 6)))
  )

  x_a <- data.frame(
    var = c("A", rep("B", 3L)),
    by = "A"
  )
  x_b <- data.frame(
    var = c(rep("A", 2L), rep("B", 5L)),
    by = "B"
  )
  x <- rbind(x_a, x_b)
  x <- df_summarize(x, var = "var", by = "by")
  tbl2 <- df_mask(x)
  # Table (formatted) before masking
  # group all   A   B
  # n      11   3   8
  # var
  #   A     4   1   3
  #   B     7   2   5

  # Table (formatted) after masking
  # group all   A   B
  # n      11   *  *
  # var
  #   A     *   *   *
  #   B     *   *   *
  expect_true(
    identical(tbl2$.mask_var_level, c(rep(1L, 6))) &
    identical(tbl2$.mask_by_level, c(0L, 0L, 1L, 1L, 1L, 1L))
  )

  ### All should be masked ###

  x <- data.frame(
      var = c(rep(c("A", "B"), times = 2)),
      by = rep(c("C", "D"), each = 2)
  )
  x <- df_summarize(x, var = "var", by = "by")
  tbl2 <- df_mask(x)
  # Table (formatted) before masking
  # group all   C   D
  # n      4   2   2
  # var
  #   A     2   1   1
  #   B     2   1   1

  # Table (formatted) after masking
  # group all   A   B
  # n      *   *  *
  # var
  #   A     *   *   *
  #   B     *   *   *
  expect_true(
    identical(tbl2$.mask_var_level, c(rep(1L, 6))) &
    identical(tbl2$.mask_by_level, c(rep(1L, 6)))
  )

})


#### .update_mask_flags ####

test_that(".update_mask_flags() throws error if input is invalid", {
  expect_error(.update_mask_flags(c(NA_real_, TRUE)))
  expect_error(.update_mask_flags(1, mask_flags = 1))
  expect_error(.update_mask_flags(1:2, mask_flags = c(TRUE, NA)))
  expect_error(.update_mask_flags(1:2, mask_flags = TRUE))
  expect_error(.update_mask_flags(1, protect = 1))
  expect_error(.update_mask_flags(1:2, protect = c(TRUE, NA)))
  expect_error(.update_mask_flags(1:2, protect = TRUE))
})

test_that(".update_mask_flags() primary masking works", {
  expect_equal(.update_mask_flags(c(1, 2, 5)), c(TRUE, TRUE, FALSE))
  # Input is partially flagged vector
  out <- .update_mask_flags(
    x = c(1, 2, 5),
    mask_flags = c(TRUE, FALSE, FALSE)
  )
  expect_equal(out, c(TRUE, TRUE, FALSE))
  expect_equal(.update_mask_flags(1, TRUE), TRUE)
})

test_that(".update_mask_flags() secondary masking works", {
  x <- c(1, 5, 5)
  expect_equal(.update_mask_flags(x), c(TRUE, TRUE, FALSE))
  x <- c(1, 1, 5, 5)
  expect_equal(.update_mask_flags(x), c(TRUE, TRUE, TRUE, FALSE))
})
