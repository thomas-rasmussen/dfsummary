#### df_mask ####

test_that("data from vignette that is curently failing", {
  set.seed(1)
  n <- 20
  pop <-data.frame(
    exposure = sample(c("no", "yes"), n, replace = TRUE),
    cat_var = sample(c("A", "B", "C"), n, replace = TRUE),
    bin_var = sample(c(0, 1), n, replace = TRUE),
    cont_var = rnorm(n)
  )

  # Add "n" variable to data
  pop1 <- data.table::as.data.table(pop)[, .n := 1L]

  # It is combination of .n and cat_var that is for some reason making the
  # algorithm fail
  tbl1 <- df_summarize(
    x = pop1,
    vars = c(".n", "cat_var"),
    by = "exposure"
  )
  tbl2 <- df_mask(tbl1)
  expect_true(
    identical(tbl2$.n_by_level_mask_flag, rep(FALSE, nrow(tbl2))) &
    identical(
      tbl2$.n_var_level_mask_flag,
      c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE)
    )
  )

  # For some reason the
  # if (identical(new_mask_flag, n_var_level_mask_flags_ij)) {
  #   cnt_no_change <- cnt_no_change + 1L
  # }
  # part of the code comparing x2, and x2_prev_ite (before the above change)
  # found differences in the objects. Weird or some obvious blunder? Fixed
  # by making the above change, but probably something that should be investigated
  # further. In any case, the code in df_mask would benefit from some further
  # refactoring





})

test_that("df_mask() primary supression on variable level works", {
  x <- data.frame(
    var = c("A", "B", "B", rep("C", 5L))
  )
  tbl1 <- df_summarize(x, var = "var")
  tbl2 <- df_mask(tbl1)
  expect_true(
    identical(tbl2$.n_by_level_mask_flag, c(FALSE, FALSE, FALSE)) &
    identical(tbl2$.n_var_level_mask_flag, c(TRUE, TRUE, FALSE))
  )
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
    identical(tbl2$.n_by_level_mask_flag, rep(FALSE, nrow(tbl1))) &
    identical(
      tbl2$.n_var_level_mask_flag,
      c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)
    )
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
    identical(tbl2$.n_by_level_mask_flag, rep(FALSE, nrow(tbl2))) &
    identical(tbl2$.n_var_level_mask_flag, rep(TRUE, nrow(tbl2)))
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
    identical(tbl2$.n_var_level_mask_flag, rep(TRUE, nrow(tbl2))) &
    identical(
      tbl2$.n_by_level_mask_flag,
      c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
    )
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
    identical(tbl2$.n_var_level_mask_flag, rep(TRUE, nrow(tbl2))) &
    identical(tbl2$.n_by_level_mask_flag, rep(TRUE, nrow(tbl2)))
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
