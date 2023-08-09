#### summarize_df ####

test_that("summary_df() signals error if input is invalid", {
  x <- data.frame(
    var_cont = 1:4,
    var_bin = c(rep(1, 2), rep(0, 2)),
    var_cat = c(rep(1, 2), rep(2, 2))
  )

  expect_error(
    summarize_df(
      x = x,
      vars = c("var_cont", "var_bin", cat = "var_cat")
    ),
    NA
  )

  expect_error(
    summarize_df(
      x = x,
      vars = c(bin = "var_cont", "var_bin", "var_cat")
    )
  )
})

test_that(
  "how does var = c(var, var, cat = var) works? Can it cause errors?", {}
  )

#### .is_var_name ####

test_that(".is_var_name() returns NULL if input is variable name in data.frame", {
  x <- data.frame(var1 = 1:2)
  expect_null(.is_var_name(x, "var1"))
})

test_that(".is_var_name() signals error if input is not valid", {
  x <- data.frame(var1 = 1:2)
  expect_error(.is_var_name(NULL, "var1"))
  expect_error(.is_var_name(data.frame(), "var1"))
  expect_error(.is_var_name(x, c("var1", "var2")))
  expect_error(.is_var_name(x, 1))
})


#### .summarize_var ####

test_that(".summarize_var() returns a data.table", {
  x <- data.frame(var = 1:2)
  dt <- .summarize_var(x, var = "var")
  expect_true("data.table" %in% class(dt))
})

test_that(".summarize_var() returns an empty data.table if input has zero rows", {
  x <- data.table(var1 = numeric(0))
  expect_error(dt <- .summarize_var(x, "var1"), NA)
  expect_true("data.table" %in% class(dt))
  expect_true(nrow(dt) == 0L)
})

test_that(".summarize_var signals error when basic input is invalid", {
  x <- data.frame(var = 1:2)
  expect_error(.summarize_var(1:2, "var"))
  expect_error(.summarize_var(x, "var1"))
})

test_that(".summarize_var() treats factors correctly", {
  x <- data.frame(
    var = factor(
      c("A", "B"),
      levels = c("A", "B"),
      labels = c("Level A", "Level B")
    )
  )
  expect_error(dt <- .summarize_var(x, "var"), NA)
  expect_true(all(dt$.var_level == c("Level A", "Level B")))
  expect_error(.summarize_var(x, "var", "cont"))
  expect_error(.summarize_var(x, "var", "bin"))
})

test_that(".summarize_var() treats NA's correctly", {
  x <- data.frame(
    var_cont = c(NA, 1, 2),
    var_cat = c(NA, 0, 1)
  )
  expect_error(dt <- .summarize_var(x, "var_cont"), NA)
  expect_true(identical(dt$.var_level, c("", NA_character_)))
  expect_error(dt <- .summarize_var(x, "var_cat", "cat"), NA)
  expect_true(identical(dt$.var_level, c("0", "1", NA_character_)))
})

test_that(".summarize_var() handles weighted observations correctly", {
  x <- data.frame(
    var = c("a", "b"),
    weight = c(0.5, 2)
  )
  expect_error(dt <- .summarize_var(x, "var", weight = "weight"), NA)
  expect_true(identical(dt$.n_var_level, c(0.5, 2)))
})

test_that(".summarize_var() `type` argument works as intended", {
  x <- data.frame(
    var_cont = 1:2, var_cat = 1:2, var_bin = 0:1
  )
  expect_error(.summarize_var(x, "var_cont", type = "cont"), NA)
  expect_error(.summarize_var(x, "var_cat", type = "cat"), NA)
  expect_error(.summarize_var(x, "var_bin", type = "bin"), NA)
})

test_that(".summarize_var() `by` argument works as intended", {
  x <- data.frame(
    var = rep(c(0, 1), each = 4),
    by = rep(c(0, 1), times = 4)
  )

  expect_error(dt <- .summarize_var(x, "var", by = "by"), NA)
})
