#### .is_binary ####

test_that(".is_binary() returns TRUE if input is binary vector", {
  expect_true(.is_binary(0L))
  expect_true(.is_binary(1))
  expect_true(.is_binary(FALSE))
  expect_true(.is_binary(c(0L, 1L)))
  expect_true(.is_binary(c(1, 0.0)))
  expect_true(.is_binary(c(TRUE, FALSE)))
})

test_that(".is_binary() returns FALSE if input is not binary vector", {
  expect_false(.is_binary(0.1))
  expect_false(.is_binary(c(0, 0.1)))
  expect_false(.is_binary(as.factor(1)))
  expect_false(.is_binary(list()))
  expect_false(.is_binary(NULL))
})

test_that(".is_binary() handles NA values correctly", {
  expect_false(.is_binary(NA))
  expect_false(.is_binary(c(1, NA)))
  expect_true(.is_binary(NA, allow_na = TRUE))
  expect_true(.is_binary(c(1, NA), allow_na = TRUE))
})

test_that(".is_binary() signals an error if invalid input is given", {
  expect_error(.is_binary())
  expect_error(.is_binary(1, tol = TRUE))
  expect_error(.is_binary(1, tol = c(1, 1)))
  expect_error(.is_binary(1, allow_na = 1))
  expect_error(.is_binary(1, allow_na = c(TRUE, FALSE)))
})

test_that(".is_binary() `tol` argument works as intended", {
  expect_false(.is_binary(1.1, tol = 0.1))
  expect_true(.is_binary(1.1, tol = 0.2))
})


#### .winputcheck ####

test_that(".winputcheck() returns NULL if inputs is valid", {
  expect_null(.winputcheck(x = 1:2, w = 1:2, na.rm = FALSE))
  expect_null(.winputcheck(x = 1:2, w = c(1L, 1L), na.rm = FALSE))
  expect_null(.winputcheck(x = c(1, NA), w = c(1L, 1L), na.rm = TRUE))
})

test_that(".winputcheck() throws an error if input is invalid", {
  expect_error(.winputcheck(x = 1:2, w = 1, na.rm = TRUE))
  expect_error(.winputcheck(x = 1:2, w = c(1L, 1L), na.rm = 1))
  expect_error(.winputcheck(x = as.factor(1:2), w = c(1L, 1L), na.rm = FALSE))
  expect_error(.winputcheck(x = 1:2, w = as.factor(1:2), na.rm = FALSE))
  expect_error(.winputcheck(x = 1:2, w = c(NA, 1), na.rm = FALSE))
  expect_error(.winputcheck(x = 1:2, w = c(-1, 1), na.rm = FALSE))
})


#### .wmean ####

test_that("that mean() is a special case of .wmean() with no/unit weights", {
  x <- 1:10
  expect_equal(.wmean(x), mean(x))
})

test_that(".wmean() returns correct results", {
  x <- 1:5
  w <- rep(2, 5)
  expect_equal(.wmean(x), 3)
  expect_equal(.wmean(x, w), 3)
  w<- rep(0.5, 5)
  expect_equal(.wmean(x, w), 3)
})

test_that(".wmean() handles NA values correctly", {
  expect_equal(.wmean(x = c(NA, 1)),  NA_real_)
  expect_equal(.wmean(x = c(NA, 1), na.rm = TRUE),  1)
  expect_equal(.wmean(x = c(NA, 1, 2), w = c(2, 1, 1), na.rm = TRUE),  1.5)
})


#### .wsd ####

test_that(".wsd() gives the same results as sd() when weights are not specified", {
  x <- 1:5
  expect_equal(.wsd(x), sd(x))
})

test_that(".wsd() gives correct results", {
  # Hard to come up with good examples testing the weighted formula.
  # In the special case where we assume that wsd = 1, M = N, w[i] = k,
  # x_mean = 0, and i is even,
  # we get that x[i] = (-1)**i*sqrt((n-1)/n) (which does not depend on k!)
  # solves this.
  k <- 2
  n <- 10
  x <- numeric(n)
  for (i in 1:n) {
    x[i] <- (-1)**i*sqrt((n-1)/n)
  }
  expect_equal(.wsd(x, w = rep(k, n)), 1)
})

test_that(".wsd() handles NA's correctly", {
  x <- c(NA, 0, 1, 2)
  expect_equal(.wsd(x), NA_real_)
  expect_equal(.wsd(x, na.rm = TRUE), 1)
})

test_that(".wsd() returns NA if input vector is of length one or zero", {
  expect_equal(.wsd(numeric(0)), NA_real_)
  expect_equal(.wsd(1), NA_real_)
})


#### .wquantile ####

test_that("using unit weights results in intuitive results", {
  expect_equal(.wquantile(x = c(1, 3, 2), prob = 0.5), 2)
  expect_equal(.wquantile(x = c(1, 2, 2, 3), prob = 0.5), 2)
  expect_equal(.wquantile(x = c(1, 2, 4, 5), prob = 0.5), 3)
})

test_that("using non-unit weights results in intuitive results", {
  expect_equal(.wquantile(x = c(1, 2, 3), w = c(2, 2, 2), prob = 0.5), 2)
  expect_equal(.wquantile(x = c(1, 2, 3), w = c(0.5, 0.5, 0.5), prob = 0.5), 2)
})

test_that("NA's are handled as expected", {
  expect_equal(.wquantile(x =c(1, NA_real_, 2,  3), prob = 0.5), NA_real_)
  expect_equal(.wquantile(x =c(1, NA_real_, 2,  3), prob = 0.5, na.rm = TRUE), 2)
})

test_that("`prob` argument works when a vector is provided", {
  q <- .wquantile(x = 1:9, prob = c(1/3, 0.5, 2/3))
  expect_equal(round(q, 1), c(3.5, 5, 6.5))
  expect_equal(.wquantile(x = c(1:10, NA_real_), prob = c(0.4, 0.6)), c(NA_real_, NA_real_))
})



#### .wsum ####

test_that("sum() is a special case of .wsum() with unit/no weights", {
  x <- 1:3
  expect_equal(.wsum(x), sum(x))
  expect_equal(.wsum(x, rep(1L, length(x))), sum(x))
})

test_that(".wsum() returns correct results", {
  x <- 1:3
  n <- length(x)
  expect_equal(.wsum(x, rep(1L, n)), 6)
  expect_equal(.wsum(x, rep(1.5, n)), 9)
  expect_equal(.wsum(x, rep(2L, n)), 12)
})

test_that(".wsum() handles NA/NaN values as intended", {
  expect_equal(.wsum(x = NA), NA_real_)
  expect_equal(.wsum(x = c(1, NA)), NA_real_)
  expect_equal(.wsum(x = NA, na.rm = TRUE), 0)
  expect_equal(.wsum(x = c(1, NA), na.rm = TRUE), 1)
  expect_equal(.wsum(x = c(NA, 1), w = c(1, 2), na.rm = TRUE), 2)
  expect_equal(.wsum(x = c(1, NaN)), NaN)
})


#### .validate_tblsummary ####

test_that(".validate_tblsummary() throws error if object is nost a tblsummary", {
  expect_error(.validate_tblsummary(data.frame()))
})

test_that(".validate_tblsummary() does not throw error if object is a tblsummary", {
  x <- data.frame(var = 1:10)
  tbl <- .summarize_var(x, var = "var")
  expect_error(.validate_tblsummary(tbl), NA)
})
