
#### .is_binary ####
test_that(".is_binary() returns TRUE if input is binary-ish", {
  expect_equal(.is_binary(0L), TRUE)
  expect_equal(.is_binary(1), TRUE)
  expect_equal(.is_binary(FALSE), TRUE)
  expect_equal(.is_binary(c(0L, 1L)), TRUE)
  expect_equal(.is_binary(c(1, 0.0)), TRUE)
  expect_equal(.is_binary(c(TRUE, FALSE)), TRUE)
})

test_that(".is_binary() returns FALSE if input is not binary-ish", {
  expect_equal(.is_binary(0.1), FALSE)
  expect_equal(.is_binary("0"), FALSE)
  expect_equal(.is_binary(c(0, 0.1)), FALSE)
  expect_equal(.is_binary(list(1)), FALSE)
})

test_that(".is_binary() correctly handles NA values", {
  expect_equal(.is_binary(NA), FALSE)
  expect_equal(.is_binary(c(1, NA)), FALSE)
  expect_equal(.is_binary(NA, allow_na = TRUE), TRUE)
  expect_equal(.is_binary(c(1, NA), allow_na = TRUE), TRUE)
  expect_equal(.is_binary(c(1, NA, 1.2), allow_na = TRUE), FALSE)
})

test_that(".is_binary(NULL) returns FALSE", {
  expect_equal(.is_binary(NULL), FALSE)
})

test_that(".is_binary() throws an error if invalid input is given", {
  expect_error(.is_binary(data.frame()))
  expect_error(.is_binary(1, tol = NULL))
  expect_error(.is_binary(1, allow_na = 1))
})

test_that(".is_binary() is not too inefficient", {
  skip("Performance tests. Only run manually")
  n_obs <- 1e7
  x <- sample(c(0,1), size = n_obs, replace = TRUE)
  .is_binary(x)
})

#### .w_inputcheck ####
test_that(".winputcheck() works as intended", {
  expect_equal(.w_inputcheck(x = 1:5, w = 1:5, na.rm = TRUE), NULL)
  expect_equal(.w_inputcheck(x = 1:5, w = NULL, na.rm = TRUE), NULL)
  expect_equal(.w_inputcheck(x = c(1, NA), w = NULL, na.rm = TRUE), NULL)
  expect_equal(.w_inputcheck(x = c(1, NA), w = c(NA, 1), na.rm = TRUE), NULL)
})

test_that(".winputcheck() throws an error if input is invalid", {
  expect_error(.w_inputcheck(1:5, 1, TRUE))
  expect_error(.w_inputcheck(1:5, NULL, 1))
})

#### .wmean ####
test_that("that mean() is a special case of .wmean() with no/unit weights", {
  x <- 1:10
  expect_equal(.wmean(x), mean(x))
  expect_equal(.wmean(x, w = rep(1, length(x))), mean(x))
})

test_that(".wmean() returns correct results", {
  x <- 1:5
  w <- rep(2, 5)
  expect_equal(.wmean(x, w), 3)
  w<- rep(0.5, 5)
  expect_equal(.wmean(x, w), 3)
})

test_that(".wmean() handles NA values as intended", {
  expect_equal(.wmean(c(1, 2), c(NA, 1)), NA_real_)
  expect_equal(.wmean(c(1, 2), c(NA, 1), TRUE), 2)
})

test_that(".wmean() throws an error if invalid input is given", {
  expect_error(.wmean(x = 1:9, w = 1:8))
  expect_error(.wmean(1:10, na.rm = 1))
  expect_error(.wmean(NULL))
})

#### .wsd ####

test_that(".wsd() gives the same results as sd() when weights are not specified", {
  x <- 1:10
  expect_equal(.wsd(x), sd(x))
})

# TODO: It's not obvious how to test this function in a good way. Simulating
# data with a certain sd would only allow for approximately checking with some
# error tolerance. Can (simple) manual examples with an intuitive correct answer
# be constructed?

#### .wquantile ####

test_that(".wquantile works", {
  expect_equal(.wquantile(x = c(1, 3, 2), w = NULL, prob = 0.5), 2)
  # TODO: Intuitively this should be exactly 3. Why isn't it? Is it because of the
  # definition of the weighted quantile or a bug? Investigate. Might just be a
  # consequence of the definition.
  # expect_equal(.wquantile(x = c(1, 2, 3), w = c(1, 1, 3), prob = 0.5), 3)
  expect_equal(.wquantile(x = c(1, 2, 3), w = c(2, 2, 2), prob = 0.5), 2)
})

#### .wsum ####

test_that("sum() is a special case of .wsum() with unit/no weights", {
  x <- 1:10
  expect_equal(.wsum(x), sum(x))
  expect_equal(.wsum(x, rep(1L, length(x))), sum(x))
})

test_that(".wsum() returns correct results", {
  x <- 1:5
  n <- length(x)
  expect_equal(.wsum(x, rep(1L, n)), 15)
  expect_equal(.wsum(x, rep(1.5, n)), 22.5)
  expect_equal(.wsum(x, rep(2L, n)), 30)
})

test_that(".wsum() handles NA values as intended", {
  expect_equal(.wsum(NA), NA_real_)
  expect_equal(.wsum(c(1, NA)), NA_real_)
  expect_equal(.wsum(NA, na.rm = TRUE), 0)
  expect_equal(.wsum(c(1, NA), na.rm = TRUE), 1)
  expect_equal(.wsum(c(1, NA, 1), c(3, NA, NA), na.rm = TRUE), 3)
})

test_that(" .wsum() throws an error if invalid input is given", {
  expect_error(.wsum(x = 1:9, w = 1:8))
  expect_error(.wsum(1:10, na.rm = 1))
  expect_error(.wsum(NULL))
})
