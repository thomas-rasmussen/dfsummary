test_that("basic use of tbl_summary works", {

  x <- data.frame(
    var_cont = 1:10,
    var_bin = c(rep(1, 5), rep(0, 5)),
    var_cat = c(rep(1, 5), rep(2, 5))
  )

  dt <- tbl_summary(
    x = x,
    vars = c("var_cont", "var_bin", cat = "var_cat"),
  )

  expect_true(all(
    all(dt$.by == rep(".all", 5L)) &
    all(dt$.var_name == c(".n", "var_bin", "var_cat", "var_cat", "var_cont")) &
    all(dt$.var_type == c("n", "bin", "cat", "cat", "cont")) &
    all(dt$.var_level == c("", "", "1", "2", "")) &
    all(dt$.stat_label == c("n", "n_pct", "n_pct", "n_pct", "median_p25_p75")) &
    all(dt$.stat_value == c("10", "5 (50.0%)", "5 (50.0%)", "5 (50.0%)", "5.5 (3.0;8.0)"))
  ))
})

test_that("use of by-variable works as intended", {
  x <- data.frame(var_bin = c(rep(1, 5), rep(0, 5)))
  x <- rbindlist(list(x, x))
  x$by_var <- c(rep("A", 10L), rep("B", 10L))
  dt <- tbl_summary(
    x = x,
    vars = "var_bin",
    by = "by_var"
  )

  expect_true(all(
    all(dt$.by == rep(c(".all", "A", "B"), each = 2L)) &
    all(dt$.var_name == rep(c(".n", "var_bin"), 3L)) &
    all(dt$.var_type == rep(c("n", "bin"), 3L)) &
    all(dt$.var_level == rep("", 6L)) &
    all(dt$.stat_label == rep(c("n", "n_pct"), 3L)) &
    all(dt$.stat_value == c(
        "20", "10 (50.0%)",
        rep(c("10", "5 (50.0%)"), 2L)
      ))
  ))
})

test_that("masking works as intended", {
    x <- data.frame(
      var = rep(c(0,1), times = 50),
      by = c(rep("A", 5), rep("B", 5), rep("C", 90))
    )
    dt <- tbl_summary(
      x = x,
      vars = c("var"),
      by = "by",
      mask = TRUE
    )

    expect_true(all(
      all(dt$.by == rep(c(".all", "A", "B", "C"), each = 2L)) &
      all(dt$.var_name == rep(c(".n", "var"), 4L)) &
      all(dt$.var_type == rep(c("n", "bin"), 4L)) &
      all(dt$.var_level == rep("", 8L)) &
      all(dt$.stat_label == rep(c("n", "n_pct"), 4L)) &
      all(dt$.stat_value == c(
        "100", "50 (50.0%)",
        rep(c("5", "*"), 2L),
        "90", "45 (50.0%)"
      ))
    ))
})

test_that("tbl_summary performance tests", {
  skip("Only run performance tests manually")

  # Just an informal performance test to get an idea of how efficient the code
  # is.
  n <- 1e6
  x <- data.frame(
    var_cont = rnorm(n),
    var_bin = sample(c(FALSE, TRUE), size = n, replace = TRUE),
    var_cat = sample(c("A", "B"), size = n, replace = TRUE),
    by = sample(c("grp1", "grp2"), size = n, replace = TRUE)
  )
  Sys.time()
  dt <- tbl_summary(x, vars = c("var_cont", "var_bin", "var_cat"), by = "by")
  Sys.time()
  # 8s on private gaming laptop. Definitely acceptable performance.
})
