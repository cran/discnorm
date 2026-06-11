make_disc_data <- function(n = 200, seed = 1) {
  set.seed(seed)
  Sigma <- diag(3)
  Sigma[Sigma == 0] <- 0.5
  norm.data <- MASS::mvrnorm(n, mu = rep(0, 3), Sigma = Sigma)
  apply(norm.data, 2, cut, breaks = c(-Inf, -0.5, 0.5, Inf), labels = FALSE)
}

test_that("bootTest validates its input", {
  disc.data <- make_disc_data()
  expect_error(bootTest(disc.data[, 1, drop = FALSE]), "at least two variables")
  na.data <- disc.data
  na.data[1, 1] <- NA
  expect_error(bootTest(na.data), "missing values")
  expect_error(bootTest(disc.data + 0.5), "integer-valued")
  bin.data <- (disc.data > 1) + 0L
  expect_error(bootTest(bin.data), "more than two categories")
})

test_that("bootTest returns a valid p-value on discretized normal data", {
  disc.data <- make_disc_data(n = 200, seed = 1)
  set.seed(42)
  p <- bootTest(disc.data, B = 25, verbose = FALSE)
  expect_true(is.numeric(p) && length(p) == 1)
  expect_gte(p, 0)
  expect_lte(p, 1)
})
