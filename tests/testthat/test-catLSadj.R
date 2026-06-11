make_disc_data <- function(n = 300, seed = 2) {
  set.seed(seed)
  Sigma <- diag(3)
  Sigma[Sigma == 0] <- 0.5
  norm.data <- MASS::mvrnorm(n, mu = rep(0, 3), Sigma = Sigma)
  disc <- apply(norm.data, 2, cut, breaks = c(-Inf, -0.5, 0.5, Inf), labels = FALSE)
  data.frame(disc)
}

normal_margin <- list(F = pnorm, qF = qnorm, sd = 1)

test_that("catLSadj validates its input", {
  disc.data <- make_disc_data()
  expect_error(catLSadj(disc.data, list(normal_margin, normal_margin)),
               "one element per column")
  expect_error(catLSadj(disc.data[, 1, drop = FALSE], list(normal_margin)),
               "at least two columns")
  bad_margin <- list(qF = qnorm, sd = 1) # lacks F
  expect_error(catLSadj(disc.data, list(normal_margin, normal_margin, bad_margin)),
               "lacks F")
  not_cdf <- list(F = function(x) pnorm(x) / 2, qF = qnorm, sd = 1)
  expect_error(catLSadj(disc.data, list(normal_margin, normal_margin, not_cdf)),
               "does not evaluate to zero and one")
})

test_that("catLSadj with standard normal margins reproduces the polychoric matrix", {
  disc.data <- make_disc_data()
  adjusted <- catLSadj(disc.data, list(normal_margin, normal_margin, normal_margin))
  polcorr <- lavaan::lavCor(disc.data, ordered = names(disc.data), cor.smooth = TRUE)
  # with N(0,1) margins the adjustment is the identity map
  expect_equal(unclass(adjusted[[1]]), unclass(polcorr), tolerance = 1e-3,
               ignore_attr = TRUE)
  # gamma has rows/cols for thresholds (2 per variable) plus 3 correlations
  expect_equal(dim(adjusted[[2]]), c(9, 9))
})

test_that("catLSadj accepts matrix input via coercion", {
  disc.data <- make_disc_data()
  adjusted.df <- catLSadj(disc.data, list(normal_margin, normal_margin, normal_margin))
  adjusted.mat <- catLSadj(as.matrix(disc.data), list(normal_margin, normal_margin, normal_margin))
  expect_equal(adjusted.df[[1]], adjusted.mat[[1]])
})
