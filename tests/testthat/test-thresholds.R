test_that("pc_th recovers thresholds from category proportions", {
  # 25% / 25% / 50% in categories 1, 2, 3
  y <- rep(c(1, 2, 3), times = c(25, 25, 50))
  expect_equal(discnorm:::pc_th(y), stats::qnorm(c(0.25, 0.5)))
})

test_that("pc_th is invariant to the integer coding origin", {
  y0 <- rep(c(0, 1, 2), times = c(10, 20, 70))
  y5 <- rep(c(5, 6, 7), times = c(10, 20, 70))
  expect_equal(discnorm:::pc_th(y0), discnorm:::pc_th(y5))
})

test_that("pc_PI returns a proper probability table", {
  th1 <- c(-0.5, 0.5)
  th2 <- c(-1, 0, 1)
  PI <- discnorm:::pc_PI(0.4, th1, th2)
  expect_equal(dim(PI), c(length(th1) + 1L, length(th2) + 1L))
  expect_true(all(PI > 0))
  expect_equal(sum(PI), 1, tolerance = 1e-10)
})

test_that("pc_PI reduces to the independence table when rho = 0", {
  th1 <- c(-0.5, 0.5)
  th2 <- c(-1, 0, 1)
  PI <- discnorm:::pc_PI(0, th1, th2)
  rowP <- diff(c(0, stats::pnorm(th1), 1))
  colP <- diff(c(0, stats::pnorm(th2), 1))
  expect_equal(PI, outer(rowP, colP))
})
