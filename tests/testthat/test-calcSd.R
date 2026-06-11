test_that("calcSd is correct for distributions whose support includes zero", {
  expect_equal(discnorm:::calcSd(pnorm, qnorm), 1, tolerance = 1e-4)
  expect_equal(discnorm:::calcSd(pexp, qexp), 1, tolerance = 1e-4)
})

test_that("calcSd is correct when the support lies strictly above zero", {
  # regression test: pre-0.2.2 the expectation integral started at the lower
  # end of the support instead of 0, giving sd 2.47 instead of 0.289 here
  expect_equal(discnorm:::calcSd(function(x) punif(x, 2, 3),
                                 function(p) qunif(p, 2, 3)),
               1 / sqrt(12), tolerance = 1e-3)
})

test_that("calcSd is correct when the support lies strictly below zero", {
  expect_equal(discnorm:::calcSd(function(x) punif(x, -3, -2),
                                 function(p) qunif(p, -3, -2)),
               1 / sqrt(12), tolerance = 1e-3)
})
