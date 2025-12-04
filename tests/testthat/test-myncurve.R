test_that("myncurve returns correct mu", {
  res <- myncurve(mu = 10, sigma = 5, a = 6)
  expect_true(is.list(res))
  expect_named(res, c("mu","sigma","a","area"))
  expect_equal(res$mu, 10)
})

test_that("myncurve returns correct sigma", {
  res <- myncurve(mu = 10, sigma = 5, a = 6)
  expect_equal(res$sigma, 5)
})

test_that("myncurve computes correct area", {
  res <- myncurve(mu = 10, sigma = 5, a = 6)
  expected <- pnorm(6, mean = 10, sd = 5)
  expect_equal(res$area, expected, tolerance = 1e-10)
})
