test_that("blblm() computes linear regression with bag of little bootstraps", {
  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = FALSE)
  expect_s3_class(fit, "blblm")
})

test_that("blblm() computes linear regression with bag of little bootstraps with parallel programming", {
  fit1 <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = TRUE)
  expect_s3_class(fit1, "blblm")
})
