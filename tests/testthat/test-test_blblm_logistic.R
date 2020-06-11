test_that("blbglm() computes logistic regression with bag of little bootstraps without parallel programming", {
  fit2 <- blbglm(Species ~ Sepal.Length * Sepal.Width, data = iris, m = 2, B = 50, parallel = FALSE)
  expect_s3_class(fit2, "blbglm")
})

test_that("blbglm() computes logistic regression with bag of little bootstraps with parallel programming", {
  fit3 <- blbglm(Species ~ Sepal.Length * Sepal.Width, data = iris, m = 2, B = 50, parallel = TRUE)
  expect_s3_class(fit3, "blbglm")
})
