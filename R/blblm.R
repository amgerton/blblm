#' @import purrr
#' @import stats
#' @import furrr
#' @importFrom magrittr %>%
#' @importFrom utils capture.output
#' @aliases NULL
#' @details
#' Linear Regression with Little Bag of Bootstraps with the Option to Use Parallel Programming
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))

#' blblm function to compute Linear Regression with Little Bag of Bootstraps
#'
#' This function also has the option to use parallel programming so if you are using parallel programming, need to specify your workers first. For example, plan(multisession, workers = 4).
#'
#' @param formula a formula
#' @param data a dataframe or a character vector
#' @param m an integer that is the number of subsets in the data
#' @param B an integer that is the number of bootstraps
#' @param parallel a logical character
#'
#' @return blblm
#'
#' @examples fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
#' @export
blblm <- function(formula, data, m = 10, B = 5000, parallel = FALSE) {
  data_list <- split_data(data, m)
  if (parallel == FALSE) {
    map_func = map
  } else if (parallel == TRUE) {
    map_func = future_map
  } else{
    stop("You need equal parallel to TRUE or FALSE")
  }
  estimates <- map_func(
    data_list,
    ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}


#' split data into m parts of approximated equal sizes
#'
#' @param data a dataframe
#' @param m an integer that is the number of subsets in the data
#'
#' @return seperate dataframes
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}


#' Compute the Estimates
#'
#' @param formula a formula
#' @param data a dataframe
#' @param n numeric
#' @param B numeric
lm_each_subsample <- function(formula, data, n, B) {
  replicate(B, lm_each_boot(formula, data, n), simplify = FALSE)
}


#' compute the regression estimates for a blb dataset
#'
#' @param formula a formula
#' @param data a dataframe
#' @param n an integer
lm_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  lm1(formula, data, freqs)
}


#' Fitting Linear Model with A Bag of Little Bootstraps
#' estimate the regression estimates based on given the number of repetitions
#'
#' @param formula an object of class "formula"
#' @param data 	a data frame, list or environment
#' @param freqs numeric
#' @return an object of class "lm" or for multiple responses of class
lm1 <- function(formula, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wront variable from the global scope.
  environment(formula) <- environment()
  fit <- lm(formula, data, weights = freqs)
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}


#' Extracts Coefficients
#' compute the coefficients from fit
#'
#' @param fit a fitted blblm model
#' @return numeric values
blbcoef <- function(fit) {
  coef(fit)
}


#' Extract Residual Standard Deviation
#'
#' compute sigma from fit
#'
#' @param fit a fitted blblm model
#' @return a numeric value
blbsigma <- function(fit) {
  p <- fit$rank
  y <- model.extract(fit$model, "response")
  e <- fitted(fit) - y
  w <- fit$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}


#' Print Values
#'
#' print.blblm prints an argument
#' @param x an object used to select a method
#' @param ... further arguments passed to or from other methods
#'
#' @return a value
#' @export
#' @method print blblm
print.blblm <- function(x, ...) {
  cat("blblm model:", capture.output(x$formula))
  cat("\n")
}


#' Extracts the Value of Sigma
#'
#' sigma.blblm extracts the sigma from the model
#' @param object an R object
#'
#' @param confidence logical
#' @param level the confidence level
#' @param ... additional arguments
#'
#' @export
#' @return a number
#' @method sigma blblm
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - 0.95
    limits <- est %>%
      map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}

#' Extract Model Coefficients
#'
#' coef.blblm is a function which extracts model coefficients
#'
#' @param object an object for which the extraction of model coefficients is meaningfu
#' @param ... other arguments
#'
#' @return coefficients extracted from the model object
#' @export
#' @method coef blblm
coef.blblm <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}


#' The Confidence Level for the function blblm
#'
#' confint.blblm gets the confidence level for the model
#'
#' @param object a fitted model object.
#' @param parm a specification of which parameters are to be given confidence intervals, either a vector of numbers or a vector of names. If missing, all parameters are considered.
#' @param level the confidence level required
#' @param ... a model object for which prediction is desired
#'
#' @export
#' @return A matrix or vector with columns giving lower and upper confidence limits for each parameter.
#' @method confint blblm
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  out <- map_rbind(parm, function(p) {
    map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}

#' Predictions
#'
#' This function gets the predicitions for the model
#'
#' @param object a model object for which prediction is desired
#' @param new_data a dataframe or character vector
#' @param confidence logical
#' @param level double the confident level
#' @param ... additional arguments affecting the predictions produced.
#'
#' @return a vector or matrix or list of your predictions from your model
#' @export
#' @method predict blblm
predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
               apply(1, mean_lwr_upr, level = level) %>%
               t())
  } else {
    map_mean(est, ~ map_cbind(., ~X %*% .$coef) %>% rowMeans())
  }
}



mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}

map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}
