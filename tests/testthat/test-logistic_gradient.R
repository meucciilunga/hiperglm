# tests/testthat/test-logistic.R

library(testthat)

test_that("Logistic log-likelihood gradient is consistent with finite differences", {
  set.seed(1234)
  n <- 20
  p <- 3
  design <- matrix(rnorm(n * p), n, p)
  
  # Generate binary outcomes from a logistic model
  true_beta <- runif(p)
  eta <- design %*% true_beta
  prob <- 1 / (1 + exp(-eta))
  outcome <- rbinom(n, size = 1, prob = prob)
  
  llfn <- function(beta) logit_loglik_fn(design, outcome, beta)
  llgr <- function(beta) logit_loglik_grad(design, outcome, beta)
  
  beta_test <- rep(0.1, p)
  analytical <- llgr(beta_test)
  numerical <- approx_grad(llfn, beta_test)
  
  expect_true(are_all_close(analytical, numerical, rel_tol = 1e-3),
              info = "Analytical logistic gradient does not match numerical approximation")
})
