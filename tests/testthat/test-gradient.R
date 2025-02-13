# tests/testthat/test-gradient.R

library(testthat)

test_that("Log-likelihood gradient is consistent with finite differences", {
  
  # create small. random design matrix + outcome vector
  set.seed(140778)
  n <- 10
  p <- 4
  design <- matrix(rnorm(n * p), n, p)
  outcome <- rnorm(n)
  noise_var <- 1
  
  # Wrap loglik support functions from loglik.R so only input is in beta, like before
  llgr <- function(beta) loglik_grad(design, outcome, noise_var, beta)
  llfn <- function(beta) loglik_fn(design, outcome, noise_var, beta)

  # pick test value
  beta_test <- rep(0.5, p)
  
  # compute gradient analytically
  analytical <- llgr(beta_test)
  
  # computer gradient numerically (HW2 -- exercise 3 implementation)
  numerical <- approx_grad(llfn, beta_test)
  
  # check if they're close
  expect_true(are_all_close(analytical, numerical, rel_tol = 1e-3),
              info = "Analytical gradient does not match numerical approximation")
})