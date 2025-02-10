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
  
  # ------- Reimplementation of hiperglm_bfgs() from hiperglm.R -------
  # define the log-likelihood function as in hiperglm_bfgs()
  loglik_fn <- function(beta) {
    resid <- outcome - design %*% beta
    -0.5 / noise_var * sum(resid^2)
  }
  
  # define the analytical gradient as in hiperglm_bfgs()
  loglik_grad <- function(beta) {
    resid <- outcome - design %*% beta

    # Compute the gradient: design^T (outcome - design beta) / noise_var
    as.vector(t(design) %*% resid / noise_var)
  }
  # -------------------------------------------------------------------

  # pick test value
  beta_test <- rep(0.5, p)
  
  # compute gradient analytically
  analytical <- loglik_grad(beta_test)
  
  # computer gradient numerically (HW2 -- exercise 3 implementation)
  numerical <- approx_grad(loglik_fn, beta_test)
  
  # check if they're close
  expect_true(are_all_close(analytical, numerical, rel_tol = 1e-3),
              info = "Analytical gradient does not match numerical approximation")
})