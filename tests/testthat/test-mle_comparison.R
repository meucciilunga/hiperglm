# tests/testthat/test-mle_comparison.R

# this test compares mle estimates from pseudo-inverse vs. BFGS 
# (per hw2 instructions: expected to fail for now)

library(testthat)

test_that("pseudo-inverse and BFGS yield similar mle estimates", {

  # for reproducibility
  set.seed(123)
  
  # simulate some test data via aki's helper code
  sim_data <- simulate_data(n_obs = 100, n_pred = 3)
  X <- sim_data$design
  y <- sim_data$outcome
  
  # mle via pseudo-inverse
  model_pseudo <- hiperglm(X, y, optimizer = "pseudo")
  
  # mle via BFGS
  model_bfgs <- hiperglm(X, y, optimizer = "BFGS")
  
  # verify that coefficients from both methods are close enough
  expect_true(are_all_close(coef(model_pseudo), coef(model_bfgs)))
})