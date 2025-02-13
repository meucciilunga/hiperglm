# --- copied from hw3_helper.R in Aki's repo, as requested in HW3 instructions
library(testthat)

test_that("newton and bfgs outputs coincide on logit model", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'logit', seed = 1918)
  design <- data$design; outcome <- data$outcome

  via_newton_out <- hiperglm(design, outcome, model = 'logit')

  via_bfgs_out <- hiperglm(
    design, outcome, model = 'logit', option = list(mle_solver = 'BFGS')
  )
  expect_true(are_all_close(
    coef(via_newton_out), coef(via_bfgs_out), abs_tol = 1e-2, rel_tol = 1e-2
  ))
})