# tests/testthat/helper.R

# Per HW2 Instructions (778): `are_all_close` and `simulate_data`
# were both copied Directly from Aki's repo

are_all_close <- function(v, w, abs_tol = 1e-6, rel_tol = 1e-6) {
  abs_diff <- abs(v - w)
  are_all_within_atol <- all(abs_diff < abs_tol)
  are_all_within_rtol <- all(abs_diff < rel_tol * pmax(abs(v), abs(w)))
  return(are_all_within_atol && are_all_within_rtol)
}

simulate_data <- function(
    n_obs, n_pred, model = "linear", intercept = NULL, 
    coef_true = NULL, design = NULL, seed = NULL, signal_to_noise = 0.1
  ) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (is.null(coef_true)) {
    coef_true <- rnorm(n_pred, sd = 1 / sqrt(n_pred))
  }
  if (is.null(design)) {
    design <- matrix(rnorm(n_obs * n_pred), nrow = n_obs, ncol = n_pred)
  }
  if (!is.null(intercept)) {
    if (!is.numeric(intercept)) {
      stop("The intercept argument must be numeric.")
    }
    coef_true <- c(intercept, coef_true)
    design <- cbind(rep(1, n_obs), design)
  }
  expected_mean <- as.vector(design %*% coef_true)
  noise_magnitude <- sqrt(var(expected_mean) / signal_to_noise^2)
  noise <- noise_magnitude * rnorm(n_obs)
  outcome <- expected_mean + noise
  return(list(design = design, outcome = outcome, coef_true = coef_true))
}

# Part HW2.E4.6(b)(c) -- Requires code from HW2.E3(B);
# Helper code extracted from that part of assignment and put here:

# Finite-difference gradient approximation
approx_grad <- function(func, x, dx = .Machine$double.eps^(1/3)) {
  numerical_grad <- rep(0, length(x))

  for (i in seq_along(x)) {
    x_forward <- x
    x_backward <- x
    x_forward[i] <- x_forward[i] + dx
    x_backward[i] <- x_backward[i] - dx
    numerical_grad[i] <- (func(x_forward) - func(x_backward)) / (2 * dx)
  }
  
  return(numerical_grad)
}