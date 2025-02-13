# R/model_fitting.R

# loglik_fn: Computes the log-likelihood for a Gaussian model (dropped constants)
gaussian_loglik_fn <- function(design, outcome, noise_var, beta) {
  resid <- outcome - design %*% beta
  -0.5 / noise_var * sum(resid^2)
}

# loglik_grad: Computes the gradient of the Gaussian log-likelihood with respect to beta
gaussian_loglik_grad <- function(design, outcome, noise_var, beta) {
  resid <- outcome - design %*% beta
  as.vector(t(design) %*% resid / noise_var)
}

# Optimizer function: OLS pseudo-inverse
hiperglm_pseudo <- function(design, outcome) {

  # OLS pseudo-inverse approach solved as follows:
  # beta_hat = solve(t(design) %*% design, t(design) %*% outcome)
  XtX <- t(design) %*% design
  XtY <- t(design) %*% outcome
  beta_hat <- solve(XtX, XtY)

  return(beta_hat)
}

# Optimizer function: BFGS method
hiperglm_bfgs <- function(design, outcome, noise_var = 1) {

  # Wrap loglik support functions from loglik.R so only input is in beta, like before
  llfn <- function(beta) gaussian_loglik_fn(design, outcome, noise_var, beta)
  llgr <- function(beta) gaussian_loglik_grad(design, outcome, noise_var, beta)
  
  # init guess with a zero vector for beta
  beta_init <- rep(0, ncol(design))

  # use optim w/ BFGS
  # NOTE: fnscale = -1 makes optim maximize the log-likelihood
  opt_result <- optim(
    par = beta_init,
    fn = llfn,
    gr = llgr,
    method = "BFGS",
    control = list(fnscale = -1)
  )

  # Check convergence: if convergence != 0, warn the user.
  if (opt_result$convergence != 0) {
    warning("BFGS optimization did not converge before returning value: ", opt_result$message)
  }
  
  return(opt_result$par)
}

##############################

# Logistic log-likelihood: Sum of log probabilities for all observations
logistic_loglik_fn <- function(design, outcome, beta) {
  p <- 1 / (1 + exp(-design %*% beta))
  sum(outcome * log(p) + (1 - outcome) * log(1 - p))
}

# Logistic log-likelihood gradient: Derivative with respect to beta
logistic_loglik_grad <- function(design, outcome, beta) {
  p <- 1 / (1 + exp(-design %*% beta))
  as.vector(t(design) %*% (outcome - p))
}