# logit_loglik_fn: Computes the log-likelihood for a logistic model.
# Uses the formulation: sum(y * (Xβ) - log(1 + exp(Xβ)))
logit_loglik_fn <- function(design, outcome, beta) {
  eta <- design %*% beta
  ll <- sum(outcome * eta - log(1 + exp(eta)))
  return(ll)
}

# logit_loglik_grad: Computes the gradient of the logistic log-likelihood.
# The gradient is given by: t(X) %*% (outcome - σ(Xβ)), where σ(.) is the logistic function.
logit_loglik_grad <- function(design, outcome, beta) {
  eta <- design %*% beta
  sigma <- 1 / (1 + exp(-eta))
  grad <- t(design) %*% (outcome - sigma)
  return(as.vector(grad))
}

# Optimizer function: BFGS method for logistic MLE.
hiperglm_bfgs_logit <- function(design, outcome) {

  # Wrap the logistic log-likelihood and gradient so that only beta is required.
  llfn <- function(beta) logit_loglik_fn(design, outcome, beta)
  llgr <- function(beta) logit_loglik_grad(design, outcome, beta)
  
  beta_init <- rep(0, ncol(design))
  opt_result <- optim(
    par = beta_init,
    fn = llfn,
    gr = llgr,
    method = "BFGS",
    control = list(fnscale = -1)
  )
  
  if (opt_result$convergence != 0) {
    warning("BFGS optimization (logistic) did not converge: ", opt_result$message)
  }
  
  return(opt_result$par)
}

# Logistic Hessian: Second derivative of the log-likelihood
logistic_loglik_hess <- function(design, outcome, beta) {
  p <- 1 / (1 + exp(-design %*% beta))
  W <- as.vector(p * (1 - p))
  - t(design) %*% (design * W)
}