#' hiper_glm
#'
#' @param design the GLM design matrix
#' @param outcome the outcome variable
#' @export


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

  # define log-likelihood function
  loglik_fn <- function(beta) {
    resid <- outcome - design %*% beta
    -0.5 / noise_var * sum(resid^2)
  }
  
  # define gradient of log-likelihood (w.r.t beta)
  loglik_grad <- function(beta) {
    resid <- outcome - design %*% beta
    grad <- t(design) %*% resid / noise_var
    as.vector(grad)
  }
  
  # init guess with a zero vector for beta
  beta_init <- rep(0, ncol(design))

  # use optim w/ BFGS
  # NOTE: fnscale = -1 makes optim maximize the log-likelihood
  opt_result <- optim(
    par = beta_init,
    fn = loglik_fn,
    gr = loglik_grad,
    method = "BFGS",
    control = list(fnscale = -1)
  )
  
  return(opt_result$par)
}


# hiperglm: Fit a GLM using either pseudo-inverse (OLS) or BFGS optimization,
hiperglm <- function(design, outcome, optimizer = c("pseudo", "BFGS")) {

  # validate optimizer choice (thorw error if invalid choice)
  optimizer <- match.arg(optimizer)

  # choose approach based on specified optimizer
  if (optimizer == "pseudo") {
    beta_hat <- hiperglm_pseudo(design, outcome)
  } else if (optimizer == "BFGS") {
    beta_hat <- hiperglm_bfgs(design, outcome)
  }
  
  # build and return updated model object
  model_out <- list(
    design = design,
    outcome = outcome,
    optimizer = optimizer,
    coef = beta_hat
  )
  class(model_out) <- "hiperglm"
  
  return(model_out)
}