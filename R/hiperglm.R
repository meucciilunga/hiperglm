# R/hiperglm.R

#' hiper_glm
#'
#' Fit a GLM using either pseudo-inverse (OLS) or BFGS optimization.
#'
#' @param design A numeric matrix representing the linear model design matrix.
#' @param outcome A numeric vector representing the response variable observations.
#' @param optimizer Character. Optimization method to use. Must be either "pseudo" or "BFGS".
#'                  Default is "pseudo".
#' @return A list containing the fitted model components.
#' @export

# hiperglm: Fit a GLM using either pseudo-inverse (OLS) or BFGS optimization,
hiperglm <- function(design, outcome, optimizer = "pseudo") {

  # Sanity check: validate optimizer choice (thorw error if invalid choice)
  valid_opts <- c("pseudo", "BFGS")
  optimizer <- match.arg(optimizer, choices = valid_opts)

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