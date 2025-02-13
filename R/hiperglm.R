# R/hiperglm.R

#' hiperglm
#' 
#' Fit a GLM via maximum likelihood. For the Gaussian model, uses either
#' pseudo-inverse (OLS) or BFGS optimization. For the logistic model, supports
#' MLE estimation via BFGS.
#' 
#' @param design A numeric matrix representing the design matrix.
#' @param outcome A numeric vector representing the response variable.
#' @param model Character. The model to fit; must be either "gaussian" or "logit".
#'              Default is "gaussian".
#' @param option A list of options. For the Gaussian model, use option$mle_solver to choose between "pseudo" or "BFGS".
#'               For the logistic model, currently only BFGS is supported (option$mle_solver should be "BFGS").
#' @return A list containing the fitted model components.
#' @export
hiperglm <- function(design, outcome, model = "gaussian", option = list()) {
  
  if (model == "gaussian") {
    
    # For Gaussian, choose the MLE solver: default to "pseudo".
    mle_solver <- if (is.null(option$mle_solver)) "pseudo" else option$mle_solver
    valid_opts <- c("pseudo", "BFGS")
    mle_solver <- match.arg(mle_solver, choices = valid_opts)
    
    if (mle_solver == "pseudo") {
      beta_hat <- hiperglm_pseudo(design, outcome)
    } else if (mle_solver == "BFGS") {
      beta_hat <- hiperglm_bfgs(design, outcome)
    }
    
  } else if (model == "logit") {
    # For logistic regression, we currently support only BFGS.
    mle_solver <- if (is.null(option$mle_solver)) "BFGS" else option$mle_solver
    if (mle_solver != "BFGS") {
      stop("For the logit model, only BFGS is supported.")
    }
    beta_hat <- hiperglm_bfgs_logit(design, outcome)
    
  } else {
    stop("Unsupported model type. Choose either 'gaussian' or 'logit'.")
  }
  
  model_out <- list(
    design = design,
    outcome = outcome,
    model = model,
    mle_solver = mle_solver,
    coef = beta_hat
  )
  class(model_out) <- "hiperglm"
  return(model_out)
}