#' hiper_glm
#'
#' @param design the GLM design matrix
#' @param outcome the outcome variable
#' @export

hiperglm <- function(design, outcome) {

  # TODO - implement glm
  warning("hiper_glm is not yet implemented.")
  
  # Return a list with class "hiperglm"
  model_out <- list(design = design, outcome = outcome)
  class(model_out) <- "hiperglm"
  
  return(model_out)
}