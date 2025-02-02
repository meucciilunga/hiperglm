# Coefficients
#' Extract coefficients from a hiperglm object
#' @export
coef.hiperglm <- function(object, ...) {
  warning("coef.hiperglm is not yet implemented.")
  return(NULL)
}

# Covariance Matrix
#' Extract covariance matrix from a hiperglm object
#' @export
vcov.hiperglm <- function(object, ...) {
  warning("vcov.hiperglm is not yet implemented.")
  return(NULL)
}

#' Summarize Model
#' Print method for a hiperglm object
#' @export
print.hiperglm <- function(x, ...) {
  warning("print.hiperglm is not yet implemented.")
  invisible(x)
}