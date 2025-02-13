# R/methods.R

# Coefficients
#' Extract coefficients from a hiperglm object
#' @export
coef.hiperglm <- function(object, ...) {
  # Return the numeric coefficient vector stored in the model
  return(object$coef)
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
  cat("hiperglm model fitted using", x$optimizer, "optimizer.\n")
  cat("Coefficients:\n")
  print(x$coef)
  invisible(x)
}