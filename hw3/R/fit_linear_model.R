#' Fit Linear Model and Extract Statistical Measures
#'
#' This function fits a linear regression model and extracts various statistical measures.
#'
#' @param dataset A data.frame containing the data
#' @param response_var The name of the response variable (dependent variable)
#' @param predictor_vars A vector of names of predictor variables (independent variables)
#' @param intercept Logical; whether to include an intercept term (default is TRUE)
#'
#' @return A list containing:
#' - betahat: Estimated regression coefficients
#' - y_hat: Predicted values
#' - residuals: Residuals of the model
#' - var_betahat: Variance-covariance matrix for the regression coefficients
#' - se_betahat: Standard errors of the regression coefficients
#' - mse: Mean squared error
#' - degree: Degrees of freedom (n - p)
#' @export
fit_linear_model <- function(dataset, response_var, predictor_vars, intercept = TRUE) {

  # Prepare Data
  y <- prepare_response_var(dataset, response_var)
  X <- prepare_predictor_matrix(dataset, predictor_vars, intercept)

  # Fit Model using OLS
  fit_results <- fit_ols(X, y)

  # Calculate Residuals
  residuals <- calculate_residuals(y, fit_results$y_hat)

  # Get model statistics
  stats <- calculate_model_stats(X, residuals)

  # Return the fit results as a list
  return(list(
    betahat = fit_results$betahat,
    y_hat = fit_results$y_hat,
    residuals = residuals,
    var_betahat = fit_results$var_betahat,
    se_betahat = fit_results$se_betahat,
    mse = stats$mse,
    degree = stats$degree
  ))
}

# Helper function to prepare response variable (y)
prepare_response_var <- function(dataset, response_var) {
  return(as.matrix(dataset[[response_var]]))
}

# Helper function to prepare predictor matrix (X)
prepare_predictor_matrix <- function(dataset, predictor_vars, intercept) {
  X <- as.matrix(dataset[predictor_vars])
  if (intercept) {
    X <- cbind(1, X)  # Add intercept
    colnames(X)[1] <- "(Intercept)"
  }
  return(X)
}

# Helper function to fit OLS model and extract coefficients and predictions
fit_ols <- function(X, y) {
  betahat <- solve(t(X) %*% X) %*% t(X) %*% y  # Estimated coefficients
  y_hat <- X %*% betahat  # Predicted values
  var_betahat <- solve(t(X) %*% X) * mean((y - y_hat)^2)  # Variance-covariance matrix

  # Standard errors for betahat
  se_betahat <- sqrt(diag(var_betahat))

  return(list(
    betahat = betahat,
    y_hat = y_hat,
    var_betahat = var_betahat,
    se_betahat = se_betahat
  ))
}

# Helper function to calculate residuals
calculate_residuals <- function(y, y_hat) {
  return(y - y_hat)
}

# Helper function to calculate model statistics like MSE and degree of freedom
calculate_model_stats <- function(X, residuals) {
  n <- nrow(X)
  p <- ncol(X)

  mse <- sum(residuals^2) / (n - p)  # Mean squared error
  degree <- n - p  # Degrees of freedom

  return(list(
    mse = mse,
    degree = degree
  ))
}
