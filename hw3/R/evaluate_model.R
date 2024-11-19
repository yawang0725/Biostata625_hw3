#' Evaluate Model Performance
#'
#' This function evaluates the performance of a linear regression model by computing R-squared,
#' Adjusted R-squared, F-statistic, and its associated p-value.
#'
#' @param fit_result A list containing the results from a linear regression model
#' @param dataset The dataset used to fit the model
#' @param response_var The name of the response variable
#' @param predictor_vars A vector of predictor variables
#'
#' @return A list containing:
#' - R_squared: The R-squared value
#' - Adj_R_squared: The Adjusted R-squared value
#' - F_statistic: The F-statistic value
#' - p_value_F: The p-value for the F-statistic
#' @export
evaluate_model_performance <- function(fit_result, dataset, response_var, predictor_vars) {
  # Prepare data
  y <- as.matrix(dataset[[response_var]])
  residuals <- fit_result$residuals
  n <- length(y)
  p <- length(predictor_vars) + 1  # including intercept

  # Calculate total sum of squares
  ss_total <- sum((y - mean(y))^2)

  # Calculate residual sum of squares
  ss_residual <- sum(residuals^2)

  # Calculate R-squared and Adjusted R-squared
  R_squared <- 1 - (ss_residual / ss_total)
  Adj_R_squared <- 1 - (1 - R_squared) * (n - 1) / (n - p)

  # Calculate F-statistic
  F_statistic <- (ss_total - ss_residual) / p / (ss_residual / (n - p))

  # Calculate p-value for F-statistic
  p_value_F <- 1 - pf(F_statistic, p - 1, n - p)

  # Return the evaluation metrics
  return(list(
    R_squared = R_squared,
    Adj_R_squared = Adj_R_squared,
    F_statistic = F_statistic,
    p_value_F = p_value_F
  ))
}
