#' Generate Full Model Summary (Merged into One Function)
#'
#' This function generates a full regression summary, including:
#' - Regression equation
#' - Coefficient table
#' - Model fit statistics
#' - Model evaluation
#'
#' @param dataset A data.frame containing the data
#' @param response_var The name of the response variable (dependent variable)
#' @param predictor_vars A vector of names of predictor variables (independent variables)
#' @param intercept Logical; whether to include an intercept term (default is TRUE)
#'
#' @return A printed model summary with all relevant statistics.
#' @export
generate_full_summary <- function(dataset, response_var, predictor_vars, intercept = TRUE) {

  # Prepare Data (Response variable and Predictor matrix)
  y <- as.matrix(dataset[[response_var]])  # Response variable (y)
  X <- as.matrix(dataset[predictor_vars])  # Predictor variables (X)

  # If intercept is TRUE, add an intercept term (a column of ones)
  if (intercept) {
    X <- cbind(1, X)  # Add intercept
    colnames(X)[1] <- "(Intercept)"  # Name the first column as "(Intercept)"
  }

  # Fit the OLS model: Calculate betahat (regression coefficients)
  betahat <- solve(t(X) %*% X) %*% t(X) %*% y  # OLS estimation of coefficients

  # Predicted values (y_hat)
  y_hat <- X %*% betahat

  # Residuals (difference between observed and predicted values)
  residuals <- y - y_hat

  # Variance-covariance matrix for betahat
  var_betahat <- solve(t(X) %*% X) * mean(residuals^2)

  # Standard errors for betahat (diagonal of the variance-covariance matrix)
  se_betahat <- sqrt(diag(var_betahat))

  # Calculate Model Statistics: Mean Squared Error (MSE) and Degrees of Freedom
  n <- nrow(X)  # Number of observations
  p <- ncol(X)  # Number of parameters (predictors + intercept)

  mse <- sum(residuals^2) / (n - p)  # Mean Squared Error (MSE)
  degree <- n - p  # Degrees of freedom (n - p)

  # Construct regression equation
  equation <- paste0(response_var, " = ",
                     paste(sprintf("%.3f * %s", betahat[-1], predictor_vars), collapse = " + "))
  if (!is.na(betahat[1])) {
    equation <- paste0(equation, sprintf(" + %.3f (Intercept)", betahat[1]))
  }

  # Coefficients and significance
  t_statistic <- betahat / se_betahat  # t-statistics
  p_value <- 2 * (1 - pt(abs(t_statistic), df = degree))  # p-values (two-tailed test)

  # Confidence intervals (using normal approximation)
  ci_low <- betahat - 1.96 * se_betahat  # 95% Confidence Interval Lower Bound
  ci_up <- betahat + 1.96 * se_betahat  # 95% Confidence Interval Upper Bound

  # Display significance stars
  significance_stars <- function(p) {
    if (p < 0.001) return("***")
    if (p < 0.01) return("**")
    if (p < 0.05) return("*")
    if (p < 0.1) return(".")
    return("")
  }
  sig <- sapply(p_value, significance_stars)

  # Create a data frame for the coefficient table
  coef_table <- data.frame(
    Term = c("(Intercept)", predictor_vars),
    Estimate = betahat,
    Std.Error = se_betahat,
    tvalue = t_statistic,
    `Pr(>|t|)` = p_value,
    Significance = sig,
    `Lower CI` = ci_low,
    `Upper CI` = ci_up
  )

  # Calculate model fit statistics
  R_squared <- 1 - sum(residuals^2) / sum((y - mean(y))^2)  # R-squared
  F_statistic <- (sum((y_hat - mean(y))^2) / (p - 1)) / mse  # F-statistic
  p_value_F <- pf(F_statistic, df1 = p - 1, df2 = degree, lower.tail = FALSE)  # p-value for F-statistic

  # Print the model summary
  cat("\n================= Regression Summary =================\n")
  cat("Regression Equation:\n")
  cat(equation, "\n")

  cat("\nRegression Coefficients:\n")
  print(coef_table, row.names = FALSE)

  cat("\nModel Fit:\n")
  cat(sprintf("R-squared: %.3f\n", R_squared))
  cat(sprintf("Adjusted R-squared: %.3f\n", 1 - (1 - R_squared) * (n - 1) / (n - p)))  # Adjusted R-squared
  cat(sprintf("F-statistic: %.3f, p-value: %.3e\n", F_statistic, p_value_F))

  cat("\nModel Evaluation:\n")
  if (R_squared > 0.8) {
    cat("The model explains most of the variance, fitting well.\n")
  } else if (R_squared > 0.5) {
    cat("The model explains a moderate amount of variance.\n")
  } else {
    cat("The model explains a small amount of variance, improvements might be needed.\n")
  }
  cat("=====================================================\n")
}
