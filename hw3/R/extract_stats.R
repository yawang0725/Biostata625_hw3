#' Extract Statistical Measures
#'
#' This function extracts statistical measures (t-statistics, p-values, confidence intervals)
#' from a linear regression model fit result.
#'
#' @param fit_result A list containing the results from a linear regression model
#' @param alpha The significance level for confidence intervals (default is 0.05)
#'
#' @return A list containing:
#' - t_statistic: t-statistics for each coefficient
#' - p_value: p-values for each coefficient
#' - conf_int: Confidence intervals for each coefficient
#' - mse: Mean squared error
#' @export
extract_stats <- function(fit_result, alpha = 0.05) {

  # Extract betahat and standard errors
  betahat <- fit_result$betahat
  se_betahat <- fit_result$se_betahat
  degree <- fit_result$degree
  mse <- fit_result$mse

  # Calculate t-statistics
  t_stats <- betahat / se_betahat

  # Calculate p-values based on t-statistics
  p_values <- 2 * (1 - pt(abs(t_stats), df = degree))

  # Calculate confidence intervals
  t_critical <- qt(1 - alpha / 2, df = degree)
  ci_low <- betahat - t_critical * se_betahat
  ci_up <- betahat + t_critical * se_betahat
  conf_int <- cbind(ci_low, ci_up)

  # Return results as a list
  return(list(
    t_statistic = t_stats,
    p_value = p_values,
    conf_int = conf_int,
    mse = mse
  ))
}
