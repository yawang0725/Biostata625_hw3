#' Clean Data for Linear Regression
#'
#' This function performs the following preprocessing steps:
#' - Handles missing values
#' - Handles outliers
#' - Scales numeric data
#' - Removes constant columns
#'
#' @param dataset A data.frame to be cleaned
#' @param method_missing How to handle missing data: "omit", "mean", or "median"
#' @param method_outliers How to handle outliers: "remove", "replace", or "none"
#' @param scale_data Logical; whether to scale numeric data (default is FALSE)
#' @param remove_constant Logical; whether to remove columns with constant values (default is FALSE)
#' @return A cleaned data.frame
#' @export
clean_data <- function(dataset,
                       method_missing = c("omit", "mean", "median"),
                       method_outliers = c("remove", "replace", "none"),
                       scale_data = FALSE,
                       remove_constant = FALSE) {

  # Match arguments
  method_missing <- match.arg(method_missing)
  method_outliers <- match.arg(method_outliers)

  # Step 1: Identify numeric columns
  numeric_columns <- sapply(dataset, is.numeric)

  # Handle missing values
  dataset <- handle_missing_values(dataset, numeric_columns, method_missing)

  # Handle outliers
  dataset <- handle_outliers(dataset, numeric_columns, method_outliers)

  # Standardize the data (if required)
  if (scale_data) {
    dataset <- scale_numeric_columns(dataset, numeric_columns)
  }

  # Remove constant columns
  if (remove_constant) {
    dataset <- remove_constant_columns(dataset)
  }

  return(dataset)
}

# Helper function to handle missing values
handle_missing_values <- function(dataset, numeric_columns, method_missing) {
  if (method_missing == "omit") {
    return(dataset[complete.cases(dataset), ])
  }

  for (col in names(dataset)[numeric_columns]) {
    missing_indices <- is.na(dataset[[col]])

    if (any(missing_indices)) {
      if (method_missing == "mean") {
        dataset[[col]][missing_indices] <- mean(dataset[[col]], na.rm = TRUE)
      } else if (method_missing == "median") {
        dataset[[col]][missing_indices] <- median(dataset[[col]], na.rm = TRUE)
      }
    }
  }
  return(dataset)
}

# Helper function to handle outliers
handle_outliers <- function(dataset, numeric_columns, method_outliers) {
  if (method_outliers == "none") {
    return(dataset)
  }

  for (col in names(dataset)[numeric_columns]) {
    Q1 <- quantile(dataset[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(dataset[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR

    if (method_outliers == "remove") {
      dataset <- dataset[dataset[[col]] >= lower_bound & dataset[[col]] <= upper_bound, ]
    } else if (method_outliers == "replace") {
      dataset[[col]][dataset[[col]] < lower_bound | dataset[[col]] > upper_bound] <- NA
    }
  }
  return(dataset)
}

# Helper function to scale numeric columns
scale_numeric_columns <- function(dataset, numeric_columns) {
  dataset[numeric_columns] <- lapply(dataset[numeric_columns], scale)
  return(dataset)
}

# Helper function to remove constant columns
remove_constant_columns <- function(dataset) {
  dataset <- dataset[, sapply(dataset, function(col) length(unique(col)) > 1)]
  return(dataset)
}
