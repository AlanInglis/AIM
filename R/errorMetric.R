#' Calculate Error Metrics for Predictions
#'
#' This function computes error metrics between actual and predicted values.
#' It can return either the Mean Squared Error (MSE) or the Root Mean Squared Error (RMSE).
#'
#' @param actual A numeric vector of actual (observed) values.
#' @param predicted A numeric vector of predicted values.
#' @param metric  Character indicating the type of error metric to return.
#' If \code{metric = 'MSE'} (default), the function returns the Mean Squared Error (MSE).
#' If \code{metric = 'RMSE'}, it returns the Root Mean Squared Error (RMSE).
#'
#' @return A numeric value representing the error metric.
#' If \code{metric = 'MSE'},  it returns the Mean Squared Error;
#' If \code{metric = 'RMSE'}, it returns the Root Mean Squared Error.
#'
#' @examples
#' actual <- c(1, 2, 3, 4, 5)
#' predicted <- c(1.1, 1.9, 3.1, 3.9, 4.8)
#' errorMetric(actual, predicted) # Returns MSE
#' errorMetric(actual, predicted, metric = "RMSE") # Returns RMSE
#'
#' @export

errorMetric <- function(actual, predicted, metric = "MSE") {
  # Convert metric to uppercase for case-insensitive comparison
  metric <- toupper(metric)

  # Check if the metric is valid
  if (!metric %in% c("MSE", "RMSE")) {
    stop("Invalid metric. Please use 'MSE' or 'RMSE'.")
  }

  err <- mean((actual - predicted)^2)

  if (metric == "MSE") {
    return(err)
  } else if (metric == "RMSE") {
    err <- sqrt(err)
    return(err)
  }
}

