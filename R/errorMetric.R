#' Calculate Error Metrics for Predictions
#'
#' This function computes error metrics between actual and predicted values.
#' It can return either the Mean Squared Error (MSE) or the Root Mean Squared Error (RMSE).
#'
#' @param actual A numeric vector of actual (observed) values.
#' @param predicted A numeric vector of predicted values.
#' @param MSE A logical value indicating the type of error metric to return.
#' If TRUE (default), the function returns the Mean Squared Error (MSE).
#' If FALSE, it returns the Root Mean Squared Error (RMSE).
#'
#' @return A single numeric value representing the error metric.
#' If `MSE` is TRUE, it returns the Mean Squared Error;
#' if `MSE` is FALSE, it returns the Root Mean Squared Error.
#'
#' @examples
#' actual <- c(1, 2, 3, 4, 5)
#' predicted <- c(1.1, 1.9, 3.1, 3.9, 4.8)
#' errorMetric(actual, predicted) # Returns MSE
#' errorMetric(actual, predicted, MSE = FALSE) # Returns RMSE
#'
#' @export

errorMetric <- function(actual, predicted, MSE = TRUE) {
  err <- mean((actual - predicted)^2)
  if (MSE) {
    return(err)
  } else {
    err <- sqrt(err)
    return(err)
  }
}
