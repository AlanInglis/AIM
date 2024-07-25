#' Calculate Variable Importance Direction for Encoded Dimensions
#'
#' This function calculates the direction of importance for each input feature
#' with respect to each encoded dimension in an autoencoder model.
#'
#' @param encoder A Keras encoder model.
#' @param test_data A matrix or data frame of test data to be encoded.
#' @param threshold Numeric. If provided, coefficients with absolute value below this threshold will be set to 0. Default is NULL.
#'
#' @return A matrix where each column represents an encoded dimension and each row represents an input feature.
#'         Values are -1 (negative relationship), 0 (no significant relationship), or 1 (positive relationship).
#'
#' @details
#' The function fits a simple linear regression for each pair of input feature and encoded dimension.
#' The sign of the regression coefficient determines the direction of the relationship.
#' If a threshold is provided, weak relationships (as determined by the threshold) are set to 0.
#'
#' @importFrom stats lm predict
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#'
#' @export


vimp_direction <- function(encoder, test_data, threshold = NULL){

  # Encode the test data using the provided encoder
  encoded_imgs <- predict(encoder, test_data, verbose = FALSE)

  # Convert input and encoded data to data frames for easier manipulation
  test_data_df <- as.data.frame(test_data)
  encoded_imgs_df <- as.data.frame(encoded_imgs)

  # Rename columns of encoded data for clarity
  colnames(encoded_imgs_df) <- paste0("encoded_dim_", 1:ncol(encoded_imgs_df))

  # Get the number of encoded dimensions
  edim <- ncol(encoded_imgs_df)

  # Get the number of input features
  data_dim <- ncol(test_data_df)

  # Initialize matrix to hold the linear regression coefficients
  model_summaries <- matrix(0, nrow = data_dim, ncol = edim)

  # Set up the progress bar
  pb <- txtProgressBar(min = 0, max = edim, style = 3)

  # Loop through each encoded dimension
  for (i in 1:edim) {
    setTxtProgressBar(pb, i)

    # For each input feature, fit a linear regression model
    for(j in 1:data_dim){
      # Fit linear regression: Encoded Dimension ~ Input Feature
      model <- lm(encoded_imgs_df[, i] ~ test_data_df[,j])
      # Store the slope coefficient
      model_summaries[j, i] <- model$coefficients[2]
    }
  }

  close(pb)

  # Replace NA values with 0
  model_summaries[is.na(model_summaries)] <- 0

  # Simplify relationships to -1, 0, or 1
  model_summaries_adjusted <- sign(model_summaries)

  # Apply threshold if provided
  if(!is.null(threshold)){
    model_summaries_adjusted[abs(model_summaries_adjusted) < threshold] <- 0
  }

  return(model_summaries_adjusted)
}
