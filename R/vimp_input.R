#' Calculate Variable Importance for Encoder Input
#'
#' This function calculates the variable importance for each input feature of an encoder
#' using permutation importance. It permutes each feature multiple times and measures
#' the impact on the encoder's output.
#'
#' @param encoder A Keras encoder model.
#' @param test_data A matrix or data frame containing the test data.
#' @param num_permutations Integer. The number of times to permute each feature. Default is 4.
#' @param metric Character. The error metric to use. Default is "MSE" (Mean Squared Error).
#'
#' @return A list containing:
#'   \item{Vimp}{A list of data frames, each representing the variable importance for one dimension of the encoded output.}
#'
#' @importFrom stats predict
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#'
#' @export


vimp_input <- function(encoder,
                       test_data,
                       num_permutations = 4,
                       metric = "MSE") {

  # error checks
  if(num_permutations < 1){
    stop("The number of permutations must be greater than zero.")
  }

  # Rename test data
  x_test <- test_data

  # get the number of encoded dimensions
  len_output <- length(encoder$output_shape)
  edim <- encoder$output_shape[[len_output]]

  # Get the dimensions of the data
  len_input <- length(encoder$input_shape)
  data_dim <- encoder$input_shape[[len_input]]

  # Encode the original test data
  original_encoded_imgs <- predict(encoder, x_test, verbose = FALSE)

  # Initialize matrices to store importance
  varimp <- matrix(0, nrow = data_dim, ncol = edim) # Initialise with 0 for summing

  # Set up the progress bar
  pb <- txtProgressBar(min = 0, max = data_dim, style = 3)

  # Permutation importance loop
  for (i in 1:dim(x_test)[2]) { # Loop over each feature

    # progress bar
    setTxtProgressBar(pb, i)

    for (n in 1:num_permutations) {
      # Copy the original test data
      permuted_x_test <- x_test

      # Permute the values in the i-th column (feature)
      permuted_x_test[, i] <- sample(permuted_x_test[, i])

      # Encode the permuted data
      permuted_encoded_imgs <- predict(encoder, permuted_x_test, verbose = F)

      for (j in 1:edim) {
        varimp[i, j] <- varimp[i, j] + errorMetric(original_encoded_imgs[, j], permuted_encoded_imgs[, j], metric = metric)
      }
    }
  }

  # Close the progress bar
  close(pb)

  # Average the importance over the number of permutations
  varimp <- varimp / num_permutations

  # Store results in a list
  all_dimensions_importance <- list()
  # get rows and cols
  row_col <- sqrt(data_dim)

  for (j in 1:edim) {
    # Convert to matrix for each dimension
    feature_importance_matrix <- matrix(varimp[, j], nrow = row_col, ncol = row_col)

    # Create a data frame with numeric row and column indices
    feature_importance_df <- expand.grid(
      Row = 1:row_col,
      Col = 1:row_col
    )
    feature_importance_df$Value <- as.vector(feature_importance_matrix)

    all_dimensions_importance[[as.character(j)]] <- feature_importance_df
  }

  # store as a list and return
  myList <- list(
    "Vimp" = all_dimensions_importance
  )

  return(myList)
}








