#' Variable Importance for Encoded Autoencoder
#'
#' @description
#' Computes the importance of each feature in the encoding process
#' of the MNIST data set using a provided encoder model. It uses permutation-based
#' importance measurement.
#'
#' @details
#' The function computes the permutation importance
#' by permuting each feature across a specified number
#' of permutations and calculating the impact on the encoding output.
#'
#'
#' @param encoderModel A list obtained from the \code{mnistAuto} function.
#' @param num_permutations The number of permutations to use for calculating
#'        importance. Default is 4.
#' @param metric  Character indicating the type of error metric to return.
#' If \code{metric = 'MSE'}, (default), the function returns the Mean Squared Error (MSE).
#' If \code{metric = 'RMSE'}, it returns the Root Mean Squared Error (RMSE).
#'
#' @return A list containing two elements:
#' \itemize{
#'   \item{"Vimp"}{ - A list of data frames, each representing the feature importance
#'           matrix for one encoded dimension.}
#'   \item{"selectedNumber"}{ - The number selected in the encoder model, if any.}
#' }
#'
#'
#' @examples
#' # Train model using only digit 3 for 10 epochs
#' encoderModel <- mnistAuto(numSel = 3, epochs = 10)
#' # Calculate importance
#' vimpEncoded(encoderModel)
#'
#' @export

vimpEncoded <- function(encoderModel, num_permutations = 4, metric = "MSE") {

  # Messages
  if (is.null(encoderModel$selectedNumber)) {
    message("No number selected. Using full MNIST data set")
  } else {
    digit <- paste(encoderModel$selectedNumber, collapse = ",")
    print(paste0("Number(s) ", digit, " selected."))
  }
  encoder <- encoderModel[[1]] # get encoder model
  x_test <- encoderModel[[2]] # get test data

  # train_indices <- which(y_train %in% 3)
  # test_indices <- which(y_test %in%  3)
  #
  # x_train <- x_train[train_indices, ]
  # y_train <- y_train[train_indices]
  # x_test <- x_test[test_indices,  ]
  # y_test <- y_test[test_indices]

  # Encode the original test data
  original_encoded_imgs <- predict(encoder, x_test, verbose = FALSE)

  # Initialize matrices to store importance
  varimp <- matrix(0, nrow = 784, ncol = 32) # Initialize with 0 for summing

  # Set up the progress bar
  total_iterations <- dim(x_test)[2]
  pb <- txtProgressBar(min = 0, max = total_iterations, style = 3)

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

      for (j in 1:32) {
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

  for (j in 1:32) {
    # Convert to matrix for each dimension
    feature_importance_matrix <- matrix(varimp[, j], nrow = 28, ncol = 28)

    # Convert the matrix to a data frame suitable for ggplot
    feature_importance_df <- as.data.frame(as.table(feature_importance_matrix))
    all_dimensions_importance[[as.character(j)]] <- feature_importance_df
  }

  myList <- list(
    "Vimp" = all_dimensions_importance,
    "selectedNumber" = encoderModel$selectedNumber
  )

  return(myList)
}
