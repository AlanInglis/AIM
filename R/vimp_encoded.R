#' Calculate Variable Importance for Encoded Dimensions
#'
#' This function calculates the variable importance for each encoded dimension
#' of an autoencoder model using permutation importance.
#'
#' @param encoder A Keras encoder model.
#' @param test_data A matrix or array of test data.
#' @param test_labels A vector of test labels.
#' @param autoencoder A Keras autoencoder model.
#' @param classes A vector of class labels to calculate importance for.
#' @param num_permutations Integer. Number of permutations for importance calculation. Default is 4.
#' @param errorMetric Character. The error metric to use. Default is "MSE".
#'
#' @return A data frame with columns for Class, EncodedDimension, and Importance.
#'
#' @importFrom keras layer_input keras_model
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#' @export
vimp_encoded <- function(encoder,
                         test_data,
                         test_labels,
                         autoencoder,
                         classes,
                         num_permutations = 4,
                         errorMetric = "MSE") {

  # Ensure test_labels is a vector
  test_labels <- as.vector(test_labels)

  # Get the number of encoded dimensions
  len_output <- length(encoder$output_shape)
  edim <- encoder$output_shape[[len_output]]

  # Create input layer for the encoded representation
  encoded_input <- keras::layer_input(shape = c(edim))

  # # Get the decoder layer from the autoencoder
  # decoder_length <- length(autoencoder$layers)
  # decoder_layer <- autoencoder$layers[[decoder_length]]
  #
  # # Create a decoder model
  # decoder <- keras::keras_model(inputs = encoded_input, outputs = decoder_layer(encoded_input))

  # Identify the index of the last layer in the encoder within the autoencoder
  decoder_start_index <- which(sapply(autoencoder$layers, function(layer) identical(layer$name, encoder$layers[[length(encoder$layers)]]$name))) + 1

  # Ensure the decoder_start_index is valid
  if (length(decoder_start_index) == 0) {
    stop("Could not identify the start of the decoder layers. Ensure the encoder is correctly defined.")
  }

  # Extract decoder layers
  decoder_layers <- autoencoder$layers[decoder_start_index:length(autoencoder$layers)]

  # Rebuild the decoder model
  decoded_output <- encoded_input
  for (layer in decoder_layers) {
    decoded_output <- layer(decoded_output)
  }
  decoder <- keras::keras_model(inputs = encoded_input, outputs = decoded_output)


  # Internal function to calculate permutation importance
  permImpInternal <- function(encoder, test_data, test_labels, class, num_permutations = 4, em = errorMetric) {
    # Find indices of the current class in test_labels
    class_indices <- which(test_labels == class)
    if(length(class_indices) == 0) {
      warning(paste("No instances of class", class, "found in test_labels"))
      return(rep(0, edim))  # Return a vector of zeros if no instances found
    }
    class_test_data <- test_data[class_indices, ]

    # Encode and decode original images
    original_encoded_imgs <- predict(encoder, class_test_data, verbose = F)
    decoded_imgs <- predict(decoder, original_encoded_imgs, verbose = F)

    # Initialize importance matrix
    varimp <- matrix(0, nrow = edim, ncol = 1)

    # Calculate importance for each encoded dimension
    for (i in 1:edim) {
      for (n in 1:num_permutations) {
        # Copy the original encoded data
        permuted_encoded_imgs <- original_encoded_imgs

        # Permute the values in the i-th dimension
        permuted_encoded_imgs[, i] <- sample(permuted_encoded_imgs[, i])

        # Decode permuted encoded images
        permuted_decoded_imgs <- predict(decoder, permuted_encoded_imgs, verbose = F)

        # Calculate error and accumulate importance
        varimp[i] <- varimp[i] + errorMetric(decoded_imgs, permuted_decoded_imgs, metric = em)
      }
    }
    # Average importance over the number of permutations
    varimp <- varimp / num_permutations
    return(varimp)
  }

  # Set up progress bar
  pb <- txtProgressBar(min = 0, max = length(classes), style = 3)

  # Calculate importance for each class
  all_classes_importance <- list()
  for (class_idx in 1:length(classes)) {
    class <- classes[class_idx]
    setTxtProgressBar(pb, class_idx)

    class_importance <- permImpInternal(encoder, test_data, test_labels, class)
    all_classes_importance[[as.character(class)]] <- class_importance
  }

  close(pb)

  # Create a data frame with importance results
  importance_df <- data.frame(
    Class = rep(classes, each = edim),
    EncodedDimension = rep(1:edim, times = length(classes)),
    Importance = unlist(all_classes_importance)
  )

  return(importance_df)
}
