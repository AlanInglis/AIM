#' Variable Importance of Decoded Images in Autoencoder
#'
#' This function calculates the importance of each encoded dimension in an autoencoder model
#' for each digit in the MNIST dataset by using a permutation approach.
#'
#' @param encoderModel A list containing components of the trained autoencoder model.
#'                     This should include the encoder, test data (x_test, y_test), and the autoencoder model itself.
#' @param num_permutations An integer indicating the number of permutations to be used
#'                         for each dimension in the importance calculation. Default is 4.
#' @param encodingDim The number of encoding dimensions.
#' @param errorMetric A character string specifying the metric to be used for calculating error.
#'                    Default is 'MSE' (Mean Squared Error).
#'
#' @return If `plot = TRUE`, returns a ggplot object visualizing the importance of each encoded dimension
#'         for each digit. If `plot = FALSE`, returns a data frame with columns 'Digit', 'EncodedDimension',
#'         and 'Importance'.
#'
#' @examples
#' # Assuming 'ae' is a trained autoencoder model on MNIST data
#' # vimpDecodedNew(ae, num_permutations = 5, plot = TRUE)
#'
#' @importFrom keras layer_input keras_model
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_gradient guide_colorbar theme_bw theme
#'                 element_text labs
#' @export



vimpDecodedNew <- function(encoderModel, num_permutations = 4, encodingDim = 32, errorMetric = 'MSE') {
  # extract info from autoencoder output
  encoder <- encoderModel[[1]]
  x_test <- encoderModel[[2]]
  y_test <- encoderModel[[3]]
  autoencoder <- encoderModel[[6]]
  digits <- encoderModel$selectedNumber
  if(is.null(digits)){
    digits <- c(0:9)
  }

  encoded_input <- keras::layer_input(shape = c(encodingDim))
  decoder_layer <-  autoencoder$layers[[3]]
  decoder <- keras::keras_model(inputs = encoded_input, outputs = decoder_layer(encoded_input))


  permImpInternal <- function(encoder, autoencoder, x_test, y_test, digit, num_permutations = 4, em = errorMetric) {
    digit_indices <- which(y_test == digit)
    digit_x_test <- x_test[digit_indices, ]

    # original
    original_encoded_imgs <- predict(encoder, digit_x_test, verbose = F)
    decoded_imgs <- predict(decoder, original_encoded_imgs, verbose = F)

    varimp <- matrix(0, nrow = encodingDim, ncol = 1)

    for (i in 1:encodingDim) {
     # print(paste0("Encoded dimension ", i, " computed"))


      for (n in 1:num_permutations) {
        # OG
        permuted_encoded_imgs <- original_encoded_imgs

        # Permute
        permuted_encoded_imgs[, i] <- sample(permuted_encoded_imgs[, i])

        # Decode permuted encoded images
        permuted_decoded_imgs <- predict(decoder, permuted_encoded_imgs, verbose = F)

        # error
        varimp[i] <- varimp[i] + errorMetric(decoded_imgs, permuted_decoded_imgs, metric = em)
      }
    }
    # Average importance over the number of permutations
    varimp <- varimp / num_permutations
    return(varimp)
  }

  # progress bar setup
  pb <- txtProgressBar(min = 0, max = length(digits), style = 3)

  all_digits_importance <- list()
  for (digit_idx in 1:length(digits)) {
    digit <- digits[digit_idx]
     # Update progress bar
    setTxtProgressBar(pb, digit_idx)
    #print(paste0("Running digit ", digit))

    digit_importance <- permImpInternal(encoder, autoencoder, x_test, y_test, digit)
    all_digits_importance[[as.character(digit)]] <- digit_importance
  }

  close(pb)


  importance_df <- data.frame(
    Digit = rep(digits, each = encodingDim),
    EncodedDimension = rep(1:encodingDim, times = length(digits)),
    Importance = unlist(all_digits_importance)
  )

    return(importance_df)
}




