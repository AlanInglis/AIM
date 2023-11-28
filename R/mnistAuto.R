#' Train an Autoencoder on MNIST Data
#'
#' This function trains an autoencoder neural network on the MNIST dataset.
#' It can optionally focus on a specific digit (0-9).
#'
#' @param numSel An optional integer (0-9) to filter the dataset to a specific
#' digit. If NULL (default), the function uses the entire MNIST dataset.
#' @param epochs An integer specifying the number of epochs for training the
#' autoencoder. Default is 5. Must be a positive number.
#' @param verbose A logical (TRUE/FALSE) indicating whether to display detailed
#' training progress. Default is FALSE.
#'
#' @return A list containing the following components:
#' \itemize{
#'   \item{"encoder"}{An encoder model.}
#'   \item{"testData"}{Test dataset (features).}
#'   \item{"testLabel"}{Test dataset (labels).}
#'   \item{"trainLabel"}{Training dataset (labels).}
#'   \item{"trainData"}{Training dataset (features).}
#'   \item{"autoencoder"}{The trained autoencoder model.}
#' }
#'
#' @details The function builds an autoencoder neural network using the MNIST data.
#'
#' @examples
#' # Train with default settings on entire MNIST dataset
#' result <- mnistAuto()
#'
#' # Train focusing only on the digit 1 for 10 epochs
#' result <- mnistAuto(numSel = 1, epochs = 10)
#'
#' @importFrom keras layer_input layer_dense keras_model
#' @import keras$optimizers$legacy$Adam
#' @export

mnistAuto <- function(numSel = NULL, epochs = 5, verbose = FALSE) {

  # Checks ------------------------------------------------------------------

  if (!is.null(numSel) && !(numSel %in% 0:9)) {
    stop("numSel must be a digit between 0 and 9 or NULL.")
  }

  if (epochs <= 0) {
    stop("The number of epochs must be a positive number.")
  }

  # Get the Data ------------------------------------------------------------

  # Load Data
  cat("Loading Data \n")
  mnist <- dataset_mnist()
  x_train <- mnist$train$x
  y_train <- mnist$train$y
  x_test <- mnist$test$x
  y_test <- mnist$test$y


  if (!is.null(numSel)) {
    # Filter for selected number
    train_indices <- which(y_train == numSel)
    test_indices <- which(y_test == numSel)

    x_train <- x_train[train_indices, , ]
    y_train <- y_train[train_indices]
    x_test <- x_test[test_indices, , ]
    y_test <- y_test[test_indices]
  }


  # Normalise data
  x_train <- array_reshape(x_train / 255, c(nrow(x_train), 784))
  x_test <- array_reshape(x_test / 255, c(nrow(x_test), 784))



  # Autoencoder Set-up ------------------------------------------------------

  # Encoder Code ------------------------------------------------------------
  # Set the number of dimensions in the encoded (compressed) representation
  encoding_dim <- 32

  # Define the input layer of the neural network with shape 784 (28x28)
  input_img <- layer_input(shape = c(784))


  # reduce dimensionality to size of 'encoding_dim', using ReLU activation
  encoded <- input_img %>%
    layer_dense(units = encoding_dim, activation = "relu")

  # bring dimensionality back up to 784, using sigmoid activation
  decoded <- encoded %>%
    layer_dense(units = 784, activation = "sigmoid")

  # Create the autoencoder model by specifying input and output layers
  autoencoder <- keras_model(inputs = input_img, outputs = decoded)

  # Compile the autoencoder model
  autoencoder %>% compile(
    optimizer = keras$optimizers$legacy$Adam(learning_rate = 0.01),
    loss = "binary_crossentropy"
  )

  # Train the autoencoder using:
  # - Validation data, which also uses the same input and output format as training data
  cat("Training Model \n")
  history <- autoencoder %>% fit(
    x_train, x_train,
    epochs = epochs,
    batch_size = 256,
    validation_data = list(x_test, x_test),
    verbose = verbose
  )
  cat("Training Completed \n")

  # Create an encoder model by specifying input of the autoencoder and encoded output
  encoder <- keras_model(inputs = autoencoder$input, outputs = encoded)

  myList <- list(
    "encoder" = encoder,
    "testData" = x_test,
    "testLabel" = y_test,
    "trainLabel" = y_train,
    "trainData" = x_train,
    "autoencoder" = autoencoder
  )
  return(myList)
}
