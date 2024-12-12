# Autoencoder Importance Mapping (aim) Package

## Installation

You can install the package directly from GitHub using the `devtools` package:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install the aim package
devtools::install_github("AlanInglis/AIM")
```

### Overview

The AutoEncoder Importance Mapping (aim) package provides tools to enhance the interpretability of autoencoder (AE) models. It includes methods for assessing the importance of input pixels and latent space dimensions, as well as comprehensive visualisation tools to present the findings.

#### Features

**1. Input Pixel Importance Method**

This method measures the importance of input pixels on the latent space by permuting the values of each pixel in the input data and observing the impact on the AE’s performance. Additionally, a regression model is fitted for each pixel against the encoded dimension to determine the directional importance by examining the sign of the slope.

**2. Latent Space Importance Method**

Permutation importance is applied to the latent space data to identify which latent dimensions are crucial for reconstructing images of specific classes. This helps in understanding the role of each latent dimension in the reconstruction process.

**3. Analysis and Visualisation**

The package combines the findings from both the input pixel importance and latent space importance methods into a single visualisation. This comprehensive visualisation allows for a coherent understanding of how individual input pixels and latent dimensions contribute to the AE’s performance and reconstruction accuracy.

### Example Usage

The following code is a quick-start guide demonstarting how to use the `aim` package. 
For a more detailed discussion of the functions and their arguments, see the vignette associated with this package.
In the code, we fist build a simple AE model using the MNIST data. 
We then calculate the importance of both the input pixels and encoded dimesnions and visualise the results.

```r

library(keras)
library(aim)

# set seed
set.seed(1701)
tensorflow::tf$random$set_seed(1701)

# Build Autoencoder using MNIST Data ---------

# Load Data
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y


# Preprocess Data
x_train <- array_reshape(x_train / 255, c(nrow(x_train), 784))
x_test <- array_reshape(x_test / 255, c(nrow(x_test), 784))


# Define Autoencoder Model
encoding_dim <- 12
input_img <- layer_input(shape = c(784))

encoded <- input_img %>%
  layer_dense(units = encoding_dim, activation = 'relu')

decoded <- encoded %>%
  layer_dense(units = 784, activation = 'sigmoid')

autoencoder <- keras_model(inputs = input_img, outputs = decoded)

# Compile and Fit the Model
autoencoder %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy'
)

autoencoder %>% fit(
  x_train, x_train,
  epochs = 50,
  batch_size = 256,
  validation_data = list(x_test, x_test)
)

# Create Encoder Model
encoder <- keras_model(inputs = autoencoder$input, outputs = encoded)


# AIM functions -----------------------------------------------------------

# get pixel importance
pixel_imp <- vimp_input(encoder = encoder, test_data = x_test, num_permutations = 1) # one permutation for speed

# plot pixel importance
plot_input(input_vimp = pixel_imp,
           sort = F,
           colors = c('white', 'red'),
           flip_horizontal = F,
           flip_vertical = T,
           rotate = 0)


# get encoded dimension importance
encoded_imp <- vimp_encoded(encoder = encoder,
                            test_data =  x_test,
                            test_labels = y_test,
                            autoencoder = autoencoder,
                            classes = considered_digits,
                            num_permutations = 1)

# plot ncoded dimension importance
plot_encoded_vimp(encoded_data = encoded_imp, plot_colors = c('steelblue', 'firebrick'), flip = F)

# get directional importance
dir_imp  <- vimp_direction(encoder = encoder,
                           test_data =  x_test)

# plot everything
plot_input_direction(input_vimp = pixel_imp,
                     encoded_vimp = encoded_imp, 
                     direction_vimp = dir_imp,
                     class = 5, 
                     sort = T, 
                     topX = 10,
                     flip_horizontal = F,
                     flip_vertical = T, 
                     rotate = 0,
                     filter_zero = F)


# shiny app
shiny_vimp(input_vimp = pixel_imp, encoded_vimp = encoded_imp, direction_vimp = dir_imp)
```


