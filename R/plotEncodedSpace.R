#' Plot Encoded Space of Encoder Model
#'
#' This function takes an encoder model and its associated test data to plot the encoded space.
#' It supports both standard deviation-based plotting and t-SNE (t-Distributed Stochastic Neighbor Embedding) for dimensionality reduction.
#'
#' @param encoderModel A list containing the encoder model at the first position and the test data x_test at the second position.
#' @param type Character string specifying the type of plot. 'std' for standard deviation-based plot and 'tsne' for t-SNE based plot. Defaults to 'std'.
#' @param perplexity Numeric. The perplexity parameter used in the t-SNE algorithm. Typically values between 5 and 50 are used. The default is 30.
#' @param tsneDims Integer. The number of dimensions for the t-SNE reduced space. Can be either 2 or 3. The default is 2.
#' @param verbose Logical. If TRUE, additional information will be printed during the execution of t-SNE algorithm. Defaults to TRUE.
#'
#' @return A ggplot object representing the encoded space.
#'
#' @examples
#' \dontrun{
#' plot <- plotEncodedSpace(encoderModel, type = 'std')
#' plot(plot) # To display the plot
#'
#' # For a t-SNE based plot with specific perplexity
#' tsne_plot <- plotEncodedSpace(encoderModel, type = 'tsne', perplexity = 40)
#' plot(tsne_plot)
#'}
#' @importFrom ggplot2 ggplot aes geom_point scale_color_discrete labs theme_bw
#' @importFrom Rtsne Rtsne
#' @export

plotEncodedSpace <- function(encoderModel,
                             type = 'std',
                             perplexity = 30,
                             tsneDims = 2,
                             verbose = TRUE){

  if (tsneDims > 3) {
    stop('tsneDims should be either 2 or 3')
  }


  encoder <- encoderModel[[1]] # get encoder model
  x_test <- encoderModel[[2]]  # get x_test data
  y_test <- encoderModel$testLabel # get y_test data

  # encode images
  encoded_imgs <- predict(encoder, x_test, verbose = F)

  if(type == 'std'){
    # Plot Encoded Space
    col_sds <- apply(encoded_imgs, 2, 'sd')
    o <- order(col_sds, decreasing = TRUE)
    encoded_df <- as.data.frame(encoded_imgs[,o[1:2]])
    encoded_df$digit <- y_test

    p <- ggplot(encoded_df, aes(x = V1, y = V2, color = as.factor(digit))) +
      geom_point() +
      scale_color_discrete(name = "Digit") +
      labs(title = "2D Visualization of Encoded Space",
           x = paste("Dimension",o[1]),
           y = paste("Dimension",o[2])) +
      theme_bw()
  }else if(type == 'tsne'){

    # Use t-SNE for dimensionality reduction
    tsne_results <- Rtsne::Rtsne(as.data.frame(encoded_imgs),
                                 dims = tsneDims,
                                 perplexity = perplexity,
                                 verbose = verbose)

    # Create a data frame for ggplot
    tsne_data <- data.frame(X = tsne_results$Y[,1],
                            Y = tsne_results$Y[,2],
                            Label = y_test)

    # Plot using ggplot2
    p <- ggplot(tsne_data, aes(x = X,
                          y = Y,
                          color = as.factor(Label))) +
      geom_point() +
      theme_bw() +
      labs(color = 'Digit')
  }
  return(p)
}

