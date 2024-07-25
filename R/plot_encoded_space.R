#' Plot Encoded Space
#'
#' This function plots the encoded space of images using either standard 2D plotting, t-SNE, or UMAP for dimensionality reduction.
#'
#' @param encoder A trained encoder model to generate the encoded space.
#' @param type The type of plot to generate. Options are 'STD' (standard deviation-based plot) , "TSNE" (t-SNE), or "UMAP" (UMAP).
#'             Default is "std".
#' @param test_data A matrix or data frame of test data to be encoded.
#' @param perplexity Perplexity parameter for t-SNE. Default is 30.
#' @param tsneDims Number of dimensions for t-SNE. Should be either 2 or 3. Default is 2.
#' @param verbose Logical. If TRUE, t-SNE will print progress messages. Default is TRUE.
#'
#' @return A ggplot object visualising the encoded space.
#'
#' @import ggplot2
#' @importFrom Rtsne Rtsne
#' @importFrom umap umap
#'
#'
#' @export

plot_encoded_space <- function(encoder,
                               type = "STD",
                               test_data,
                               perplexity = 30,
                               tsneDims = 2,
                               verbose = TRUE) {

  # Convert type to uppercase for case-insensitive comparison
  type <- toupper(type)

  # Check if the type is valid
  if (!type %in% c("STD", "TSNE", "UMAP")) {
    stop("Invalid type. Please use 'STD', 'TSNE', or 'RMSE'.")
  }


  if (tsneDims > 3) {
    stop("tsneDims should be either 2 or 3")
  }

  # encode images
  encoded_imgs <- predict(encoder, test_data, verbose = F)

  if (type == "STD") {
    # Plot Encoded Space
    col_sds <- apply(encoded_imgs, 2, "sd")
    o <- order(col_sds, decreasing = TRUE)
    encoded_df <- as.data.frame(encoded_imgs[, o[1:2]])
    encoded_df$digit <- y_test

    p <- ggplot(encoded_df, aes(x = V1, y = V2, color = as.factor(digit))) +
      geom_point() +
      scale_color_discrete(name = "Digit") +
      labs(
        title = "2D Visualization of Encoded Space",
        x = paste("Dimension", o[1]),
        y = paste("Dimension", o[2])
      ) +
      theme_bw()
  } else if (type == "TSNE") {
    # Use t-SNE for dimensionality reduction
    tsne_results <- Rtsne::Rtsne(as.data.frame(encoded_imgs),
      dims = tsneDims,
      perplexity = perplexity,
      verbose = verbose
    )

    # Create a data frame for ggplot
    tsne_data <- data.frame(
      X = tsne_results$Y[, 1],
      Y = tsne_results$Y[, 2],
      Label = y_test
    )

    # Plot using ggplot2
    p <- ggplot(tsne_data, aes(
      x = X,
      y = Y,
      color = as.factor(Label),
      label = as.factor(Label)
    )) +
      # geom_point() +
      geom_text(alpha = 0.5) +
      theme_bw() +
      labs(color = "Digit") +
      theme(legend.position = "none")
  } else if (type == "UMAP") {
    df <- as.data.frame(encoded_imgs)

    # Perform UMAP dimensionality reduction
    umap_results <- umap::umap(df)

    # Convert UMAP results to a data frame
    umap_data <- as.data.frame(umap_results$layout)
    umap_data$digit <- y_test

    # Plot the UMAP results
    p <- ggplot(umap_data, aes(x = V1, y = V2, label = as.factor(digit), color = as.factor(digit))) +
      # geom_point(alpha = 0.5) +
      geom_text(alpha = 0.5) +
      labs(
        title = "",
        x = "UMAP 1",
        y = "UMAP 2",
        color = "Digit"
      ) +
      theme_bw() +
      theme(legend.position = "none")
  }
  return(p)
}
