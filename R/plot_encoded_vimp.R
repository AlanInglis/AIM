#' Plot Variable Importance for Encoded Dimensions
#'
#' This function creates a ggplot visualisation of the variable importance
#' for encoded dimensions, typically output from the \code{vimp_encoded} function.
#'
#' @param encoded_data A data frame containing the encoded dimension importance data, created from the
#'        \code{vimp_encoded} function. It should have columns: Class, EncodedDimension, and Importance.
#' @param plot_colors A vector of two colors for the gradient fill.
#'        Default is c("steelblue", "red").
#' @param flip If TRUE, then flip the bar plot to be displayed vertically.
#'
#' @return A ggplot object of a facet bar plot for each class showing the importance for each encoded dimension.
#'
#' @details
#' The function creates a faceted bar plot where each facet represents a class.
#' The bars represent the importance of each encoded dimension, with colour
#' intensity indicating the level of importance.
#'
#' @import ggplot2
#'
#'
#' @export


plot_encoded_vimp <- function(encoded_data, plot_colors = c("steelblue", "red"), flip = FALSE) {

  # # Sort data by Importance if the sort argument is TRUE
  # if (sort) {
  #   encoded_data <- encoded_data[order(-encoded_data$Importance), ]
  #   # Reorder the factor levels based on subset
  #   if(flip){
  #     encoded_data$EncodedDimension <- factor(encoded_data$EncodedDimension,
  #                                             levels = rev(encoded_data$EncodedDimension))
  #   }else{
  #     encoded_data$EncodedDimension <- factor(encoded_data$EncodedDimension,
  #                                             levels = encoded_data$EncodedDimension)
  #   }
  # }

  p <- ggplot(encoded_data, aes(x = as.factor(EncodedDimension), y = Importance, fill = Importance)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(
      low = plot_colors[1],
      high = plot_colors[2],
      name =  "Encoded \nDimension \nVimp",
      guide = guide_colorbar(
        frame.colour = "black",
        ticks.colour = "black"
      )
    ) +
    facet_wrap(~Class) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
          axis.text=element_text(size=5)) +
    labs(
      x = "Encoded Dimension",
      y = "Importance"
    )

  if(flip){
    p <- p + coord_flip()
  }

  return(p)
}

# Example usage:
# plot_encoded_vimp(encoded_data, plot_colors = c("blue", "green"), flip = TRUE, sort = TRUE)
