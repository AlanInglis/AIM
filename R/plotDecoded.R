#' Plot Feature Importance for Each Decoded Dimension
#'
#' @description
#' Creates a series of plots, displayed on a grid,
#' showing the feature importance for each dimension of the decoded data.
#'
#' @details
#' The function generates a grid of plots, one for each
#' digit showing the feature importance for each dimension of the decoded data.
#'
#' @param data A data frame created from \code{vimpDecoded} function.
#' @param legendName The name to be used for the heatmap legend. Default is 'Error'.
#' @param colours The selected colours to display the heatmap. First value corresponds to
#' low values
#'
#' @return A grid of plots, each representing the feature importance for each
#'         dimension of each digit.
#'
#' @import ggplot2
#'
#' @export


plotDecoded <-function(data){

    #  plot
    p <- ggplot(data, aes(x = as.factor(EncodedDimension), y = Importance, fill = Importance)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(
        low = "steelblue",
        high = "red",
        name = "error",
        guide = guide_colorbar(
          frame.colour = "black",
          ticks.colour = "black"
        )
      ) +
      facet_wrap(~Digit) +
      # facet_wrap(~ Digit, scales = "free_y") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 55, hjust = 0.5)) +
      labs(
        title = "Importance of Each Encoded Dimension by Digit",
        x = "Encoded Dimension",
        y = "Importance"
      )
    return(p)
}
