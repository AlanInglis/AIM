

#' Plot Average Decoded Values
#'
#' Creates a bar plot visualization showing the average decoded values over all selected digits
#'
#' @param data A data frame created from \code{vimpDecoded} function.
#' @param colours A character vector of length 2 specifying the gradient colors for the heatmap.
#'   Defaults to \code{c('white', 'red')}. The first element is used for low values, and the second for high values.
#' @param legendName The name to be used for the heatmap legend. Default is 'Avg Error'.
#'
#' @return A ggplot object.
#'
#' @details
#' The function calculates the average importance across all dimensions.
#'
#'
#' @import ggplot2
#' @export

plotAvgDecoded <- function(data, colours = c('white', 'red'), legendName = 'Avg Error'){

  # Aggregate data to calculate mean importance for each EncodedDimension
  average_importance <- aggregate(Importance ~ EncodedDimension, data = data, mean)


  p <- ggplot(average_importance, aes(x = as.factor(EncodedDimension), y = Importance, fill = Importance)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(
      low = colours[1],
      high = colours[2],
      name = legendName,
      guide = guide_colorbar(
        frame.colour = "black",
        ticks.colour = "black"
      )
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 55, hjust = 0.5)) +
    labs(
      title = "Average Importance of Each Decoded Dimension",
      x = "Encoded Dimension",
      y = "Importance"
    )
  return(p)
}
