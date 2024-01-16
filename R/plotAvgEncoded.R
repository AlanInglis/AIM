#' Plot Average Encoded Values
#'
#' Creates a heatmap visualization showing the average encoded values for a given digit.
#'
#' @param digitResults A list containing the selected digit/s and its/their corresponding importance values.
#' @param colours A character vector of length 2 specifying the gradient colors for the heatmap.
#'   Defaults to \code{c('white', 'red')}. The first element is used for low values, and the second for high values.
#' @param legendName The name to be used for the heatmap legend. Default is 'Avg Error'.
#'
#' @return A ggplot object representing the heatmap.
#'
#' @details
#' The function calculates the average importance across all dimensions and creates a heatmap.
#' The heatmap represents the average permutation importance for the selected digit,
#' with each pixel's importance depicted.
#'
#' @examples
#' \dontrun{
#' # Train focusing only on the digit 1 for 10 epochs
#' encoderModel <- mnistAuto(numSel = 1, epochs = 10)
#' vimps <- vimpEncoded(encoderModel)
#' plotAvgEncoded(digitResults = vimps)
#'}
#' @import ggplot2
#' @export
#'
#'



plotAvgEncoded <- function(digitResults, colours = c('white', 'red'), legendName = 'Avg Error') {
  # Ensure that each element in digitResults is a data frame
  if (!all(sapply(digitResults$Vimp, is.data.frame))) {
    stop("All elements in digitResults must be data frames")
  }


  digit <- digitResults$selectedNumber
  if(is.null(digit)){
    plot_title <- paste0("Average Permutation Importance Over All Digits ")
  }else{
    num_string <- paste(digit, collapse = ", ")
    plot_title <- paste0("Average Permutation Importance For Digit(s) ", num_string)
  }

  digitResults <- digitResults$Vimp


  # Calculate the average importance across all dimensions
  suppressWarnings(
    combined_df <- Reduce("+", digitResults) / length(digitResults)
  )
  combined_df$Var1 <- digitResults[[1]]$Var1
  combined_df$Var2 <- digitResults[[1]]$Var2

  # Create the plot
  p <- ggplot(combined_df, aes(Var1, Var2, fill = Freq)) +
    geom_tile() +
    scale_y_discrete(limits = rev(levels(combined_df$Var2))) +
    scale_fill_gradient(low = colours[1],
                        high = colours[2],
                        name = legendName,
                        guide = guide_colorbar(
                          order = 1,
                          frame.colour = "black",
                          ticks.colour = "black"
                        ),
                        oob = scales::squish) +
    theme_void() +
    labs(title = plot_title,
         x = "", y = "") +
    coord_fixed()

  return(p)
}
