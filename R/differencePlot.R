#' Plot Difference in Importance Between Two Digits
#'
#' This function takes a data frame containing encoded dimensions and their importance values, along with two specified digits.
#' It plots the difference in importance between these two digits based on the encoded dimensions.
#'
#' @param decocdedVimp A data frame containing encoded dimensions and their importance values. Expected columns include 'Digit' and 'Importance'.
#' @param digit1 The first digit for comparison (should be between 0 and 9).
#' @param digit2 The second digit for comparison (should be between 0 and 9 and different from digit1).
#'
#' @return A ggplot object representing the difference in importance between the two specified digits.
#' @examples
#' \dontrun{
#' plot <- plotDifference(decocdedVimp, 3, 8)
#' plot(plot) # Display the plot for digits 3 and 8
#'
#' # For another pair of digits, say 1 and 4
#' plot_1_4 <- plotDifference(decocdedVimp, 1, 4)
#' plot(plot_1_4)
#'}
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual labs theme_bw
#' @importFrom dplyr filter
#' @export

plotDifference <- function(decocdedVimp, digit1, digit2){

  if (!digit1 %in% 0:9 || !digit2 %in% 0:9) {
    stop("Please select two digits between 0 and 9.")
  }

  if (digit1 == digit2) {
    stop("Please select two different digits.")
  }

  # Filter data for the selected digits
  data_digit1 <- filter(decocdedVimp, Digit == digit1)
  data_digit2 <- filter(decocdedVimp, Digit == digit2)

  # Calculate differences
  difference_data <- data.frame(
    EncodedDimension = data_digit1$EncodedDimension,
    ImportanceDifference = data_digit1$Importance - data_digit2$Importance
  )

  p <- ggplot(difference_data, aes(x = as.factor(EncodedDimension),
                                   y = ImportanceDifference,
                                   fill = ImportanceDifference > 0)) +
    geom_bar(stat = "identity", position = 'dodge') +
    scale_fill_manual(values = c("TRUE" = "firebrick", "FALSE" = "steelblue")) +
    labs(x = "Encoded Dimension", y = "Importance Difference",
         title = paste("Difference in Importance for Digits", digit1, "and", digit2)) +
    theme_bw() +
    theme(legend.position = 'none')

  return(p)
}
















