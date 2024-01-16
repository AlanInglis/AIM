#' Plot Feature Importance for Each Dimension
#'
#' @description
#' Creates a series of plots, displayed on a grid,
#' showing the feature importance for each dimension of the encoded data.
#'
#' @details
#' The function generates a grid of plots, one for each
#' dimension of the encoded data, where each plot visualizes the feature importance
#' as a heatmap.
#'
#' @param digitResults A list obtained from the \code{vimpEncoded} function.
#' @param legendName The name to be used for the heatmap legend. Default is 'Error'.
#' @param colours The selected colours to display the heatmap. First value corresponds to
#' low values
#'
#' @return A grid of plots, each representing the feature importance for one
#'         dimension of the encoded data.
#' @examples
#' \dontrun{
#' # Train focusing only on the digit 1 for 10 epochs
#' encoderModel <- mnistAuto(numSel = 1, epochs = 10)
#' vimps <- vimpEncoded(encoderModel)
#' plotEncoded(digitResults = vimps)
#' }
#'
#' @import ggplot2
#' @importFrom cowplot get_legend
#' @importFrom gridExtra arrangeGrob
#' @importFrom gridExtra grid.arrange
#'
#' @export

plotEncoded <- function(digitResults, legendName = 'Error', colours = c('black', 'white')) {

  digit <- digitResults$selectedNumber
  if(length(digit) < 10){
    digit <- paste(digit, collapse = ",")
  }
  digitResults <- digitResults$Vimp

  all_plots <- list()


  # get range for legend
  max_val <- max(sapply(digitResults, function(df) max(df$Freq)))
  min_val <- 0


  for (j in 1:length(digitResults)) {
    feature_importance_df <- digitResults[[j]]

    # create all plots
    p <- ggplot(feature_importance_df, aes(Var1, Var2, fill = Freq)) +
      geom_tile() +
      scale_y_discrete(limits = rev(levels(feature_importance_df$Var2))) +
      scale_fill_gradient(low = colours[1],
                          high = colours[2],
                          limits = c(min_val, max_val),
                          name = legendName,
                          guide = guide_colorbar(
                            order = 1,
                            frame.colour = "black",
                            ticks.colour = "black"
                          ),
                          oob = scales::squish) +
      theme_void() +
      theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) +
     #labs(title = if(length(digit) < 10) as.character(j) else paste0(digit, ':', j)) +
     labs(title = paste0(digit, ':', j)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      coord_fixed()

    all_plots[[j]] <- p
  }


  # Get legend
  legend <- cowplot::get_legend(all_plots[[1]])

  # remoe legend from all plots
  all_plots_no_leg <- lapply(all_plots, function(x) x + theme(legend.position = "none"))


  # plot on a grid
  n <- length(all_plots)
  nRow <- floor(sqrt(n))
  all_plot_grid <- gridExtra::arrangeGrob(grobs=all_plots_no_leg, nrow=nRow)
  suppressMessages(
    gridExtra::grid.arrange(all_plot_grid,
                            legend,
                            ncol = 2,
                            widths = c(10, 1))
  )
}

