#' Plot Feature Importance for Each Dimension
#'
#' @description
#' Creates a series of plots, displayed on a grid,
#' showing the feature importance for each dimension of the encoded data.
#'
#' @details
#' The function generates a grid of plots, one for each
#' dimension of the encoded data, where each plot visualises the feature importance
#' as a heatmap.
#'
#' @param input_vimp A list obtained from the \code{vimp_input} function.
#' @param sort Logical. If TRUE, sorts the input importance plots based on their value range. Default is FALSE.
#' @param colors A vector of two colours for the gradient scale. Default is c("white", "red").
#' @param flip_horizontal Logical. If TRUE, flips the plot horizontally. Default is FALSE.
#' @param flip_vertical Logical. If TRUE, flips the plot vertically. Default is FALSE.
#' @param rotate Integer. Specifies the rotation of the plot. 0 = no rotation, 1 = 90 degrees, 2 = 180 degrees, 3 = 270 degrees.
#' Default is 0.
#'
#' @return A grid of plots, each representing the feature importance for one
#'         dimension of the encoded data.
#'
#' @import ggplot2
#' @importFrom gridExtra arrangeGrob grid.arrange
#' @importFrom scales squish
#'
#'
#'
#' @export

plot_input <- function(input_vimp,
                       sort = FALSE,
                       colors = c("white", "red"),
                       flip_horizontal = FALSE,
                       flip_vertical = FALSE,
                       rotate = 0
) {
  if (sort) {
    # remove empty dfs from encoded vimps
    # Access list of data frames
    vimp_dfs <- input_vimp$Vimp

    # Identify dfs with only zeros in 'Value'
    dfs_to_keep <- sapply(vimp_dfs, function(df) {
      if ("Value" %in% names(df)) {
        return(any(df$Value != 0)) # Return TRUE if there's any non-zero value
      } else {
        return(TRUE) # Keep the data frame if it doesn't have 'Value' column
      }
    })

    # Remove dfs with only zeros
    vimp_dfs <- vimp_dfs[dfs_to_keep]

    # Update the original list
    input_vimp$Vimp <- vimp_dfs

    # Calculate range for each data frame
    ranges <- sapply(input_vimp$Vimp, function(df) {
      if ("Value" %in% names(df)) {
        return(max(df$Value, na.rm = TRUE) - min(df$Value, na.rm = TRUE))
      } else {
        return(0) # Assign 0 range for data frames without 'Value' column
      }
    })

    # Sort the dfs by ranges
    sorted_indices <- order(ranges, decreasing = TRUE)
    sorted_dfs <- input_vimp$Vimp[sorted_indices]

    # Update the original list
    input_vimp$Vimp <- sorted_dfs
  }

  digit <- input_vimp$selectedNumber
  if (length(digit) < 10) {
    digit <- paste(digit, collapse = ",")
  }
  input_vimp <- input_vimp$Vimp
  nam <- names(input_vimp)

  all_plots <- list()

  # get range for legend
  max_val <- max(sapply(input_vimp, function(df) max(df$Value)))
  min_val <- 0

  for (j in 1:length(input_vimp)) {
    feature_importance_df <- input_vimp[[j]]

    # Create the plot
    p <- ggplot(feature_importance_df, aes(Row, Col, fill = Value)) +
      geom_tile() +
      scale_y_discrete(limits = rev(levels(feature_importance_df$Col))) +
      scale_fill_gradient(
        low = colors[1],
        high = colors[2],
        limits = c(min_val, max_val),
        name = 'Input \nImportance',
        guide = guide_colorbar(
          order = 1,
          frame.colour = "black",
          ticks.colour = "black"
        ),
        oob = scales::squish
      ) +
      theme_void() +
      theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
      labs(title = paste0(nam[j])) +
      theme(plot.title = element_text(hjust = 0.5)) +
      coord_fixed()

    # Apply rotations and flips
    flip_x <- FALSE
    flip_y <- FALSE

    if (rotate == 1) {
      p <- suppressMessages(p + coord_flip())
      flip_x <- !flip_x
      if (flip_horizontal) flip_y <- !flip_y
      if (flip_vertical) flip_x <- !flip_x
    } else if (rotate == 2) {
      flip_x <- !flip_x
      flip_y <- !flip_y
      if (flip_horizontal) flip_x <- !flip_x
      if (flip_vertical) flip_y <- !flip_y
    } else if (rotate == 3) {
      p <- suppressMessages(p + coord_flip())
      flip_y <- !flip_y
      if (flip_horizontal) flip_y <- !flip_y
      if (flip_vertical) flip_x <- !flip_x
    } else {
      if (flip_horizontal) flip_x <- !flip_x
      if (flip_vertical) flip_y <- !flip_y
    }

    if (flip_x) p <- suppressMessages(p + scale_x_reverse())
    if (flip_y) p <- suppressMessages(p + scale_y_reverse())

    all_plots[[j]] <- p
  }




  # Function to extract all legends from a plot
  get_all_legends <- function(plot) {
    g <- ggplotGrob(plot)
    legends <- g$grobs[grep("guide-box", sapply(g$grobs, function(x) x$name))]
    return(legends)
  }

  # Extract legends from the first plot
  legends <- get_all_legends(all_plots[[1]])

  # Remove legend from all plots
  all_plots_no_leg <- lapply(all_plots, function(x) x + theme(legend.position = "none"))

  # Plot on a grid
  n <- length(all_plots)
  nRow <- floor(sqrt(n))
  all_plot_grid <- gridExtra::arrangeGrob(grobs = all_plots_no_leg, nrow = nRow)

  # Arrange the plots and legends in a grid
  suppressMessages(
    gridExtra::grid.arrange(
      all_plot_grid,
      do.call(gridExtra::arrangeGrob, c(legends, ncol = 1)),
      ncol = 2,
      widths = c(10, 1)
    )
  )
}
