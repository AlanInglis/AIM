#' Plot Importance Direction
#'
#' This function creates visualisations of the encoded dimension importance and the input importance
#' with the direction of the importance highlighted.
#' It generates a bar plot of encoded dimension importance and heatmaps showing pixel importance direction.
#'
#' @param input_vimp A list containing input variable importance data. Created via the \code{vimp_input} function.
#' @param encoded_vimp A data frame with encoded dimension variable importance data. Created via the \code{vimp_encoded} function.
#' @param direction_vimp A matrix with linear model summary data. Created via the \code{vimp_direction} function.
#' @param class The specific class or classes to analyse.
#' @param topX Integer. If provided, limits the visualisation to the top X most important dimensions.
#' @param sort Logical. If TRUE, sorts the importance values. Default is FALSE.
#' @param filter_zero Logical. If TRUE, filters out zero importance values (i.e., unused dimensions). Default is FALSE.
#' @param flip_horizontal Logical. If TRUE, flips the heatmaps horizontally. Default is FALSE.
#' @param flip_vertical Logical. If TRUE, flips the heatmaps vertically. Default is FALSE.
#' @param rotate Integer. Specifies the rotation of the heatmaps. 0 = no rotation, 1 = 90 degrees, 2 = 180 degrees, 3 = 270 degrees. Default is 0.
#' @param show_legend Logical. If TRUE, then show the legend.
#' @return A grid arrangement of plots showing encoded dimension importance and pixel importance direction.
#'
#' @import ggplot2
#' @importFrom gridExtra arrangeGrob grid.arrange
#' @importFrom scales squish
#' @importFrom reshape2 melt
#'
#'
#' @export
plot_input_direction <- function(input_vimp,
                                 encoded_vimp,
                                 direction_vimp,
                                 class,
                                 topX = NULL,
                                 sort = FALSE,
                                 filter_zero = FALSE,
                                 flip_horizontal = FALSE,
                                 flip_vertical = FALSE,
                                 rotate = 0,
                                 show_legend = FALSE) {

  # error check
  if (!(class %in% encoded_vimp$Class)) {
    stop(paste("The selected class", class, "is not found in the data."))
  }

  # Helper function to prepare data
  prepare_data <- function(data, class, topX, sort, filter_zero) {
    data <- subset(data, Class == class)
    if (filter_zero) {
      data <- subset(data, Importance != 0)
    }
    if (sort) {
      data <- data[order(-abs(data$Importance)), ]
    }
    if (!is.null(topX)) {
      data <- head(data, topX)
    }
    return(data)
  }

  # Helper function to create vimp matrix
  create_vimp_matrix <- function(input_vimp, edim) {
    vimp_matrix <- matrix(nrow = nrow(direction_vimp), ncol = edim)
    for (i in 1:edim) {
      vimp_matrix[, i] <- input_vimp$Vimp[[as.character(i)]]$Value
    }
    return(vimp_matrix)
  }

  # Helper function to create decoded plot
  create_decoded_plot <- function(data) {

    # Reorder the factor levels based on subset
    data$EncodedDimension <- factor(data$EncodedDimension,
                                          levels = rev(data$EncodedDimension))


    ggplot(data, aes(
      x = as.factor(EncodedDimension),
      y = Importance,
      fill = Importance
    )) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      scale_fill_gradient(
        low = "steelblue", high = "red",
        name = "Encoded \nDimension \nVimp",
        guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
      ) +
      labs(x = "Encoded Dimension", y = "Importance") +
      theme_bw() +
      theme(legend.position = "none", aspect.ratio = 1)
  }

  # Helper function to create heatmap plots
  create_heatmap_plots <- function(data, myVec, min_val, max_val) {
    plots <- list()
    data_dim <- sqrt(nrow(data))

    for (i in 1:ncol(data)) {
      mat <- matrix(data[, i], ncol = data_dim, nrow = data_dim)
      mat_long <- reshape2::melt(mat)

      p <- ggplot(mat_long, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile() +
        scale_y_discrete(limits = rev(levels(mat_long$Var2))) +
        scale_fill_gradient2(
          low = 'blue', high = 'red', mid = 'white',
         # limits = c(min_val, max_val),
          name = 'Pixel \nVimp \nDirection',
          guide = guide_colorbar(order = 1, frame.colour = "black", ticks.colour = "black"),
          oob = scales::squish
        ) +
        labs(title = paste(myVec[i])) +
        theme_void() +
        theme(
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
          plot.title = element_text(hjust = 0.5), aspect.ratio = 1
        ) +
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

      plots[[i]] <- p
    }

    return(plots)
  }

  # Main function logic
  edim <- max(encoded_vimp$EncodedDimension)
  data_class <- prepare_data(encoded_vimp, class, topX, sort, filter_zero)

  vimp_matrix <- create_vimp_matrix(input_vimp, edim)
  vimp_lm_mat <- vimp_matrix * direction_vimp

  myVec <- data_class$EncodedDimension
  colnames(vimp_lm_mat) <- as.character(1:edim)
  sorted_vimp <- vimp_lm_mat[, as.character(myVec)]

  # Calculate global min and max for consistent color scaling
  all_values <- c(as.vector(sorted_vimp), data_class$Importance)
  min_val <- min(all_values, na.rm = TRUE)
  max_val <- max(all_values, na.rm = TRUE)

  p_dec <- create_decoded_plot(data_class)

  heatmap_plots <- create_heatmap_plots(sorted_vimp, myVec, min_val, max_val)

  # Function to extract the legend from a ggplot object
  get_legend <- function(plot) {
    g <- ggplotGrob(plot)
    legend <- g$grobs[[which(sapply(g$grobs, function(x) x$name) == "guide-box")]]
    return(legend)
  }

  # Extract the legend from the first plot
  legend <- get_legend(heatmap_plots[[1]])

  # Remove the legend from all plots
  heatmap_plots_no_leg <- lapply(heatmap_plots, function(x) x + theme(legend.position = "none"))

  # Arrange the plots in a grid
  n <- length(heatmap_plots_no_leg)
  nRow <- floor(sqrt(n))
  all_plot_grid <- gridExtra::arrangeGrob(grobs = heatmap_plots_no_leg, nrow = nRow)

  # Arrange and display plots
  if(show_legend){
    suppressMessages(
      gridExtra::grid.arrange(
        p_dec, all_plot_grid, legend,
        layout_matrix = rbind(c(1, 2, 3), c(1, 2, 3)),
        widths = c(5, 5, 1)
      )
    )
  }else{
    suppressMessages(
      gridExtra::grid.arrange(
        p_dec, all_plot_grid,
        layout_matrix = rbind(c(1, 2, 3), c(1, 2, 3)),
        widths = c(5, 5, 0.5)
      )
    )
  }

}









