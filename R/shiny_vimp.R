#' Launch Shiny App for Autoencoder Analysis Visualisation
#'
#' This function creates and launches a Shiny application for interactive visualisation
#' of autoencoder analysis results, including variable importance for both the
#' input pixels and the encoded dimension.
#'
#' @param input_vimp A list or data frame containing input variable importance data. Created via the \code{vimp_input} function.
#' @param encoded_vimp A list or data frame containing encoded variable importance data. Created via the \code{vimp_encoded} function.
#' @param direction_vimp A list or data frame containing direction variable importance data. Created via the \code{vimp_direction} function.
#'
#' @return A Shiny app object.
#'
#' @details
#' The app provides interactive controls for:
#' \itemize{
#'   \item Selecting a specific class to analyse
#'   \item Choosing the rotation of the plots
#'   \item Specifying the number of top importances to display
#'   \item Filter Empty Dimensions
#'   \item Sorting by importance
#'   \item Flipping the plots horizontally or vertically
#'   \item Switching between light and dark mode themes.
#' }
#'
#' The main output is a combined plot showing the encoded dimension importance on the left and the input pixel importance
#' (with directional importance highlighted) on the right..
#'
#' @import shiny
#' @importFrom shiny fluidPage titlePanel fluidRow column selectInput numericInput checkboxInput plotOutput renderPlot shinyApp
#'
#'
#' @export


shiny_vimp <- function(input_vimp, encoded_vimp, direction_vimp) {

  # get class choices
  all_classes <- unique(encoded_vimp$Class)

  # UI ----------------------------------------------------------------------

  ui <- fluidPage(
    tags$head(
      tags$style(HTML("
        .light-mode {
          background-color: #f0f0f0;
          color: #000000;
        }
        .dark-mode {
          background-color: #212121;
          color: #ebebeb;
        }
      ")),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('toggle-mode', function(mode) {
          var body = document.body;
          if (mode === 'Dark') {
            body.classList.add('dark-mode');
            body.classList.remove('light-mode');
          } else {
            body.classList.add('light-mode');
            body.classList.remove('dark-mode');
          }
        });
      "))
    ),
    titlePanel("Autoencoder Analysis"),

    # Row for digit selection and threshold
    fluidRow(
      column(
        4,
        selectInput("class", "Select Class:", choices = all_classes, selected = all_classes[1])
      ),
      column(
        4,
        selectInput("rotate", "Select Rotation:", choices = 0:4, selected = 0)
      ),
      column(
        2,
        #offset = 5,
        radioButtons("mode", "Select Mode:", choices = c("Light", "Dark"), selected = "Dark")
      )
    ),

    # Row for additional controls
    fluidRow(
      column(
        4,
        numericInput("topX", "Number of Top Importances:", value = 5, min = 1, max = 100)
      ),
      column(
        4,
        checkboxInput("filter_zero", "Filter Empty Dimensions", value = FALSE)
      ),
      column(
        4,
        checkboxInput("sort", "Sort by Importance", value = FALSE)
      ),
      column(
        4,
        checkboxInput("flip_horizontal", "Flip Horizontal", value = FALSE)
      ),
      column(
        4,
        checkboxInput("flip_vertical", "Flip Vertical", value = FALSE)
      )
    ),

    # Row for the plot
    fluidRow(
      column(
        12,
        plotOutput("combinedPlot", width = "100%", height = "600px")
      )
    )
  )

  # server logic ------------------------------------------------------------

  server <- function(input, output, session) {

    observe({
      session$sendCustomMessage("toggle-mode", input$mode)
    })

    output$combinedPlot <- renderPlot({
      plot_input_direction(
        input_vimp = input_vimp,
        encoded_vimp = encoded_vimp,
        direction_vimp = direction_vimp,
        class =  input$class,
        sort = input$sort,
        topX = input$topX,
        filter_zero = input$filter_zero,
        flip_horizontal = input$flip_horizontal,
        flip_vertical = input$flip_vertical,
        rotate = input$rotate
      )
    })
  }

  # launch ------------------------------------------------------------------

  shinyApp(ui = ui, server = server)
}

