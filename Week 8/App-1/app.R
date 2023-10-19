library(shiny)
library(ggplot2)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  titlePanel("Custom Dataset Viewer"),
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "caption", label = "Custom Caption:", value = "What's your caption?"),
      selectInput(inputId = "dataset", label = "Choose a dataset:", choices = c("iris", "airquality")),
      numericInput(inputId = "obs", label = "Number of observations to view:", value = 5, min = 1),
      checkboxInput(inputId = "show_summary", label = "Show Summary", value = TRUE),
      sliderInput(inputId = "custom_range", label = "Custom Range:", min = 0, max = 10, value = c(2, 8)),
      selectInput(inputId = "plot_variable_x",
                  label = "Select x-axis variable:",
                  choices = NULL),
      selectInput(inputId = "plot_variable_y",
                  label = "Select y-axis variable:",
                  choices = NULL)
    ),
    mainPanel(
      h3(textOutput("caption", container = span)),
      verbatimTextOutput("summary"),
      tableOutput("view"),
      uiOutput("dataset_image"),  # Output: Image based on selected dataset
      plotOutput("plot")  # Output: Plot based on selected variables
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Define image URLs for different datasets
  dataset_images <- list(
    "iris" = "https://gardenerspath.com/wp-content/uploads/2023/03/How-to-Grow-Douglas-Iris-Feature.jpg",
    "airquality" = "https://di-uploads-development.dealerinspire.com/natewadesubaru/uploads/2022/09/ttk-image-engine_05_05-4.png"
  )
  
  # Render the image based on selected dataset
  output$dataset_image <- renderUI({
    dataset <- input$dataset
    if (!is.null(dataset) && dataset %in% names(dataset_images)) {
      image_url <- dataset_images[[dataset]]
      tags$img(src = image_url, width = "300px")
    }
  })
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "iris" = iris,
           "airquality" = airquality)
  })
  
  # Create caption ----
  output$caption <- renderText({
    input$caption
  })
  
  # Generate a summary of the dataset (conditional) ----
  output$summary <- renderPrint({
    if (input$show_summary) {
      dataset <- datasetInput()
      summary(dataset)
    }
  })
  
  # Show the first "n" observations within custom range ----
  output$view <- renderTable({
    dataset <- datasetInput()
    subset(dataset, dataset >= input$custom_range[1] & dataset <= input$custom_range[2])[1:10, ]
  })
  
  # Update variable choices based on selected dataset ----
  observeEvent(input$dataset, {
    dataset <- datasetInput()
    updateSelectInput(
      inputId = "plot_variable_x",
      choices = names(dataset),
      selected = NULL
    )
    updateSelectInput(
      inputId = "plot_variable_y",
      choices = names(dataset),
      selected = NULL
    )
  })
  
  # Generate plot based on selected variables ----
  output$plot <- renderPlot({
    dataset <- datasetInput()
    variable_x <- input$plot_variable_x
    variable_y <- input$plot_variable_y
    
    if (!is.null(variable_x) && !is.null(variable_y)) {
      ggplot(dataset, aes_string(x = variable_x, y = variable_y)) +
        geom_point() +
        labs(x = variable_x, y = variable_y) +
        theme_minimal()
    }
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)