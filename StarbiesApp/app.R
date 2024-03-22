library(shiny)
library(ggplot2)
library(shinythemes)

citystarbies <- read.csv("Starbucks in California - City Stats.csv")
worldstarbies <- read.csv("Starbucks World Stats.csv")

# Define UI for dataset viewer app ----
ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Glimpse into Starbucks... America vs the World"),
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "caption", label = "Title:", value = "What does the data look like?"),
      uiOutput("dataset_image"),  # Output: Image based on selected dataset
      selectInput(inputId = "dataset", label = "Have A Look?", choices = c("citystarbies","worldstarbies")),
      selectInput(inputId = "plot_variable_x",
                  label = "Select x-axis variable:",
                  choices = NULL),
      selectInput(inputId = "plot_variable_y",
                  label = "Select y-axis variable:",
                  choices = NULL)
    ),
    mainPanel(
      h3(textOutput("caption", container = span)),
      plotOutput("plot", width = "1000px", height = "800px") # Output: Plot based on selected variables
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Define image URLs for different datasets
  dataset_images <- list(
    "citystarbies" = "https://i.pinimg.com/474x/6f/50/a9/6f50a9cf3175e6c6c38410c6edd4b7fc.jpg",
    "worldstarbies" = "https://i.pinimg.com/474x/4d/a2/5f/4da25faee1ab0c94247a99acf94afbe0.jpg"
  )
  
  # Render the image based on selected dataset
  output$dataset_image <- renderUI({
    dataset <- input$dataset
    if (!is.null(dataset) && dataset %in% names(dataset_images)) {
      image_url <- dataset_images[[dataset]]
      tags$img(src = image_url, width = "230px")
    }
  })
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "citystarbies" = citystarbies,
           "worldstarbies" = worldstarbies)
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
  
  # Update variable choices based on selected dataset ----
  observeEvent(input$dataset, {
    dataset <- datasetInput()
    updateSelectInput(
      inputId = "plot_variable_x",
      choices = names(dataset),
      selected = NULL
    )
    if (input$dataset == "citystarbies") {
      updateSelectInput(
        inputId = "plot_variable_x",
        choices = c("Percentage.of.white.population","Median.Household.Income","Median.Age","City", "County"),
        selected = NULL
      )}
    if (input$dataset == "worldstarbies") {
      updateSelectInput(
        inputId = "plot_variable_x",
        choices = c("Country","Number.of.Starbucks","Starbucks.per.million.inhabitants"),
        selected = NULL
      )}
    updateSelectInput(
      inputId = "plot_variable_y",
      choices = names(dataset),
      selected = NULL
    )
  }
  )
  
  # Generate plot based on selected variables ----

  output$plot <- renderPlot({
    dataset <- datasetInput()
    variable_x <- input$plot_variable_x
    variable_y <- input$plot_variable_y
    
    if (!is.null(variable_x) && !is.null(variable_y)) {
      ggplot(dataset, aes_string(x = variable_x, y = variable_y)) +
        geom_point() +
        labs(x = variable_x, y = variable_y) +
        theme_minimal() +
        geom_smooth(method = "lm")
    }
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)