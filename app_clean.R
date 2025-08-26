library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Simple Plot App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "X Variable:", choices = names(mtcars), selected = "wt"),
      selectInput("y_var", "Y Variable:", choices = names(mtcars), selected = "mpg"),
      selectInput("geom_type", "Plot Type:", 
                  choices = c("point", "line", "bar"), selected = "point")
    ),
    mainPanel(
      plotOutput("plot"),
      textOutput("status")
    )
  )
)

server <- function(input, output, session) {
  output$status <- renderText({
    paste("Selected: X =", input$x_var, ", Y =", input$y_var, ", Type =", input$geom_type)
  })
  
  output$plot <- renderPlot({
    req(input$x_var, input$y_var, input$geom_type)
    
    if (input$geom_type == "point") {
      ggplot(mtcars, aes(x = .data[[input$x_var]], y = .data[[input$y_var]])) + 
        geom_point() + 
        ggtitle(paste("Plot of", input$y_var, "vs", input$x_var))
    } else if (input$geom_type == "line") {
      ggplot(mtcars, aes(x = .data[[input$x_var]], y = .data[[input$y_var]])) + 
        geom_line() + 
        ggtitle(paste("Plot of", input$y_var, "vs", input$x_var))
    } else if (input$geom_type == "bar") {
      ggplot(mtcars, aes(x = .data[[input$x_var]], y = .data[[input$y_var]])) + 
        geom_bar(stat = "identity") + 
        ggtitle(paste("Plot of", input$y_var, "vs", input$x_var))
    }
  })
}

shinyApp(ui = ui, server = server)
