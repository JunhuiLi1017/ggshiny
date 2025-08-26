library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Simple Plot Test"),
  mainPanel(
    plotOutput("myplot"),
    textOutput("status")
  )
)

server <- function(input, output, session) {
  output$status <- renderText({
    "App is running - checking plot display"
  })
  
  output$myplot <- renderPlot({
    # Very simple plot
    plot(mtcars$wt, mtcars$mpg, main="Simple Plot Test", xlab="Weight", ylab="MPG")
  })
}

shinyApp(ui = ui, server = server)
