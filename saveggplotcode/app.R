library(shiny)
library(ggplot2)

source("helpers.R")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title", "My plot"),
      numericInput("num", "Point size", 5),
      colourpicker::colourInput("col", "Colour", "yellow")
    ),
    mainPanel(
      verbatimTextOutput("code"),
      plotOutput("plot")
    )
  )
)


server <- function(input, output, session) {
  
  plotcode <- reactive({
    fontsize <- 30
    p <- sourceable()
    p <- p + ggplot(mtcars, aes(mpg, wt)) + geom_point(size = input$num)
    p <- p + geom_line(col = input$col)
    p <- p + theme_bw(fontsize)
    p <- p + ggtitle(input$title)
    p <- add_source_dep(p, c("fontsize"))
    
    p
  })
  
  output$code <- renderText({
    decorate_source(plotcode())
  })
  output$plot <- renderPlot({
    run_source(plotcode())
  })
}


shinyApp(ui = ui, server = server)