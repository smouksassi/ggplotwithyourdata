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
  
  values <- reactiveValues(data = mtcars)
  
  plotcode <- reactive({
    fontsize <- 30
    p <- sourceable(ggplot(values$data, aes(mpg, wt)))
    p <- p + geom_point(size = input$num)
    p <- p + geom_line(col = input$col)
    p <- p + theme_bw(fontsize)
    p <- add_source_dep(p, c("fontsize"))
    
    p
  })
  
  output$code <- renderText({
    get_sourcecode(plotcode())
  })
  output$plot <- renderPlot({
    plotcode()
  })
}


shinyApp(ui = ui, server = server)