# Example app that shows how to use "sourceable" to keep track of a ggplot's
# source code

library(shiny)
library(ggplot2)

source("helpers.R")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title", "My plot"),
      numericInput("num", "Point size", 5),
      colourpicker::colourInput("col", "Colour", "yellow",
                                showColour = "background")
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
    
    # All you need to do is wrap the initial ggplot() call inside sourceable()
    p <- sourceable(ggplot(values$data, aes(mpg, wt)))
    p <- p + geom_point(size = input$num)
    p <- p + geom_line(col = input$col)
    p <- p + ggtitle(input$title)
    p <- p + theme_bw(fontsize)
    
    # If there are any dependencies (variables) that are used in the source code
    # and you want to show them, you can add them with the "attach_source_dep"
    # function. Try removing this line, and you'll see that the fontsize does
    # not get printed. It can be a good idea to attach most variables you use
    # as dependencies to show in the code, but you probably shouldn't attach
    p <- attach_source_dep(p, c("fontsize"))
    
    p
  })
  
  output$code <- renderText({
    # To access the plot's source code, call the "get_source_code" function
    get_source_code(plotcode())
  })
  output$plot <- renderPlot({
    # To visualize the plot, just print it as usual
    plotcode()
  })
}


shinyApp(ui = ui, server = server)