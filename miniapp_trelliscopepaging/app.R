library(shiny)
library(ggplot2)
library(ggforce)
library(nlme)
dataplot<- Tetracycline1

source("trelliscope_page_nav.R")
ui <- fluidPage(
  tags$head(tags$script(src="trelliscope_page_nav.js")),
    fluidRow(
      column(12,
             br(),
             h4("Use Trelliscope Navigation to change pages,
                the floating menu below let you change Paging Colouring and Grouping")
      )
    ),#fluidRow
  fluidRow(
    column(12,uiOutput("tc_pager_ui")),
    column(12,plotOutput('plot'))
  ),
  fixedPanel(right=20,bottom=0, width=1200,draggable=TRUE,
             wellPanel(
               fluidRow(
                 column(4,
                        selectInput(inputId = "pageby",label = "Page By",
                                    choices = c("Formulation","Subject"))
                 ),
                 column(4,
                        selectInput(inputId = "groupby",label = "Group By",
                                    choices = c("Subject","Formulation"))
                 ),
                 column(4,
                        selectInput(inputId = "colourby",label = "Colour By",
                                    choices = c("Subject","Formulation"))
                 )
               )#fluidRow
             ,style = "opacity: 0.8")#wellpanel
  )#fixedPanel
)#fluidPage

server <- function(input, output, session) {
  rr_input <- reactiveValues(
    page_current=1
  )
  
  tc_data <- reactive({
    dataplot[,input$pageby]<-as.factor(dataplot[,input$pageby])
   d <- dataplot[dataplot[,input$pageby]==
                 unique(dataplot[,input$pageby])[tc_pager_current()]
                 ,]

  })
  
  output$tc_pager_ui <- renderUI({
    npages<- length(unique(dataplot[,input$pageby]))
    return(trelliscopePageNavInput('tc_pager',1,npages))
  })
  observe({
    page_i <- suppressWarnings(as.integer(input$tc_pager))
    rr_page_i <- as.integer(isolate(rr_input$page_current))
    if (!is.na(page_i) && length(page_i) != 0 && length(rr_page_i) != 0 && page_i != rr_page_i)
      isolate(rr_input$page_current <- page_i)
  })
  
  tc_pager_current <- reactive({
    page_i <- as.integer(rr_input$page_current)
    if (length(page_i) == 0 || is.na(page_i))
      page_i <- 1L
    page_i
  })
  
  output$plot = renderPlot({
    
    #colourby<- ifelse(input$colourby=="none",NULL,input$colourby)
    
    facets <- paste('~', input$pageby)

    p <-  ggplot(tc_data(), aes(Time,conc))+
      geom_point(aes_string(colour=input$colourby),size=5,alpha=0.5)+
      geom_line(aes_string(colour=input$colourby,
                           group=input$groupby),size=5,alpha=0.5)+
      labs(colour=input$colourby)+
      scale_color_discrete(drop=FALSE)
    p+ facet_grid(facets,labeller = label_both)+
      theme_bw(base_size = 20)
  })
  
}

shinyApp(ui, server)

