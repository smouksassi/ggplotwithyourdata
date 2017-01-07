tagList(
  tags$head(
    tags$link(href = "app.css", rel = "stylesheet")
  ),
  
  navbarPage(
    title = "TTP",
    id = "mainNav",
    inverse = TRUE,
    fluid = FALSE,
    collapsible = TRUE,
    
    tabPanel(
      "How to",
      value = "howto",
      icon = icon("info"),
      class = "fade in",
      
      "[TODO] information goes here"
    ),
    
    tabPanel(
      "Plots",
      value = "plots",
      icon = icon("bar-chart"),
      class = "fade in",
      
      selectInput("reference_treat", "Reference Treatment", width = 500,
                  choices = c("", drug_list), selected = ""),
      selectInput("test_treat", "Test Treatment", width = 500,
                  choices = c("", drug_list), selected = ""),
      plotOutput("tpp_boxplot", width = "100%", height = "500px"),
      div(
        id = "plot-main-opts",
        checkboxInput("opt_overlay", "Overlay data", value = FALSE),
        checkboxInput("opt_median", "Show median", value = FALSE),
        checkboxInput("opt_samplesize", "Show sample size", value = FALSE) 
      )
    ),
    
    tabPanel(
      "Graph Options",
      value = "graph-options",
      icon = icon("cog"),
      class = "fade in",
      
      "[TODO] more plot options"
    )
  )
)