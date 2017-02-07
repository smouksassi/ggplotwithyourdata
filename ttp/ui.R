tagList(
  shinyjs::useShinyjs(),
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
      
      includeMarkdown(file.path("text", "howto.md"))
    ),
    
    tabPanel(
      "Plots",
      value = "plots",
      icon = icon("bar-chart"),
      class = "fade in",
      
      tags$strong("Reference Treatment"),
      selectInput("reference_treat", NULL, width = 500,
                  choices = c("", ref_drug_list), selected = ""),
      tags$strong("Test Treatment"),
      checkboxInput("upload_custom", "Upload TTP data file for test treatment",
                    FALSE),
      conditionalPanel("input.upload_custom",
                       fileInput("custom_file", NULL),
                       shinyjs::hidden(div(id = "upload_error"))
      ),
      selectInput("test_treat", NULL, width = 500,
                  choices = c("", ref_drug_list), selected = ""),
      div(
        id = "plot-main-opts",
        checkboxInput("opt_overlay", "Overlay data", value = FALSE),
        checkboxInput("opt_median", "Show median", value = FALSE),
        checkboxInput("opt_samplesize", "Show sample size", value = FALSE) 
      ),
      plotOutput("tpp_boxplot", width = "100%", height = "500px")
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