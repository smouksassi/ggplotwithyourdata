tagList(
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(href = "app.css", rel = "stylesheet")
  ),
  
  navbarPage(
    title = "Plot and Compare TTP",
    id = "mainNav",
    inverse = TRUE,
    fluid = FALSE,
    collapsible = TRUE,
    
    tabPanel(
      "TB Study Description",
      value = "TBStudyDescription",
      icon = icon("info"),
      class = "fade in",
      
      includeHTML(file.path("text", "TBStudyDescription.html"))
    ),
    
    tabPanel(
      "How to",
      value = "howto",
      icon = icon("info"),
      class = "fade in",
      
      includeHTML(file.path("text", "howto.html"))
    ),
    
    tabPanel(
      "Plots",
      value = "plots",
      icon = icon("bar-chart"),
      class = "fade in",
      
      fluidPage(
        column(6,
          tags$strong("Reference Treatment"),
          selectInput("reference_treat", NULL, width = 500,
                      choices = c("", ref_drug_list_all), selected = ""),
          tags$strong("Test Treatment"),
          checkboxInput("upload_custom", "Upload TTP data file for test treatment",
                        FALSE),
          conditionalPanel("input.upload_custom",
                           fileInput("custom_file", NULL),
                           shinyjs::hidden(div(id = "upload_error"))
          ),
          selectInput("test_treat", NULL, width = 500,
                      choices = c("", ref_drug_list_all), selected = "")
        ),
        column(6,
          tags$strong("Subset by study (optional)"),
          selectInput("subset_by_study", NULL, width = 500,
                      choices = c("", study_list), selected = "")
        )
      ),
      div(
        id = "plot-main-opts",
        checkboxInput("opt_overlay", "Overlay data", value = FALSE),
        checkboxInput("opt_median", "Show median", value = FALSE),
        checkboxInput("opt_samplesize", "Show sample size", value = FALSE),
        checkboxInput("opt_timedays", "Time in days (default: weeks)", value = FALSE,
          width = 300) 
      ),
      plotOutput("tpp_boxplot", width = "100%", height = "500px")
    )
  )
)
