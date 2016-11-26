library(shiny)
library(ggplot2)
library(shinyjs)

inline_ui <- function(tag) {
  div(style = "display: inline-block", tag)
}

ui <- fluidPage(
  useShinyjs(),
  includeCSS("app.css"),
  tabsetPanel(
    tabPanel(
      "Plots",
      fluidRow(
        column(
          4,
          h1("Create a plot"),
          numericInput("num", "Number of points", 50, 1, 100),
          numericInput("size", "Point size", 2, 1, 10),
          numericInput("shape", "Point shape", 1, 0, 17)
        ),
        column(
          8,
          plotOutput("plot"),
          div(
            id = "save_plot_area",
            inline_ui(
              textInput("save_plot_name", NULL, "",
                        placeholder = "Enter plot name to save")
            ),
            actionButton("save_plot_btn", "Save plot", icon = icon("star")),
            shinyjs::hidden(
              span(
                id = "save_plot_checkmark",
                icon("check")
              )
            )
          )
        )
      )
    ),
    tabPanel(
      "Export",
      conditionalPanel(
        condition = "!output.saved_plots_exist",
        h2("You do not have any saved plots to export")
      ),
      conditionalPanel(
        condition = "output.saved_plots_exist",
        fluidRow(
          column(
            4,
            h2("Exporting", textOutput("num_plots", inline = TRUE),
               "saved plots"),
            selectInput("export_file_type", "File type",
              c("PDF" = "pdf", "JPEG" = "jpeg", "PNG" = "png", "BMP" = "bmp")),
            numericInput("export_file_height", "Height (inches)",
                         value = 7, min = 1, max = 100),
            numericInput("export_file_width", "Width (inches)",
                         value = 7, min = 1, max = 100),
            textInput("export_file_name", "File name", "plots"),
            downloadButton("export_btn", "Download plots")
          ),
          column(
            8,
            h2("Preview saved plots"),
            uiOutput("plots_select_ui"),
            actionButton("remove_plot", "Remove plot"),
            plotOutput("plot_preview")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  values <- reactiveValues(
    plots = list()
  )
  
  # ---------- Plots tab -----------
  
  # Code for generating a plot
  plot_object <- reactive({
    df <- data.frame(x = rnorm(input$num), y = rnorm(input$num))
    ggplot(df, aes(x, y)) +
      geom_point(size = input$size, shape = input$shape) +
      theme_bw()
  })

  # Display the plot
  output$plot <- renderPlot({
    plot_object()
  })
  
  # When the save button is clicked, add the plot to a list and clear the input
  observeEvent(input$save_plot_btn, {
    shinyjs::show("save_plot_checkmark")
    values$plots[[trimws(input$save_plot_name)]] <- plot_object()
    updateTextInput(session, "save_plot_name", value = "")
    shinyjs::delay(
      1000,
      shinyjs::hide("save_plot_checkmark", anim = TRUE, animType = "fade")
    )
  })
  
  # Disable the "save" button if the plot name input is empty
  observe({
    shinyjs::toggleState("save_plot_btn",
                         condition = nzchar(trimws(input$save_plot_name)))
  })
  
  # ----------- Export tab -----------
  
  # Create a server variable that we can use in the UI for a conditionalPanel
  output$saved_plots_exist <- reactive({
    length(values$plots) > 0
  })
  outputOptions(output, 'saved_plots_exist', suspendWhenHidden = FALSE)
  
  output$num_plots <- renderText({
    length(values$plots)
  })

  # Show a dropdown to select a plot to preview/remove in the Export tab
  output$plots_select_ui <- renderUI({
    selectInput("plots_select", "Plot name", names(values$plots))
  })
  
  # Preview a plot in the Export tab
  output$plot_preview <- renderPlot({
    if (is.null(input$plots_select)) return()
    values$plots[[input$plots_select]]
  })
  
  # Remove the currently selected plot from the saved plots list
  observeEvent(input$remove_plot, {
    values$plots[[input$plots_select]] <- NULL
  })

  # Determine the file name of the exported plots file
  # If there's only one plot, export it in its raw format. Zip multiple plots.
  export_file_name <- reactive({
    if (length(values$plots) == 1) {
      paste0(input$export_file_name, ".", input$export_file_type)
    } else {
      paste0(input$export_file_name, ".zip")
    }
  })
  
  # Update the "export dimensions" numeric inputs based on the export file type
  observeEvent(input$export_file_type, {
    export_unit <- if(input$export_file_type == "pdf") "inches" else "pixels"
    export_default_size <- if(input$export_file_type == "pdf") 7 else 480
    export_max_size <- if(input$export_file_type == "pdf") 50 else 2000
    
    updateNumericInput(session, "export_file_width",
      label = sprintf("Width (%s)", export_unit),
      value = export_default_size,
      max = export_max_size
    )
    updateNumericInput(session, "export_file_height",
      label = sprintf("Height (%s)", export_unit),
      value = export_default_size,
      max = export_max_size
    )
  })

  # Download the saved plots
  output$export_btn <- downloadHandler(
    filename = function() {
      export_file_name()
    },
    content = function(file) {
      tryCatch({
        file_type <- input$export_file_type
        
        # Create all the individual files for each plot and save the filenames
        file_names <- lapply(names(values$plots), function(plot_name) { 
          file_name <- paste0(plot_name, ".", file_type)
          export_params <- list(file_name,
                                width = input$export_file_height,
                                height = input$export_file_width)
          do.call(file_type, export_params)
          print(values$plots[[plot_name]])
          grDevices::dev.off()
          file_name
        })
        file_names <- unlist(file_names)
        
        # If there's a single plot, download the file. If multiple plots, zip
        if (length(file_names) == 1) {
          file.copy(file_names, file, overwrite = TRUE)
        } else {
          zip(file, file_names)
        }
        
        # Remove the generated files so that we don't run out of disk space :)
        file.remove(file_names)
      },
      error = function(err) {
        stop(err$message)
      })
    },
    contentType = "application/zip"
  )
  
observe({isolate({
values$plots[['aaa']] <- ggplot(mtcars,aes(mpg,wt))+geom_point()
values$plots[['bbb']] <- ggplot(mtcars,aes(mpg,wt))+geom_line()
values$plots[['...']] <- ggplot(mtcars,aes(mpg,wt))+geom_point(col="red")
values$plots[['3-s']] <- ggplot(mtcars,aes(mpg,wt))+geom_point(size=5)
})
})
}

shinyApp(ui = ui, server = server)