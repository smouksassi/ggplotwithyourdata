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
},
width = function() { plot_preview_width() },
height = function() { plot_preview_height() })

# Return the dimensions of the PDF page selected in the Export tab
pdf_page_dim <- reactive({
  if (input$export_pdf_orientation == "landscape") {
    width <- 11
    height <- 8.5
  } else if (input$export_pdf_orientation == "portrait") {
    width <- 8.5
    height <- 11
  } else {
    width <- input$export_pdf_width
    height <- input$export_pdf_height
  }
  list(width = width, height = height)
})

# Calculate the dimensions of the plot preview
plot_preview_dim <- reactive({
  # If it's PDF, the units are inches and default resolution is 72 px/inch
  if (input$export_file_type == "pdf") {
    width <- pdf_page_dim()$width * 72
    height <- pdf_page_dim()$height * 72
    
    # Account for multiple plots per page
    if (input$export_pdf_multiple) {
      width <- width / input$export_pdf_ncol
      height <- height / input$export_pdf_nrow
    }
  } else {
    width <- input$export_file_width
    height <- input$export_file_height
  }
  
  # Keep the aspect ratio, but make the max dimensions 500
  ratio <- height/width
  if (ratio > 1) {
    height <- 500
    width <- height/ratio
  } else {
    width <- 500
    height <- ratio*width
  }
  
  list(width = width, height = height)
})
plot_preview_width <- reactive({
  plot_preview_dim()$width
})
plot_preview_height<- reactive({
  plot_preview_dim()$height
})

# Remove the currently selected plot from the saved plots list
observeEvent(input$remove_plot, {
  values$plots[[input$plots_select]] <- NULL
})

# Determine the file name of the exported plots file.
# If there's only one plot or using PDF, export it in its raw format.
# Multiple plots in non-PDF format are zipped together.
export_file_name <- reactive({
  if (length(values$plots) == 1 || input$export_file_type == "pdf") {
    paste0("export-plots", ".", input$export_file_type)
  } else {
    paste0("export-plots", ".zip")
  }
})

# Download the saved plots
output$export_btn <- downloadHandler(
  filename = function() {
    export_file_name()
  },
  content = function(file) {
    tryCatch({
      file_type <- input$export_file_type
      
      # If saving as PDF, there are many more options, so take special care
      if (file_type == "pdf") {
        width <- pdf_page_dim()$width
        height <- pdf_page_dim()$height
        
        file_names <- "export_plots.pdf"
        grDevices::pdf(file = file_names, width = width, height = height,
                       title = file_names, onefile = TRUE)
        
        # Print all the images into a PDF file, one plot per page
        if (!input$export_pdf_multiple) {
          invisible <- lapply(values$plots, print)
        }
        # Print multiple plots per page
        else {
          plots_per_page <- input$export_pdf_nrow * input$export_pdf_ncol
          pages <- ceiling(length(values$plots) / plots_per_page)
          for (page in seq(pages)) {
            idx_start <- (page - 1) * plots_per_page + 1
            idx_end <- min(length(values$plots), page * plots_per_page)
            plots <- values$plots[idx_start:idx_end]
            gridExtra::grid.arrange(
              grobs = plots,
              nrow = input$export_pdf_nrow,
              ncol = input$export_pdf_ncol,
              as.table = (input$export_pdf_arrangement == "byrow")
            )              
          }
        }
        
        grDevices::dev.off()
      }
      # If saving as simple images, the job is much simpler
      else {
        # Create all the individual files for each plot and save the filenames
        file_names <- lapply(names(values$plots), function(plot_name) { 
          file_name <- paste0(plot_name, ".", file_type)
          export_params <- list(file_name,
                                width = input$export_file_width,
                                height = input$export_file_height)
          do.call(file_type, export_params)
          print(values$plots[[plot_name]])
          grDevices::dev.off()
          file_name
        })
        file_names <- unlist(file_names)
      }
      
      # If there's a single file, download the file. If multiple files, zip
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
  }
)