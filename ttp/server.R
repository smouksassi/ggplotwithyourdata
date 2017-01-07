# TODO
# allow user to upload data file for Test (have a checkbox)
# When an uploaded file is used the test treatment will be from this file while the reference will be from the built in and in this situation we will allow to have the same treatment since it comes from two different files


function(input, output, session) {
  # Make sure the reference and test treatment dropdowns don't let you select
  # the same drug
  observeEvent(input$reference_treat, {
    updateSelectInput(session, "test_treat",
                      choices = c("", setdiff(drug_list, input$reference_treat)),
                      selected = input$test_treat)
  })
  observeEvent(input$test_treat, {
    updateSelectInput(session, "reference_treat",
                      choices = c("", setdiff(drug_list, input$test_treat)),
                      selected = input$reference_treat)
  })
  
  # Create the main plot
  output$tpp_boxplot <- renderPlot({
    if (input$reference_treat == "" || input$test_treat == "") {
      return()
    }

    # Get data only for the two chosen drugs, and make sure the reference
    # comes before the treatment so that in the box plot it'll be on the left
    # (it will happen because "R" comes before "T")
    ref_rows <- subset(ttp_data, TRTDOSE == input$reference_treat)
    ref_rows$TRTDOSE_NAME <- paste0("Reference: ", input$reference_treat)
    test_rows <- subset(ttp_data, TRTDOSE == input$test_treat)
    test_rows$TRTDOSE_NAME <- paste0("Test: ", input$test_treat)
    data <- rbind(ref_rows, test_rows)
    
    plot <- ggplot(data, aes(WEEK_BIN, TTP)) +
      aes(color = TRTDOSE, group = TRTDOSE) + 
      geom_point(alpha = 0.5, shape = 16, size = 1) +
      geom_boxplot(aes(group = NULL), varwidth = FALSE,
                   notch = FALSE, show.legend = TRUE) + 
      stat_summary(fun.y = median, geom = "line") +
      xlab("Time (Weeks)") +
      ylab("TTP (Days)") +
      theme_bw(base_size = 16) +
      theme(legend.position = "bottom",
            legend.box = "vertical", legend.direction = "horizontal",
            axis.text.x = ggplot2::element_text(angle = 90,
                                                hjust = 1, vjust = 0.5),
            legend.title = element_blank())
    
    # Customize the plot based on user options
    if (!input$opt_overlay) {
      plot <- plot +
        facet_grid(. ~ TRTDOSE_NAME)
    }
    if (input$opt_samplesize) {
      give.n <- function(x){
        return(c(y = min(x)*1.05,  label = length(x))) 
      }
      plot <- plot +
        stat_summary(fun.data = give.n,
                     aes(group = NULL), geom = "label", alpha = 0.1,
                     fun.y = median, fontface = "bold", fill = "white",
                     show.legend = FALSE, size = 6,
                     position = position_dodge(width = 0.8))
    }
    if (input$opt_median) {
      median.n <- function(x){
        return(c(y = ifelse(median(x)<0,median(x),median(x)),
                 label = round(median(x),1))) 
      }
      plot <- plot + 
        stat_summary(fun.data = median.n,
                     aes(group = NULL), geom = "label", alpha = 0.1,
                     fun.y = median, fontface = "bold", fill = "white",
                     show.legend = FALSE, size = 6,
                     position=position_dodge(width = 0.8))
    }
    
    plot
  })
}