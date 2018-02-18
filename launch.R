#' Launch ggplotwithyourdata
#' 
#' Launch the \code{ggplotwithyourdata} in a browser with custom data pre-loaded.
#' @param init_data The initial data to load into the app.
launch_ggplotwithyourdata <- function(init_data = NULL) {
  .GlobalEnv$ggplotwithyourdata_init_data <- init_data
  on.exit(rm(ggplotwithyourdata_init_data, envir = .GlobalEnv))
  shiny::runApp("shinyapp/")
}
