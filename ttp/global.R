library(ggplot2)

# Given a week range, figure out the human-friendly category name
# [1,2) becomes simply 1
# [11,13) becomes 11-12
week_category_name <- function(old_name) {
  # extract the two numbers out of the range
  limits <- unlist(strsplit(old_name, c("[^0-9]")))[-1]
  
  # if the range includes only one integer
  if (as.integer(limits[1]) + 1 == as.integer(limits[2])) {
    name <- limits[1]
  }
  # if the range includes multiple integers
  else {
    if (substr(old_name, nchar(old_name), nchar(old_name)) == "]") {
      name <- paste0(limits[1], "-", limits[2])
    } else {
      name <- paste0(limits[1], "-", as.integer(limits[2]) - 1)
    }
  }
  
  name
}

# validate a TTP dataset to ensure it contains the necessary columns and values
validate_dataset <- function(data) {
  required_cols <- c("WEEK", "TTP", "TRTDOSE")
  for (col in required_cols) {
    if (!col %in% colnames(data)) {
      error <- paste("Column", col, "must be present in the data")
      return(error)
    }
  }
  
  suppressWarnings(
    weeks <- as.integer(data$WEEK)
  )
  if (any(is.na(weeks)) || any(weeks < 0)) {
    error <-  "WEEK must be non-negative integers"
    return(error)
  }
  
  return(TRUE)
}

# Given a TTP dataset with the WEEKS column as raw integers, convert it to bins 
clean_dataset_weeks <- function(data) {
  data$WEEK_BIN <- data$WEEK
  max_week <- max(as.integer(data$WEEK))
  week_cats <- 
    cut(
      data$WEEK_BIN,
      
      breaks = unique(sort( c(0:11, 13, 15, 17, 19, 21, 23, 25, max_week) ) ),
      
      include.lowest = TRUE, right = FALSE, ordered_result = TRUE
    )
  levels(week_cats) <-
    as.list(
      setNames(
        levels(week_cats),
               lapply(levels(week_cats), week_category_name)
      )
    )
  data$WEEK_BIN <- week_cats
  max_day <- max(as.integer(data$TIMEDAYS))
  day_cats <- 
    cut(
      data$TIMEDAYS,
      breaks = unique(sort(c(0:15, 17, 19, 21, 23, 25, max_day ))) ,
      include.lowest = TRUE, right = FALSE, ordered_result = TRUE
    )
  levels(day_cats) <-
    as.list(
      setNames(
        levels(day_cats),
               lapply(levels(day_cats), week_category_name)
      )
    )
  data$DAY_BIN <- day_cats
  data
}

######################

# Read and set up the internal dataset
ttp_data <- read.csv("data/TTPDATA_Shiny.csv",
                     na.strings = c("", " ", ".", "NA", "na"),
                     stringsAsFactors = FALSE)
if (!isTRUE(validate_dataset(ttp_data))) {
  stop("There is a problem with the internal data file", call. = FALSE)
}
ttp_data <- clean_dataset_weeks(ttp_data)
ref_drug_list_all <- sort(as.character(unique(ttp_data$TRTDOSE)))
study_list <- sort(as.character(unique(ttp_data$STUDY)))

