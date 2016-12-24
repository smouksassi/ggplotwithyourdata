library(ggplot2)

ttp_data <- read.csv("data/TTPDATA_Shiny.csv",
                     na.strings = c("", " ", ".", "NA", "na"),
                     stringsAsFactors = FALSE)

week_order <- c(0:10, "[11-12]","[13-14]", "[15-16]", "[17-18]", "[19-20]",
                "[21-22]", "[23-24]","[25-51]")
ttp_data$WEEK <- factor(ttp_data$WEEK, levels = week_order, ordered = TRUE)
drug_list <- as.character(unique(ttp_data$TRTDOSE))
