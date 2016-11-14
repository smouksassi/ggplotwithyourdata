suppressMessages (library(shiny))
suppressMessages (library(ggplot2))
suppressMessages (library(ggrepel))
suppressMessages (library(scales))
suppressMessages (library(DT))
suppressMessages (library(tidyr))
suppressMessages (library(dplyr))
suppressMessages (library(ggkm))
suppressMessages (library(Hmisc))
suppressMessages (library(quantreg))
#suppressMessages (library(lazyeval))


stat_sum_df <- function(fun, geom="point", ...) {
  stat_summary(fun.data=fun,  geom=geom,  ...)
}
stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun,  geom=geom,  ...)
}

median.n <- function(x){
  return(c(y = ifelse(median(x)<0,median(x),median(x)),
           label = round(median(x),2))) 
}
give.n <- function(x){
  return(c(y = min(x)*1,  label = length(x))) 
}



options(shiny.maxRequestSize=250*1024^2) 
#options(shiny.reactlog=TRUE) 
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD",
               "#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

tableau20 <- c("#1F77B4","#AEC7E8", "#FF7F0E","#FFBB78"  ,"#2CA02C",
               "#98DF8A" ,"#D62728","#FF9896" ,"#9467BD","#C5B0D5" ,
               "#8C564B","#C49C94" ,"#E377C2","#F7B6D2" ,"#7F7F7F",
               "#C7C7C7" ,"#BCBD22","#DBDB8D" ,"#17BECF","#9EDAE5")
