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



ui  <-  fluidPage(
    titlePanel("Hello GHAP HBGDki Member!"),
    sidebarLayout(
  sidebarPanel(
    tabsetPanel(
      tabPanel("Inputs", 
               fileInput("datafile", "Choose csv file to upload",
                         multiple = FALSE, accept = c("csv")),
               uiOutput("ycol"),uiOutput("xcol"),
               tabsetPanel(id = "filtercategorize",
                           tabPanel("Categorize/Rename", 
                                    uiOutput("catvar"),
                                    uiOutput("ncuts"),
                                    uiOutput("catvar2"),
                                    uiOutput("catvar3"),
                                    uiOutput("ncuts2"),
                                    uiOutput("asnumeric"),
                                    textOutput("bintext"),
                                    uiOutput("catvar4"),
                                    textOutput("labeltext"),
                                    uiOutput("nlabels")
                                    ),
                           
                           tabPanel("Combine Variables", 
                                    uiOutput("pastevar")
                           ),
                           tabPanel("Filters", 
                                    uiOutput("maxlevels"),
                                    uiOutput("filtervar1"),
                                    uiOutput("filtervar1values"),
                                    uiOutput("filtervar2"),
                                    uiOutput("filtervar2values"),
                                    uiOutput("filtervar3"),
                                    uiOutput("filtervar3values"),
                                    uiOutput("filtervarcont1"),
                                    uiOutput("fslider1"),
                                    uiOutput("filtervarcont2"),
                                    uiOutput("fslider2"),
                                    uiOutput("filtervarcont3"),
                                    uiOutput("fslider3")
                           ),
                           tabPanel("Simple Rounding",
                                    uiOutput("roundvar"),
                                    numericInput("rounddigits",label = "N Digits",value = 0,min=0,max=10) 
                           ),
                           tabPanel("Reorder Variables", 
                                    uiOutput("reordervar"),
                                    conditionalPanel(condition = "input.reordervarin!='' " ,
                                                     selectizeInput(  "functionordervariable", 'The:',
                                                                      choices =c("Median","Mean","Minimum","Maximum") ,multiple=FALSE)
                                    ),
                                    uiOutput("variabletoorderby"),
                                    conditionalPanel(condition = "input.reordervarin!='' " ,
                                                     checkboxInput('reverseorder', 'Reverse Order ?', value = FALSE) ),
                                    
                                    uiOutput("reordervar2"),
                                    uiOutput("reordervar2values"),
                                    uiOutput("catvar5"),
                                    textOutput("labeltext5"),
                                    uiOutput("nlabels5")
                           )
               ),
               hr()
      ), # tabsetPanel
      
      
      tabPanel("Graph Options",
               tabsetPanel(id = "graphicaloptions",
                           tabPanel(  "X/Y Log /Labels",
                                      hr(),
                                      textInput('ylab', 'Y axis label', value = "") ,
                                      textInput('xlab', 'X axis label', value = "") ,
                                      hr(),
                                      checkboxInput('logy', 'Log Y axis', value = FALSE) ,
                                      checkboxInput('logx', 'Log X axis', value = FALSE) ,
                                      conditionalPanel(condition = "!input.logy" ,
                                                       checkboxInput('scientificy', 'Comma separated Y axis ticks', value = FALSE)),
                                      conditionalPanel(condition = "!input.logx" ,
                                                       checkboxInput('scientificx', 'Comma separated X axis ticks', value = FALSE)),
                                      checkboxInput('rotateyticks', 'Rotate/Justify Y axis Ticks ?', value = FALSE),
                                      checkboxInput('rotatexticks', 'Rotate/Justify X axis Ticks ?', value = FALSE),
                                      conditionalPanel(condition = "input.rotateyticks" , 
                                                       sliderInput("yticksrotateangle", "Y axis ticks angle:", min=0, max=360, value=c(0),step=10),
                                                       sliderInput("ytickshjust", "Y axis ticks horizontal justification:", min=0, max=1, value=c(0.5),step=0.1),
                                                       sliderInput("yticksvjust", "Y axis ticks vertical justification:", min=0, max=1, value=c(0.5),step=0.1)
                                      ),
                                      conditionalPanel(condition = "input.rotatexticks" , 
                                                       sliderInput("xticksrotateangle", "X axis ticks angle:", min=0, max=360, value=c(20),step=10),
                                                       sliderInput("xtickshjust", "X axis ticks horizontal justification:", min=0, max=1, value=c(1),step=0.1),
                                                       sliderInput("xticksvjust", "X axis ticks vertical justification:", min=0, max=1, value=c(1),step=0.1)
                                      )
                                      
                           ),
                           tabPanel(  "Graph Size/Zoom",
                                      sliderInput("height", "Plot Height", min=1080/4, max=1080, value=480, animate = FALSE),
                                      h6("X Axis Zoom only works if facet x scales are not set to be free. The slider has limits between your x variable min/max otherwise select manual x values zoom to input your own."),
                                      uiOutput("xaxiszoom"),
                                      checkboxInput('userxzoom', 'Manual x values zoom', value = FALSE),
                                      conditionalPanel(condition = "input.userxzoom" ,uiOutput("lowerx"),uiOutput("upperx"))
                           ),
                           
                           tabPanel(  "Background Color and Legend Position",
                                      selectInput('backgroundcol', label ='Background Color',
                                                  choices=c("Gray" ="gray97","White"="white","Dark Gray"="grey90"),
                                                  multiple=FALSE, selectize=TRUE,selected="white"),
                                      selectInput('legendposition', label ='Legend Position',
                                                  choices=c("left", "right", "bottom", "top","none"),
                                                  multiple=FALSE, selectize=TRUE,selected="bottom"),
                                      selectInput('legenddirection', label ='Layout of Items in Legends',
                                                  choices=c("horizontal", "vertical"),
                                                  multiple=FALSE, selectize=TRUE,selected="horizontal"),
                                      selectInput('legendbox', label ='Arrangement of Multiple Legends ',
                                                  choices=c("horizontal", "vertical"),
                                                  multiple=FALSE, selectize=TRUE,selected="vertical")
                           ),
                           tabPanel(  "Facets Options",
                                      
                                      uiOutput("facetscales"),
                                      selectInput('facetspace' ,'Facet Spaces:',c("fixed","free_x","free_y","free")),
                                      
                                      selectizeInput(  "facetswitch", "Facet Switch to Near Axis:",
                                                       choices = c("x","y","both"),
                                                       options = list(  maxItems = 1 ,
                                                                        placeholder = 'Please select an option',
                                                                        onInitialize = I('function() { this.setValue(""); }')  )  ),
                                      checkboxInput('facetmargin', 'Show a facet with all data (margins)?'),
                                      
                                      selectInput('facetlabeller' ,'Facet Label:',c(
                                        "Variable(s) Name(s) and Value(s)" ="label_both",
                                        "Value(s)"="label_value",
                                        "Parsed Expression" ="label_parsed"),
                                        selected="label_both"),
                                      
                                      checkboxInput('facetwrap', 'Use facet_wrap?'),
                                      conditionalPanel(condition = "input.facetwrap" ,
                                                       checkboxInput('customncolnrow', 'Control N columns an N rows?')),
                                      conditionalPanel(condition = "input.customncolnrow" ,
                                                       h6("An error (nrow*ncol >= n is not TRUE) will show up if the total number of facets/panels is greater than the product of the specified  N columns x N rows. Increase the N columns and/or N rows to avoid the error. The default empty values will use ggplot automatic algorithm."),        
                                                       numericInput("wrapncol",label = "N columns",value =NA,min=1,max =10) ,
                                                       numericInput("wrapnrow",label = "N rows",value = NA,min=1,max=10) 
                                      )
                                      
                           ) ,
                           
                           tabPanel(  "Reference Lines",
                                      checkboxInput('identityline', 'Identity Line')    ,   
                                      checkboxInput('horizontalzero', 'Horizontol Zero Line'),
                                      checkboxInput('customline1', 'Vertical Line'),
                                      conditionalPanel(condition = "input.customline1" , 
                                                       numericInput("vline",label = "",value = 1) ),
                                      checkboxInput('customline2', 'Horizontal Line'),
                                      conditionalPanel(condition = "input.customline2" , 
                                                       numericInput("hline",label = "",value = 1) )
                           ),
                           tabPanel(  "Additional Themes Options",
                                      sliderInput("themebasesize", "Theme Size (affects all text elements in the plot):", min=1, max=100, value=c(16),step=1),
                                      checkboxInput('themetableau', 'Use Tableau Colors and Fills ? (maximum of 10 colours are provided)',value=TRUE),
                                      conditionalPanel(condition = "input.themetableau" ,
                                                       h6("If you have more than 10 color groups the plot will not work and you get /Error: Insufficient values in manual scale. ## needed but only 10 provided./  Uncheck Use Tableau Colors and Fills to use default ggplot2 colors.")),
                                      checkboxInput('themecolordrop', 'Keep All levels of Colors and Fills ?',value=TRUE) , 
                                      
                                      checkboxInput('themebw', 'Use Black and White Theme ?',value=TRUE), 
                                      checkboxInput('themeaspect', 'Use custom aspect ratio ?')   ,  
                                      conditionalPanel(condition = "input.themeaspect" , 
                                                       numericInput("aspectratio",label = "Y/X ratio",
                                                                    value = 1,min=0.1,max=10,step=0.01)),
                                      checkboxInput('sepguides', 'Separate Legend Guides for Median/PI ?',value = TRUE),       
                                      checkboxInput('labelguides', 'Hide the Names of the Guides ?',value = FALSE)  
                           ) #tabpanel
               )#tabsetpanel
      ), # tabpanel
      #) ,#tabsetPanel(),
      
      tabPanel("How To",
               h5("1. Upload your data file in CSV format. R default options for read.csv will apply except for missing values where both (NA) and dot (.) are treated as missing. If your data has columns with other non-numeric missing value codes then they be treated as factors."),
               h5("2. The UI is dynamic and changes depending on your choices of options, x ,y, filter, group and so on."),
               h5("3. It is assumed that your data is tidy and ready for plotting (long format)."),
               h5("4. x and y variable(s) input allow numeric and factor variables."),
               h5("5. You can select more than one y variable. The data will always be automatically stacked using tidyr::gather and result in yvars and yvalues variables.
                  if you select a mix of factor and continuous variables, all selected variables will be transformed to factor.
                  The internal variable yvalues is used for y variable mapping and you can select yvars for facetting. The app automatically select additional row split as yvars and set Facet Scales to free_y.
                  To change Facet Scales and many other options go to Graph Options tab."),
               h5("6. Inputs, Categorize/Rename, Recode into Binned Categories: Include the numeric variable to change to categorical in the list and then choose a number of cuts (default is 3). This helps when you want to group or color by cuts of a continuous variable."),
               h5("7. Inputs, Categorize/Rename, Treat as Categories: This changes a numeric variable to factor without binning. This helps when you want to group or color by numerical variable that has few unique values e.g. 1,2,3."),
               h5("8. Inputs, Categorize/Rename, Custom cuts: You can cut a numeric variable to factor using specified cutoffs, by default a text input is created and comma separated min, median, max are used. You can override with any values of your liking.
                  An optional Treat as Numeric checkbox can be selected to recode the created custom cuts variable to numeric values that start with 0. This can be useful to overlay a logistic smooth or to use a Kaplan-Meier curve. Numeric Codes/values correspondence are shown in text below the checkbox."),
               h5("9. Inputs, Categorize/Rename, Change labels of this variable: this field allow you to select a factor/character variable in order 
                  to input in a comma separated list of new labels. By default 1,2,...n of unique values will be populated in the field.
                  Make sure to type in the correct number of new labels otherwise the plot will not be generated as the recoding will fail.
                  You can combine levels by repeating a value e.g. 1,1,2."),
               
               h5("10.Inputs, Categorize, Combine Variables: This allows to paste together two variables other than those used for y axis (e.g. Sex with values: Male and Female and Treatment with values: TRT1, TRT2 and TRT3) to construct a new one called combinedvariable with values: Male TRT1, Male TRT2, Male TRT3, Female TRT1, Female TRT2, Female TRT3.
                  The combinedvariable is available to color, group and any other mappings and by default a dummy placeholder is made available."),
               h5("11. Inputs, Filters: There is six slots for Filter variables. Filter variables 1, 2,3,4,5 and 6 are applied sequentially. Values shown for filter variable 2 will depend on your selected data exclusions using filter variable 1 and so on. The first three filters accept numeric and non numeric columns while the last three are sliders and only work with numeric variables.
                  For performance improvement the first three filters only show variables with a default maximum number of levels of 500 but you can increase it to your needs."),
               h5("11. Inputs, Simple Rounding: allows you to round a numeric variable using the n digits you specify and defaults to 0. This convert values like
                  1.11, 1.12, 2.1 to 1, 1, 2 which provide an easy way to bin continuous values for better viewing in the plot."),
               h5("12. New ! Inputs, Reorder Variables: This group of options allow you to Reorder a categorical variable using a function 
                of another numerical variable and default to using the median of yvalues.
                 Maximum and Minimum are possible as well as choosing the variable of your liking.
                 You can also check the Reverse Order box to have the order inverted.
               The Custom Reorder this variable will create an input field with unique values of this variable and then
                 you can drag and drop to the order you like. Finally the change labels of this variable is another place where you can change the names of the levels of a character variable like the one holding the
                 names of the selected y variables yvars which is handy if you want to change the names in the facet labels. "),
               h5("13. You can use smoothing functions including, loess, linear and logistic fits. Please make sure that data is compatible with the smoothing used i.e. for logistic a numerical 0/1 variable is expected."),
               h5("14. You can also use quantile regression, median/percentiles, mean/CI. The median menu has options to label the plot with the median or the number
                  of data points used for the median computations."),
               h5("15. Limited boxplots support. Carefully choose grouping expecially when the x axis variable is continuous,
                  you can change the Group By: variable in the Color/Group/Split/Size/Fill Mappings (?) to better reflect your needs.
                  It is also possible to join the boxplots with a
                  line connecting medians and percentiles but for this to work color and group should be both set to none or to the same variable.
                  In addition, the ignore grouping variable in the median menu should be unchecked."),
               h5("16. Initial support to enable Kaplan-Meier Plots with Confidence Intervals, censoring ticks and commonly used transformations.
                  When K-M curves are added nothing else is show on the plot."),
               h5("17. Download the plot using the options on the 'Download' tab. This section is based on code from Mason DeCamillis ggplotLive app."),
               h5("18. Look at the data table in the 'Data' tab. You can reorder the columns, filter and much more."),
               p(),
               h5("Contact me @ samermouksassi@gmail.com for feedback or use the link below to file bugs/features/pull requests!"),
               helpText(   a("Click Here to submit an issue on github",href="https://github.com/smouksassi/ggplotwithyourdata/issues",target="_blank") ),
               h5("Samer Mouksassi 2016")
               
               
      )# tabpanel 
    )
  ), #sidebarPanel
  mainPanel(
    tabsetPanel(
      tabPanel("Plot"  , 
               uiOutput('ui_plot'),
               hr(),
               uiOutput("clickheader"),
               tableOutput("plot_clickedpoints"),
               uiOutput("brushheader"),
               tableOutput("plot_brushedpoints"),
               #actionButton("plotButton", "Update Plot"),
               uiOutput("optionsmenu") ,
               
               conditionalPanel(
                 condition = "input.showplottypes" , 
                 
                 fluidRow(
                   
                   column (12, hr()),
                   column (3,
                           radioButtons("Points", "Points/Jitter:",
                                        c("Points" = "Points",
                                          "Jitter" = "Jitter",
                                          "None" = "None")),
                           conditionalPanel( " input.Points!= 'None' ",
                                             sliderInput("pointstransparency", "Points Transparency:", min=0, max=1, value=c(0.5),step=0.01),
                                             checkboxInput('pointignorecol', 'Ignore Mapped Color')
                           )),
                   column(3,
                          conditionalPanel( " input.Points!= 'None' ",
                                            sliderInput("pointsizes", "Points Size:", min=0, max=4, value=c(1),step=0.1),
                                            numericInput('pointtypes','Points Type:',16, min = 1, max = 25),
                                            conditionalPanel( " input.pointignorecol ",
                                                              selectInput('colpoint', label ='Points Color', choices=colors(),multiple=FALSE, selectize=TRUE, selected="black") 
                                            )
                          )
                   ),                  
                   column(3,
                          radioButtons("line", "Lines:",
                                       c("Lines" = "Lines",
                                         "None" = "None"),selected="None"),
                          conditionalPanel( " input.line== 'Lines' ",
                                            sliderInput("linestransparency", "Lines Transparency:", min=0, max=1, value=c(0.5),step=0.01),
                                            checkboxInput('lineignorecol', 'Ignore Mapped Color')
                          )
                          
                   ),
                   column(3,
                          conditionalPanel( " input.line== 'Lines' ",
                                            sliderInput("linesize", "Lines Size:", min=0, max=4, value=c(1),step=0.1),
                                            selectInput('linetypes','Lines Type:',c("solid","dotted")),
                                            conditionalPanel( " input.lineignorecol ",
                                                              selectInput('colline', label ='Lines Color', choices=colors(),multiple=FALSE, selectize=TRUE,selected="black") 
                                            )
                          ),
                          checkboxInput('boxplotaddition', 'Add a Boxplot ? (makes sense if x variable is categorical and
                                        you Group By a sensible choice. By default the x variable is used for grouping)'),
                          checkboxInput('boxplotignoregroup', 'Ignore Mapped Group ? (can me helpful to superpose a loess or median on top of the boxplot)',value = TRUE)
                          
                   ),
                   column (12, h6("Points and Lines Size will apply only if Size By: in the Color Group Split Size Fill Mappings are set to None"))
                   
                 )#fluidrow
               ) ,
               conditionalPanel(
                 condition = "input.showfacets" , 
                 fluidRow(
                   column (12, hr()),
                   column (3, uiOutput("colour"),uiOutput("group")),
                   column(3, uiOutput("facet_col"),uiOutput("facet_row")),
                   column (3, uiOutput("facet_col_extra"),uiOutput("facet_row_extra")),
                   column (3, uiOutput("pointsize"),uiOutput("fill")),
                   column (12, h6("Make sure not to choose a variable that is in the y variable(s) list otherwise you will get an error Variable not found. These variables are stacked and become yvars and yvalues.This ensures that colour/group/etc. are kept intact when you apply a new filter or recode a variable." ))
                   
                 )
               ),
               
               #rqss quantile regression
               conditionalPanel(
                 condition = "input.showrqss" , 
                 
                 fluidRow(
                   column(12,hr()),
                   column(3,
                          checkboxInput('Tauvalue', 'Dynamic and Preset Quantiles', value = FALSE),
                          h5("Preset Quantiles"),
                          checkboxInput('up', '95%'),
                          checkboxInput('ninetieth', '90%'),
                          checkboxInput('mid', '50%', value = FALSE),
                          checkboxInput('tenth', '10%'),
                          checkboxInput('low', '5%')
                   ),
                   column(5,
                          sliderInput("Tau", label = "Dynamic Quantile Value:",
                                      min = 0, max = 1, value = 0.5, step = 0.01)  ,
                          sliderInput("Penalty", label = "Spline sensitivity adjustment:",
                                      min = 0, max = 10, value = 1, step = 0.1)  ,
                          selectInput("Constraints", label = "Spline constraints:",
                                      choices = c("N","I","D","V","C","VI","VD","CI","CD"), selected = "N")
                   ),
                   column(3,
                          checkboxInput('ignorecolqr', 'Ignore Mapped Color'),
                          checkboxInput('ignoregroupqr', 'Ignore Mapped Group',value = TRUE),
                          checkboxInput('hidedynamic', 'Hide Dynamic Quantile'),
                          selectInput('colqr', label ='QR Color', choices=colors(),multiple=FALSE, selectize=TRUE,selected="black")
                   )
                   
                 )#fluidrow
               ),
               
               conditionalPanel(
                 condition = "input.showSmooth" , 
                 
                 fluidRow(
                   column(12,hr()),
                   column (3, 
                           radioButtons("Smooth", "Smooth:",
                                        c("Smooth" = "Smooth",
                                          "Smooth and SE" = "Smooth and SE",
                                          "None" = "None"),selected="None")
                   ),
                   column (3, 
                           conditionalPanel( " input.Smooth!= 'None' ",
                                             selectInput('smoothmethod', label ='Smoothing Method',
                                                         choices=c("Loess" ="loess","Linear Fit"="lm","Logistic"="glm"),
                                                         multiple=FALSE, selectize=TRUE,selected="loess"),
                                             
                                             sliderInput("loessens", "Loess Span:", min=0, max=1, value=c(0.75),step=0.05)
                           ) 
                   ),
                   
                   
                   
                   
                   
                   column (3,  conditionalPanel( " input.Smooth!= 'None' ",
                                                 checkboxInput('ignorecol', 'Ignore Mapped Color'),
                                                 uiOutput("weight")
                   )
                   ),
                   column (3, conditionalPanel( " input.Smooth!= 'None' ",
                                                checkboxInput('ignoregroup', 'Ignore Mapped Group',value = TRUE),
                                                conditionalPanel( " input.ignorecol ",
                                                                  selectInput('colsmooth', label ='Smooth Color', choices=colors(),multiple=FALSE, selectize=TRUE,selected="black") )
                   ) )
                   
                 )#fluidrow
               )
               ,
               ### Mean CI section
               conditionalPanel(
                 condition = "input.showMean" , 
                 
                 fluidRow(
                   column(12,hr()),
                   column (3, 
                           radioButtons("Mean", "Mean:",
                                        c("Mean" = "Mean",
                                          "Mean (95% CI)" = "Mean (95% CI)",
                                          "None" = "None") ,selected="None") 
                   ),
                   column (3,
                           
                           conditionalPanel( " input.Mean== 'Mean (95% CI)' ",
                                             sliderInput("CI", "CI %:", min=0, max=1, value=c(0.95),step=0.01),
                                             numericInput( inputId = "errbar",label = "CI bar width:",value = 2,min = 1,max = NA)      
                           )
                           
                   )
                   ,
                   column (3,
                           conditionalPanel( " input.Mean!= 'None' ",
                                             checkboxInput('meanpoints', 'Show points') ,
                                             checkboxInput('meanlines', 'Show lines', value=TRUE),
                                             checkboxInput('meanignorecol', 'Ignore Mapped Color') ,
                                             conditionalPanel( " input.meanignorecol ",
                                                               selectInput('colmean', label ='Mean Color', choices=colors(),multiple=FALSE, selectize=TRUE,selected="black") )
                                             
                           ) ),
                   
                   
                   column(3,
                          conditionalPanel( " input.Mean!= 'None' ",
                                            checkboxInput('meanignoregroup', 'Ignore Mapped Group',value = TRUE),
                                            sliderInput("meanlinesize", "Mean(s) Line(s) Size:", min=0, max=3, value=1,step=0.05)
                          ) 
                   )
                 ) #fluidrow
               ), # conditional panel for mean
               
               ### median PI section
               
               
               conditionalPanel(
                 condition = "input.showMedian" , 
                 fluidRow(
                   column(12,hr()),
                   column (3,
                           radioButtons("Median", "Median:",
                                        c("Median" = "Median",
                                          "Median/PI" = "Median/PI",
                                          "None" = "None") ,selected="None") ,
                           conditionalPanel( " input.Median!= 'None' ",
                                             checkboxInput('medianvalues', 'Label Values?') ,
                                             checkboxInput('medianN', 'Label N?') )
                           
                   ),
                   column (3,
                           conditionalPanel( " input.Median== 'Median' ",
                                             checkboxInput('medianpoints', 'Show points') ,
                                             checkboxInput('medianlines', 'Show lines',value=TRUE)),
                           conditionalPanel( " input.Median== 'Median/PI' ",
                                             sliderInput("PI", "PI %:", min=0, max=1, value=c(0.95),step=0.01),
                                             sliderInput("PItransparency", "PI Transparency:", min=0, max=1, value=c(0.2),step=0.01)
                           )
                   ),
                   column (3,
                           conditionalPanel( " input.Median!= 'None' ",
                                             checkboxInput('medianignorecol', 'Ignore Mapped Color'),
                                             conditionalPanel( " input.medianignorecol ",
                                                               selectInput('colmedian', label ='Median Color', choices=colors(),multiple=FALSE, selectize=TRUE,selected="black") )
                                             
                           ) ),
                   column (3,
                           conditionalPanel( " input.Median!= 'None' ",
                                             
                                             checkboxInput('medianignoregroup', 'Ignore Mapped Group',value = TRUE),
                                             sliderInput("medianlinesize", "Median(s) Line(s) Size:", min=0, max=4, value=c(1),step=0.1)
                                             
                           )
                   )
                   
                 )#fluidrow
               ),
               ### median PI section
               
               ### KM section
               
               
               conditionalPanel(
                 condition = "input.showKM" , 
                 fluidRow(
                   column(12,hr()),
                   column (12, h6("KM curves support is currently experimental some features might not work. When a KM curve is added nothing else will be plotted (e.g. points, lines etc.).Color/Fill/Group/Facets are expected to work." )),
                   column (3,
                           radioButtons("KM", "KM:",
                                        c("KM" = "KM",
                                          "KM/CI" = "KM/CI",
                                          "None" = "None") ,selected="None") 
                   ),
                   column (3,
                           conditionalPanel( " input.KM!= 'None' ",
                                             checkboxInput('censoringticks', 'Show Censoring Ticks?') ,
                                             conditionalPanel( " input.KM== 'KM/CI' ",
                                                               sliderInput("KMCI", "KM CI:", min=0, max=1, value=c(0.95),step=0.01),
                                                               sliderInput("KMCItransparency", "KM CI Transparency:", min=0, max=1, value=c(0.2),step=0.01)
                                             )
                           )),
                   
                   column (3,
                           conditionalPanel( " input.KM!= 'None' ",
                                             selectInput('KMtrans', label ='KM Transformation',
                                                         choices=c("None" ="identity","event"="event","cumhaz"="cumhaz","cloglog"="cloglog"),
                                                         multiple=FALSE, selectize=TRUE,selected="loess")  
                           )
                   )
                 )#fluidrow
               )
               ### KM section
               
               ),#tabPanel1
      tabPanel("Download", 
               selectInput(
                 inputId = "downloadPlotType",
                 label   = h5("Select download file type"),
                 choices = list("PDF"  = "pdf","BMP"  = "bmp","JPEG" = "jpeg","PNG"  = "png")),
               
               # Allow the user to set the height and width of the plot download.
               h5(HTML("Set download image dimensions<br>(units are inches for PDF, pixels for all other formats)")),
               numericInput(
                 inputId = "downloadPlotHeight",label = "Height (inches)",value = 7,min = 1,max = 100),
               numericInput(
                 inputId = "downloadPlotWidth",label = "Width (inches)",value = 7,min = 1,max = 100),
               # Choose download filename.
               textInput(
                 inputId = "downloadPlotFileName",
                 label = h5("Enter file name for download")),
               
               # File downloads when this button is clicked.
               downloadButton(
                 outputId = "downloadPlot", 
                 label    = "Download Plot")
      ),
      
      tabPanel('Data',  dataTableOutput("mytablex") 
      )#tabPanel2
    )#tabsetPanel
  )#mainPanel
  )#sidebarLayout
)#fluidPage
server <-  function(input, output, session) {
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath,na.strings = c("NA","."))
    
    
  })
  
  
  
  myData <- reactive({
    df=filedata()
    if (is.null(df)) return(NULL)
  })
  
  output$optionsmenu <-  renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    fluidRow(
      column (12, h6("Select the checkbox(es) for the options to be showed")),
      hr(),
      column(4,checkboxInput('showplottypes',
                             'Plot types, Points, Lines (?)',
                             value = TRUE)),
      column(4,checkboxInput('showfacets',
                             'Color/Group/Split/Size/Fill Mappings (?)',
                             value = TRUE) ),
      column(4,checkboxInput('showrqss',
                             'Quantile Regression (?)',
                             value = TRUE)),
      column(4,checkboxInput('showSmooth',
                             'Smooth/Linear/Logistic Regressions (?)',
                             value = TRUE)),
      column(4,checkboxInput('showMean' , 'Mean CI (?)', value = FALSE)),
      column(4,checkboxInput('showMedian','Median PIs (?)', value = FALSE)),
      column(3,checkboxInput('showKM','Kaplan-Meier (?)', value = FALSE))
      
    )
  })
  
  output$ycol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("y", "y variable(s):",choices=items,selected = items[1],multiple=TRUE,selectize=TRUE)
  })
  
  output$xcol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("x", "x variable:",items,selected=items[2])
    
  })
  
  outputOptions(output, "ycol", suspendWhenHidden=FALSE)
  outputOptions(output, "xcol", suspendWhenHidden=FALSE)
  
  output$catvar <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- names(df)  [ MODEDF ]
    selectInput('catvarin',label = 'Recode into Binned Categories:',choices=NAMESTOKEEP2,multiple=TRUE)
  })
  
  
  output$ncuts <- renderUI({
    if (length(input$catvarin ) <1)  return(NULL)
    sliderInput('ncutsin',label = 'N of Cut Breaks:', min=2, max=10, value=c(3),step=1)
  })
  
  output$catvar2 <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- names(df)  [ MODEDF ]
    if (length(input$catvarin ) >=1) {
      NAMESTOKEEP2<-NAMESTOKEEP2 [ !is.element(NAMESTOKEEP2,input$catvarin) ]
    }
    
    selectInput('catvar2in',label = 'Treat as Categories:',choices=NAMESTOKEEP2,multiple=TRUE)
    
  })
  
  output$catvar3 <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- names(df)  [ MODEDF ]
    if (length(input$catvarin ) >=1) {
      NAMESTOKEEP2<-NAMESTOKEEP2 [ !is.element(NAMESTOKEEP2,input$catvarin) ]
    }
    if (length(input$catvar2in ) >=1) {
      NAMESTOKEEP2<-NAMESTOKEEP2 [ !is.element(NAMESTOKEEP2,input$catvar2in) ]
    }
    selectizeInput(  "catvar3in", 'Custom cuts of this variable, defaults to min, median, max before any applied filtering:',
                     choices =NAMESTOKEEP2 ,multiple=FALSE,
                     options = list(    placeholder = 'Please select a variable',
                                        onInitialize = I('function() { this.setValue(""); }')
                     )
    )
  })
  output$ncuts2 <- renderUI({
    df <-filedata()
    if (length(input$catvar3in ) <1)  return(NULL)
    if ( input$catvar3in!=""){
      textInput("xcutoffs", label =  paste(input$catvar3in,"Cuts"),
                value = as.character(paste(
                  min(df[,input$catvar3in] ,na.rm=T),
                  median(df[,input$catvar3in],na.rm=T),
                  max(df[,input$catvar3in],na.rm=T) ,sep=",")
                )
      )
    }
    
  })
  

  
  output$asnumeric <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    if (length(input$catvar3in ) <1)  return(NULL)
    if ( input$catvar3in!=""){
      column(12,
             checkboxInput('asnumericin', 'Treat as Numeric (helpful to overlay a smooth/regression line on top of a boxplot or to convert a variable into 0/1 and overlay a logistic fit', value = FALSE)
             #,checkboxInput('useasxaxislabels', 'Use the Categories Names as x axis label (makes sense only if you really chose it as x axis variable)', value = FALSE), 
             #checkboxInput('useasyaxislabels', 'Use the Categories Names as y axis label (makes sense only if you really chose it as y axis variable)', value = FALSE) 
      )
    }
  })
  
  
  outputOptions(output, "catvar", suspendWhenHidden=FALSE)
  outputOptions(output, "ncuts", suspendWhenHidden=FALSE)
  outputOptions(output, "catvar2", suspendWhenHidden=FALSE)
  outputOptions(output, "catvar3", suspendWhenHidden=FALSE)
  outputOptions(output, "ncuts2", suspendWhenHidden=FALSE)
  outputOptions(output, "asnumeric", suspendWhenHidden=FALSE)

  
  
    
  recodedata1  <- reactive({
    df <- filedata() 
    if (is.null(df)) return(NULL)
    if(length(input$catvarin ) >=1) {
      for (i in 1:length(input$catvarin ) ) {
        varname<- input$catvarin[i]
        df[,varname] <- cut(df[,varname],input$ncutsin)
        df[,varname]   <- as.factor( df[,varname])
      }
    }
    df
  })
  
  
  recodedata2  <- reactive({
    df <- recodedata1()
    if (is.null(df)) return(NULL)
    if(length(input$catvar2in ) >=1) {
      for (i in 1:length(input$catvar2in ) ) {
        varname<- input$catvar2in[i]
        df[,varname]   <- as.factor( df[,varname])
      }
    }
    df
  })
  
  recodedata3  <- reactive({
    df <- recodedata2()
    if (is.null(df)) return(NULL)
    if(input$catvar3in!="") {
      varname<- input$catvar3in
      xlimits <- input$xcutoffs 
      nxintervals <- length(as.numeric(unlist (strsplit(xlimits, ",")) )) -1
      df[,varname] <- cut( as.numeric ( as.character(  df[,varname])),
                           breaks=   as.numeric(unlist (strsplit(xlimits, ","))),include.lowest=TRUE)
      df[,"custombins"] <-   df[,varname] 
      
      if(input$asnumericin) {
        df[,varname] <- as.numeric(as.factor(df[,varname]) ) -1 
      }
    }
    
    df
  })
  output$bintext <- renderText({
    df <- recodedata3()
    if (is.null(df)) return(NULL)
    bintextout <- ""
    if(input$catvar3in!="") {
      varname<- input$catvar3in
      if(!input$asnumericin){
        bintextout <- levels(df[,"custombins"] )
      }
      if(input$asnumericin){
        bintextout <- paste( sort(unique(as.numeric(as.factor(df[,varname]) ) -1))  ,levels(df[,"custombins"] ),sep="/") 
      }}
    bintextout   
  })   
  #  xaxislabels <-levels(cut( as.numeric ( as.character( dataedafilter$month_ss)), breaks=   as.numeric(unlist (strsplit(ageglimits, ",") )),include.lowest=TRUE))
  #+ scale_x_continuous(breaks=seq(0,length(xaxislabels)-1),labels=xaxislabels )   useasxaxislabels
  output$catvar4 <- renderUI({
    df <-recodedata3()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- names(df)  [! MODEDF ]
    
    selectizeInput(  "catvar4in", 'Change labels of this variable:',
                     choices =NAMESTOKEEP2 ,multiple=FALSE,
                     options = list(    placeholder = 'Please select a variable',
                                        onInitialize = I('function() { this.setValue(""); }')
                     )
    )
  })
  
  output$labeltext <- renderText({
    df <- recodedata3()
    if (is.null(df)) return(NULL)
    labeltextout <- ""
    if(input$catvar4in!="") {
      varname<- input$catvar4in
      labeltextout <- c("Old labels",levels(df[,varname] ))
    }
    labeltextout   
  })   
  
  
  
  
  output$nlabels <- renderUI({
    df <-recodedata3()
    if (length(input$catvar4in ) <1)  return(NULL)
    if ( input$catvar4in!=""){
      nlevels <- length( unique( levels(as.factor( df[,input$catvar4in] ))))
      levelsvalues <- levels(as.factor( df[,input$catvar4in] ))
      textInput("customvarlabels", label =  paste(input$catvar4in,"requires",nlevels,"new labels,
                                                  type in a comma separated list below"),
                value =
                  # paste("\"",as.character(levelsvalues),"\"",collapse=", ",sep="")
                  #paste("'",as.character(1:nlevels),"'",collapse=", ",sep="")
                  paste(as.character(1:nlevels),collapse=", ",sep="")
      )
    }
    
  })
  outputOptions(output, "labeltext", suspendWhenHidden=FALSE) 
  outputOptions(output, "catvar4", suspendWhenHidden=FALSE)
  outputOptions(output, "nlabels", suspendWhenHidden=FALSE)
  
  
  recodedata4  <- reactive({
    df <- recodedata3()
    if (is.null(df)) return(NULL)
    if(input$catvar4in!="") {
      varname<- input$catvar4in
      xlabels <- input$customvarlabels 
     
      nxxlabels <- length(as.numeric(unlist (strsplit(xlabels, ",")) )) -1
      df[,varname] <- as.factor(df[,varname])
      levels(df[,varname])  <-  unlist (strsplit(xlabels, ",") )
    }
    
    df
  })
  
  
  output$pastevar <- renderUI({
    df <- recodedata4()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    yvariables <- input$y
    NAMESTOKEEP2<- names(df)  [! MODEDF ]
    NAMESTOKEEP2<- NAMESTOKEEP2[!NAMESTOKEEP2 %in% yvariables]
    
    selectizeInput("pastevarin", "Combine the categories of these two variables:", choices = NAMESTOKEEP2,multiple=TRUE,
                   options = list(
                     maxItems = 2 ,
                     placeholder = 'Please select two variables',
                     onInitialize = I('function() { this.setValue(""); }'),
                     plugins = list('remove_button', 'drag_drop')
                   )
    )
  })
  
  
  outputOptions(output, "pastevar", suspendWhenHidden=FALSE)
  outputOptions(output, "bintext", suspendWhenHidden=FALSE)
  
  
  
  
  output$maxlevels <- renderUI({
    df <-recodedata4()
    if (is.null(df)) return(NULL)
    numericInput( inputId = "inmaxlevels",label = "Max number of unique values for Filter variable (1),(2),(3) (this is to avoid performance issues):",value = 500,min = 1,max = NA)
    
  })
  outputOptions(output, "maxlevels", suspendWhenHidden=FALSE)
  
  
  output$filtervar1 <- renderUI({
    df <-recodedata4()
    if (is.null(df)) return(NULL)
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP<- names(df)  [ NUNIQUEDF  < input$inmaxlevels ]
    selectInput("infiltervar1" , "Filter variable (1):",c('None',NAMESTOKEEP ) )
  })
  
  output$filtervar2 <- renderUI({
    df <- recodedata4()
    if (is.null(df)) return(NULL)
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP<- names(df)  [ NUNIQUEDF  < input$inmaxlevels ]
    #NAMESTOKEEP<-  NAMESTOKEEP[ NAMESTOKEEP!=input$infiltervar1 ]
    selectInput("infiltervar2" , "Filter variable (2):",c('None',NAMESTOKEEP ) )
  })
  
  output$filtervar3 <- renderUI({
    df <- recodedata4()
    if (is.null(df)) return(NULL)
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP<- names(df)  [ NUNIQUEDF  < input$inmaxlevels ]
    #NAMESTOKEEP<-  NAMESTOKEEP[ NAMESTOKEEP!=input$infiltervar1 ]# allow nested filters
    #NAMESTOKEEP<-  NAMESTOKEEP[ NAMESTOKEEP!=input$infiltervar2 ]
    selectInput("infiltervar3" , "Filter variable (3):",c('None',NAMESTOKEEP ) )
  })
  
  
  output$filtervarcont1 <- renderUI({
    df <-recodedata4()
    if (is.null(df)) return(NULL)
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP<- names(df)
    NAMESTOKEEP<- NAMESTOKEEP[ is.element ( NAMESTOKEEP,names(df[sapply(df,is.numeric)]))]
    selectInput("infiltervarcont1" , "Filter continuous (1):",c('None',NAMESTOKEEP ) )
  })
  output$filtervarcont2 <- renderUI({
    df <-recodedata4()
    if (is.null(df)) return(NULL)
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP<- names(df)  
    NAMESTOKEEP<- NAMESTOKEEP[ is.element ( NAMESTOKEEP,names(df[sapply(df,is.numeric)]))]
    selectInput("infiltervarcont2" , "Filter continuous (2):",c('None',NAMESTOKEEP ) )
  })
  output$filtervarcont3 <- renderUI({
    df <-recodedata4()
    if (is.null(df)) return(NULL)
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP<- names(df)  
    NAMESTOKEEP<- NAMESTOKEEP[ is.element ( NAMESTOKEEP,names(df[sapply(df,is.numeric)]))]
    selectInput("infiltervarcont3" , "Filter continuous (3):",c('None',NAMESTOKEEP ) )
  })
  output$filtervar1values <- renderUI({
    df <-recodedata4()
    validate(       need(!is.null(df), "Please select a data set"))
    
    if (is.null(df)) return(NULL)
    if(input$infiltervar1=="None") {return(NULL)}
    if(input$infiltervar1!="None" )  {
      choices <- levels(as.factor(df[,input$infiltervar1]))
      selectInput('infiltervar1valuesnotnull',
                  label = paste("Select values", input$infiltervar1),
                  choices = c(choices),
                  selected = choices,
                  multiple=TRUE, selectize=FALSE)   
    }
  }) 
  
  filterdata  <- reactive({
    if (is.null(filedata())) return(NULL)
    df <-   recodedata4()
    if (is.null(df)) return(NULL)
    if(is.null(input$infiltervar1)) {
      df <-  df 
    }
    if(!is.null(input$infiltervar1)&input$infiltervar1!="None") {
      
      df <-  df [ is.element(df[,input$infiltervar1],input$infiltervar1valuesnotnull),]
    }
    
    df
  })
  
  output$filtervar2values <- renderUI({
    df <- filterdata()
    if (is.null(df)) return(NULL)
    if(input$infiltervar2=="None") {
      selectInput('infiltervar2valuesnull',
                  label ='No filter variable 2 specified', 
                  choices = list(""),multiple=TRUE, selectize=FALSE)   
    }
    if(input$infiltervar2!="None"&!is.null(input$infiltervar2) )  {
      choices <- levels(as.factor(as.character(df[,input$infiltervar2])))
      selectInput('infiltervar2valuesnotnull',
                  label = paste("Select values", input$infiltervar2),
                  choices = c(choices),
                  selected = choices,
                  multiple=TRUE, selectize=TRUE)   
    }
  })
  
  filterdata2  <- reactive({
    df <- filterdata()
    if (is.null(df)) return(NULL)
    if(!is.null(input$infiltervar2)&input$infiltervar2!="None") {
      df <-  df [ is.element(df[,input$infiltervar2],input$infiltervar2valuesnotnull),]
    }
    if(input$infiltervar2=="None") {
      df 
    }
    df
  }) 
  output$filtervar3values <- renderUI({
    df <- filterdata2()
    if (is.null(df)) return(NULL)
    if(input$infiltervar3=="None") {
      selectInput('infiltervar3valuesnull',
                  label ='No filter variable 2 specified', 
                  choices = list(""),multiple=TRUE, selectize=FALSE)   
    }
    if(input$infiltervar3!="None"&!is.null(input$infiltervar3) )  {
      choices <- levels(as.factor(as.character(df[,input$infiltervar3])))
      selectInput('infiltervar3valuesnotnull',
                  label = paste("Select values", input$infiltervar3),
                  choices = c(choices),
                  selected = choices,
                  multiple=TRUE, selectize=TRUE)   
    }
  })
  
  filterdata3  <- reactive({
    df <- filterdata2()
    if (is.null(df)) return(NULL)
    if(!is.null(input$infiltervar3)&input$infiltervar3!="None") {
      df <-  df [ is.element(df[,input$infiltervar3],input$infiltervar3valuesnotnull),]
    }
    if(input$infiltervar3=="None") {
      df 
    }
    df
  })  
  
  output$fslider1 <- renderUI({ 
    df <-  filterdata3()
    if (is.null(df)) return(NULL)
    xvariable<- input$infiltervarcont1
    if(input$infiltervarcont1=="None" ){
      return(NULL)  
    }
    if (!is.numeric(df[,xvariable]) ) return(NULL)
    if(input$infiltervarcont1!="None" ){
      sliderInput("infSlider1", paste("Select",xvariable,"Range"),
                  min=min(df[,xvariable],na.rm=T),
                  max=max(df[,xvariable],na.rm=T),
                  value=c(min(df[,xvariable],na.rm=T),max(df[,xvariable],na.rm=T)) 
      )
    }             
  })
  filterdata4  <- reactive({
    df <- filterdata3()
    if (is.null(df)) return(NULL)
    if(input$infiltervarcont1!="None" ){
      if(is.numeric( input$infSlider1[1]) & is.numeric(df[,input$infiltervarcont1])) {
        df <- df [!is.na(df[,input$infiltervarcont1]),]
        df <-  df [df[,input$infiltervarcont1] >= input$infSlider1[1]&df[,input$infiltervarcont1] <= input$infSlider1[2],]
      }
    }
    
    df
  })
  output$fslider2 <- renderUI({ 
    df <-  filterdata4()
    if (is.null(df)) return(NULL)
    xvariable<- input$infiltervarcont2
    if(input$infiltervarcont2=="None" ){
      return(NULL)  
    }
    if (!is.numeric(df[,xvariable]) ) return(NULL)
    if(input$infiltervarcont2!="None" ){
      sliderInput("infSlider2", paste("Select",xvariable,"Range"),
                  min=min(df[,xvariable],na.rm=T),
                  max=max(df[,xvariable],na.rm=T),
                  value=c(min(df[,xvariable],na.rm=T),max(df[,xvariable],na.rm=T)) 
      )
    }             
  })
  
  
  filterdata5  <- reactive({
    df <- filterdata4()
    if (is.null(df)) return(NULL)
    if(input$infiltervarcont2!="None" ){
      if(is.numeric( input$infSlider2[1]) & is.numeric(df[,input$infiltervarcont2])) {
        df<- df [!is.na(df[,input$infiltervarcont2]),]
        df<-df [df[,input$infiltervarcont2] >= input$infSlider2[1]&df[,input$infiltervarcont2] <= input$infSlider2[2],]
      }
    }
    
    df
  })
  
  output$fslider3 <- renderUI({ 
    df <-  filterdata5()
    if (is.null(df)) return(NULL)
    xvariable<- input$infiltervarcont3
    if(input$infiltervarcont3=="None" ){
      return(NULL)  
    }
    if (!is.numeric(df[,xvariable]) ) return(NULL)
    if(input$infiltervarcont3!="None" ){
      sliderInput("infSlider3", paste("Select",xvariable,"Range"),
                  min=min(df[,xvariable],na.rm=T),
                  max=max(df[,xvariable],na.rm=T),
                  value=c(min(df[,xvariable],na.rm=T),max(df[,xvariable],na.rm=T)) 
      )
    }             
  })
  
  
  filterdata6  <- reactive({
    df <- filterdata5()
    if (is.null(df)) return(NULL)
    if(input$infiltervarcont3!="None" ){
      if(is.numeric( input$infSlider3[1]) & is.numeric(df[,input$infiltervarcont3])) {
        df<- df [!is.na(df[,input$infiltervarcont3]),]
        df<-df [df[,input$infiltervarcont3] >= input$infSlider3[1]&df[,input$infiltervarcont3] <= input$infSlider3[2],]
      }
    }
    
    df
  })
  
  outputOptions(output, "filtervar1", suspendWhenHidden=FALSE)
  outputOptions(output, "filtervar2", suspendWhenHidden=FALSE)
  outputOptions(output, "filtervar3", suspendWhenHidden=FALSE)
  
  outputOptions(output, "filtervarcont1", suspendWhenHidden=FALSE)
  outputOptions(output, "filtervarcont2", suspendWhenHidden=FALSE)
  outputOptions(output, "filtervarcont3", suspendWhenHidden=FALSE)
  outputOptions(output, "filtervar1values", suspendWhenHidden=FALSE)
  outputOptions(output, "filtervar2values", suspendWhenHidden=FALSE)
  outputOptions(output, "filtervar3values", suspendWhenHidden=FALSE)
  
  outputOptions(output, "fslider1", suspendWhenHidden=FALSE)
  outputOptions(output, "fslider2", suspendWhenHidden=FALSE)
  outputOptions(output, "fslider3", suspendWhenHidden=FALSE)
  
  
  

  
  stackdata <- reactive({
    
    df <- filterdata6() 
    
    if (is.null(df)) return(NULL)
    if (!is.null(df)){
      validate(  need(!is.element(input$x,input$y) , "Please select a different x variable or remove the x variable from the list of y variable(s)"))
      #   validate(
      #    need(!is.null(length(input$y)| length(input$y) <1) , 
      #         "Please select a at least one y variable"))
      
      
      if(       all( sapply(df[,as.vector(input$y)], is.numeric)) )
      {
        tidydata <- df %>%
          gather_( "yvars", "yvalues", gather_cols=as.vector(input$y) ) %>%
          mutate(combinedvariable="Choose two variables to combine first")
      }
      if(       any( sapply(df[,as.vector(input$y)], is.factor)) |
                any( sapply(df[,as.vector(input$y)], is.character)))
      {
        tidydata <- df %>%
          gather_( "yvars", "yvalues", gather_cols=as.vector(input$y) ) %>%
          mutate(yvalues=as.factor(as.factor(as.character(yvalues)) ))%>%
          mutate(combinedvariable="Choose two variables to combine first")
      } 
      
      if(       all( sapply(df[,as.vector(input$y)], is.factor)) |
                all( sapply(df[,as.vector(input$y)], is.character)))
      {
        tidydata <- df %>%
          gather_( "yvars", "yvalues", gather_cols=as.vector(input$y) ) %>%
          mutate(yvalues=as.factor(as.character(yvalues) ))%>%
          mutate(combinedvariable="Choose two variables to combine first")
      }    
      
      
    }
    
    if( !is.null(input$pastevarin)   ) {
      if (length(input$pastevarin) > 1) {
        tidydata <- tidydata %>%
          unite_("combinedvariable" , c(input$pastevarin[1], input$pastevarin[2] ),
                 remove=FALSE)
      }
    }
    
    tidydata
  })
  
  
  output$roundvar <- renderUI({
    df <- stackdata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- names(df)  [MODEDF]
    selectizeInput(  "roundvarin", "Round the Values to the Specified N Digits:", choices = NAMESTOKEEP2,multiple=TRUE,
                     options = list(
                       placeholder = 'Please select some variables',
                       onInitialize = I('function() { this.setValue(""); }')
                     )
    )
    
  }) 
  
  rounddata <- reactive({
    if (is.null(df)) return(NULL)
    df <- stackdata()
    if(length(input$roundvarin ) >=1) {
      for (i in 1:length(input$roundvarin ) ) {
        varname<- input$roundvarin[i]
        df[,varname]   <- round( df[,varname],input$rounddigits)
      }
    }
    df
  })  
  
  
  output$reordervar <- renderUI({
    df <- rounddata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- names(df)  [ !MODEDF ]
    selectizeInput(  "reordervarin", 'Reorder This Variable By:',
                     choices =NAMESTOKEEP2 ,multiple=FALSE,
                     options = list(    placeholder = 'Please select a variable',
                                        onInitialize = I('function() { this.setValue(""); }')
                     )
    )
  })
  
  
  
  output$variabletoorderby <- renderUI({
    if (is.null(df)) return(NULL)
    if (length(input$reordervarin ) <1)  return(NULL)
    if ( input$reordervarin!=""){
      df <-rounddata()
      yinputs <- input$y
      items=names(df)
      names(items)=items
      MODEDF <- sapply(df, function(x) is.numeric(x))
      NAMESTOKEEP2<- names(df)  [ MODEDF ]
      selectInput('varreorderin',label = 'Of this Variable:', choices=NAMESTOKEEP2,multiple=FALSE,selected="yvalues")
    }
  })
  
  

  outputOptions(output, "roundvar", suspendWhenHidden=FALSE)
  outputOptions(output, "reordervar", suspendWhenHidden=FALSE)
  outputOptions(output, "variabletoorderby", suspendWhenHidden=FALSE)
  
  
  
  reorderdata <- reactive({
    df <- rounddata()
    if (is.null(df)) return(NULL)
    
    if(length(input$reordervarin ) >=1 &
       length(input$varreorderin ) >=1 & input$reordervarin!=""  ) {
      varname<- input$reordervarin[1]
      if(input$functionordervariable=="Median" )  {
        df[,varname]   <- reorder( df[,varname],df[,input$varreorderin], FUN=function(x) median(x[!is.na(x)]))
      }
      if(input$functionordervariable=="Mean" )  {
        df[,varname]   <- reorder( df[,varname],df[,input$varreorderin],  FUN=function(x) mean(x[!is.na(x)]))
      }
      if(input$functionordervariable=="Minimum" )  {
        df[,varname]   <- reorder( df[,varname],df[,input$varreorderin],  FUN=function(x) min(x[!is.na(x)]))
      }
      if(input$functionordervariable=="Maximum" )  {
        df[,varname]   <- reorder( df[,varname],df[,input$varreorderin],  FUN=function(x) max(x[!is.na(x)]))
      }
      if(input$reverseorder )  {
        df[,varname] <- factor( df[,varname], levels=rev(levels( df[,varname])))
        
      }
    }
    df
  })  
  
  
    output$reordervar2 <- renderUI({
      df <- reorderdata()
      if (is.null(df)) return(NULL)
      MODEDF <- sapply(df, function(x) is.numeric(x))
      NAMESTOKEEP<- names(df)  [ !MODEDF ]
      if(length(input$reordervarin ) >=1  ){
        NAMESTOKEEP<- NAMESTOKEEP  [ NAMESTOKEEP!=input$reordervarin ]
        
      }
      selectInput("reordervar2in" , "Custom Reorder this variable:",c('None',NAMESTOKEEP ) )
    })

      output$reordervar2values <- renderUI({
        df <- reorderdata()
        if (is.null(df)) return(NULL)
        if(input$reordervar2in=="None") {
          selectInput('reordervar2valuesnull',
                      label ='No reorder variable specified', 
                      choices = list(""),multiple=TRUE, selectize=FALSE)   
        }
        if(input$reordervar2in!="None"&!is.null(input$reordervar2in) )  {
          choices <- levels(as.factor(as.character(df[,input$reordervar2in])))
          selectizeInput('reordervar2valuesnotnull',
                      label = paste("Drag/Drop to reorder",input$reordervar2in, "values"),
                      choices = c(choices),
                      selected = choices,
                      multiple=TRUE,  options = list(
                      plugins = list('drag_drop')
                      )
                      )   
        }
      })
    outputOptions(output, "reordervar2", suspendWhenHidden=FALSE)
    outputOptions(output, "reordervar2values", suspendWhenHidden=FALSE)
    
    reorderdata2 <- reactive({
      df <- reorderdata()
      if (is.null(df)) return(NULL)
      
      if(input$reordervar2in!="None"  ) {
df [,input$reordervar2in] <- factor(df [,input$reordervar2in],
                                    levels = input$reordervar2valuesnotnull)

}
      df
    })
    
  output$xaxiszoom <- renderUI({
    df <-reorderdata2()
    if (is.null(df)| !is.numeric(df[,input$x] ) ) return(NULL)
    if (is.numeric(df[,input$x]) &
        input$facetscalesin!="free_x"&
        input$facetscalesin!="free"){
      xvalues <- df[,input$x][!is.na( df[,input$x])]
      xmin <- min(xvalues)
      xmax <- max(xvalues)
      xstep <- (xmax -xmin)/100
      sliderInput('xaxiszoomin',label = 'Zoom to X variable range:', min=xmin, max=xmax, value=c(xmin,xmax),step=xstep)
      
    }
    
    
  })
  outputOptions(output, "xaxiszoom", suspendWhenHidden=FALSE)
  
  output$lowerx <- renderUI({
    df <-reorderdata2()
    if (is.null(df)| !is.numeric(df[,input$x] ) ) return(NULL)
    if (is.numeric(df[,input$x]) &
        input$facetscalesin!="free_x"&
        input$facetscalesin!="free"){
      xvalues <- df[,input$x][!is.na( df[,input$x])]
      xmin <- min(xvalues)
      numericInput("lowerxin",label = "Lower Limit",value = xmin,min=NA,max=NA,width='50%')
    }
  })
  output$upperx <- renderUI({
    df <-reorderdata2()
    if (is.null(df)| !is.numeric(df[,input$x] ) ) return(NULL)
    if (is.numeric(df[,input$x]) &
        input$facetscalesin!="free_x"&
        input$facetscalesin!="free"){
      xvalues <- df[,input$x][!is.na( df[,input$x])]
      xmax <- max(xvalues)
      numericInput("upperxin",label = "Lower Limit",value = xmax,min=NA,max=NA,width='50%')
    }
  }) 
  outputOptions(output, "lowerx", suspendWhenHidden=FALSE)
  outputOptions(output, "upperx", suspendWhenHidden=FALSE)
  
  output$catvar5 <- renderUI({
    df <-reorderdata2()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- names(df)  [! MODEDF ]
    
    selectizeInput(  "catvar5in", 'Change labels of this variable:',
                     choices =NAMESTOKEEP2 ,multiple=FALSE,
                     options = list(    placeholder = 'Please select a variable',
                                        onInitialize = I('function() { this.setValue(""); }')
                     )
    )
  })
  
  output$labeltext5 <- renderText({
    df <- reorderdata2()
    if (is.null(df)) return(NULL)
    labeltext5out <- ""
    if(input$catvar5in!="") {
      varname<- input$catvar5in
      labeltext5out <- c("Old labels",levels(as.factor(df[,varname]) ))
    }
    labeltext5out   
  })   
  
  output$nlabels5 <- renderUI({
    df <-reorderdata2()
    if (length(input$catvar5in ) <1)  return(NULL)
    if ( input$catvar5in!=""){
      nlevels <- length( unique( levels(as.factor( df[,input$catvar5in] ))))
      levelsvalues <- levels(as.factor( df[,input$catvar5in] ))
      textInput("customvarlabels5", label =  paste(input$catvar5in,"requires",nlevels,"new labels,
                                                   type in a comma separated list below"),
                value =   paste(as.character(1:nlevels),collapse=", ",sep="")
      )
    }
    
  })
  
  outputOptions(output, "catvar5", suspendWhenHidden=FALSE)
  outputOptions(output, "nlabels5", suspendWhenHidden=FALSE)
  outputOptions(output, "labeltext5", suspendWhenHidden=FALSE)
  
  recodedata5  <- reactive({
    df <- reorderdata2()
    if (is.null(df)) return(NULL)
    if(input$catvar5in!="") {
      varname<- input$catvar5in
      xlabels <- input$customvarlabels5 
      nxxlabels <- length(as.numeric(unlist (strsplit(xlabels, ",")) )) -1
      df[,varname] <- as.factor(df[,varname])
      levels(df[,varname])  <-  unlist (strsplit(xlabels, ",") )
    }
    df
  })
  
  output$colour <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items
    items= c("None",items, "yvars","yvalues","combinedvariable") 
    selectInput("colorin", "Colour By:",items) 
    
  })
  
  
  output$group <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items 
    items= c("None",items, "yvars","yvalues","combinedvariable")
    selectInput("groupin", "Group By:",items)
  })
  
  
  output$facet_col <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items 
    selectInput("facetcolin", "Column Split:",c(None='.',items,"yvars", "yvalues","combinedvariable"))
  })
  output$facet_row <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items 
    selectInput("facetrowin", "Row Split:",    c(None=".",items,"yvars", "yvalues","combinedvariable"))
  })
  
  output$facet_col_extra <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items 
    selectInput("facetcolextrain", "Extra Column Split:",c(None='.',items,"yvars", "yvalues","combinedvariable"))
  })
  output$facet_row_extra <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items #[!is.element(items,input$y)]
    if (length(input$y) > 1 ){
      items= c("yvars",None=".",items, "yvalues","combinedvariable")    
    }
    if (length(input$y) < 2 ){
      items= c(None=".",items,"yvars", "yvalues","combinedvariable")    
    }
    selectInput("facetrowextrain", "Extra Row Split:",items)
  })

  output$facetscales <- renderUI({
    if (length(input$y) > 1 ){
      items= c("free_y","fixed","free_x","free")    
    }
    if (length(input$y) < 2 ){
      items= c("fixed","free_x","free_y","free")   
    }
    selectInput('facetscalesin','Facet Scales:',items)
  })

  outputOptions(output, "facet_row_extra", suspendWhenHidden=FALSE)
  outputOptions(output, "facet_col_extra", suspendWhenHidden=FALSE)
  outputOptions(output, "facet_row", suspendWhenHidden=FALSE)
  outputOptions(output, "facet_col", suspendWhenHidden=FALSE)
  outputOptions(output, "facetscales", suspendWhenHidden=FALSE)
  
  output$pointsize <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items 
    selectInput("pointsizein", "Size By:",c("None",items,"yvars", "yvalues","combinedvariable") )
    
  })
  
  output$fill <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items 
    selectInput("fillin", "Fill By:"    ,c("None",items,"yvars", "yvalues","combinedvariable") )
  })
  
  output$weight <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items 
    selectInput("weightin", "Weight By:",c("None",items,"yvars", "yvalues","combinedvariable") )
  })
  outputOptions(output, "weight", suspendWhenHidden=FALSE)
  
  
  output$mytablex = renderDataTable({
    datatable( recodedata5() , # reorderdata2
               extensions = c('ColReorder','Buttons','FixedColumns'),
               options = list(dom = 'Bfrtip',
                              searchHighlight = TRUE,
                              pageLength=-1 ,
                              lengthMenu = list(c(5, 10, 15, -1), c('5','10', '15', 'All')),
                              colReorder = list(realtime = TRUE),
                              buttons = 
                                list('colvis', 'pageLength','print','copy', list(
                                  extend = 'collection',
                                  buttons = list(
                                    list(extend='csv'  ,filename = 'plotdata'),
                                    list(extend='excel',filename = 'plotdata'),
                                    list(extend='pdf'  ,filename = 'plotdata')),
                                  text = 'Download'
                                )),
                              scrollX = TRUE,scrollY = 400,
                              fixedColumns = TRUE
               ), 
               filter = 'bottom',
               style = "bootstrap")
  })
  
  
  
  plotObject <- reactive({
    validate(
      need(!is.null(recodedata5()), "Please select a data set") 
    )
    
    plotdata <- recodedata5()
    
    
    if(!is.null(plotdata)) {
      
      if (input$themetableau){
        scale_colour_discrete <- function(...) 
          scale_colour_manual(..., values = tableau10,drop=!input$themecolordrop)
        scale_fill_discrete <- function(...) 
          scale_fill_manual(..., values = tableau10,drop=!input$themecolordrop)
      }
      
      p <- ggplot(plotdata, aes_string(x=input$x, y="yvalues")) 
      
      if (input$colorin != 'None')
        p <- p + aes_string(color=input$colorin)
      if (input$fillin != 'None')
        p <- p + aes_string(fill=input$fillin)
      if (input$pointsizein != 'None')
        p <- p  + aes_string(size=input$pointsizein)
      
      # if (input$groupin != 'None' & !is.factor(plotdata[,input$x]))
      if (input$groupin != 'None')
        p <- p + aes_string(group=input$groupin)
      if (input$groupin == 'None' & !is.numeric(plotdata[,input$x]) 
          & input$colorin == 'None')
        p <- p + aes(group=1)
      
      if (input$Points=="Points"&input$pointsizein == 'None'&!input$pointignorecol)
        p <- p + geom_point(,alpha=input$pointstransparency,shape=input$pointtypes,size=input$pointsizes)  
      if (input$Points=="Points"&input$pointsizein != 'None'&!input$pointignorecol)
        p <- p + geom_point(,alpha=input$pointstransparency,shape=input$pointtypes)
      
      if (input$Points=="Jitter"&input$pointsizein == 'None'&!input$pointignorecol)
        p <- p + geom_jitter(,alpha=input$pointstransparency,shape=input$pointtypes,size=input$pointsizes)
      if (input$Points=="Jitter"&input$pointsizein != 'None'&!input$pointignorecol)
        p <- p + geom_jitter(,alpha=input$pointstransparency,shape=input$pointtypes)
      
      
      if (input$Points=="Points"&input$pointsizein == 'None'&input$pointignorecol)
        p <- p + geom_point(,alpha=input$pointstransparency,shape=input$pointtypes,size=input$pointsizes,colour=input$colpoint)  
      if (input$Points=="Points"&input$pointsizein != 'None'&input$pointignorecol)
        p <- p + geom_point(,alpha=input$pointstransparency,shape=input$pointtypes,colour=input$colpoint)
      
      if (input$Points=="Jitter"&input$pointsizein == 'None'&input$pointignorecol)
        p <- p + geom_jitter(,alpha=input$pointstransparency,shape=input$pointtypes,size=input$pointsizes,colour=input$colpoint)
      if (input$Points=="Jitter"&input$pointsizein != 'None'&input$pointignorecol)
        p <- p + geom_jitter(,alpha=input$pointstransparency,shape=input$pointtypes,colour=input$colpoint)
      
      
      
      if (input$line=="Lines"&input$pointsizein == 'None'& !input$lineignorecol)
        p <- p + geom_line(,size=input$linesize,alpha=input$linestransparency,linetype=input$linetypes)
      if (input$line=="Lines"&input$pointsizein != 'None'& !input$lineignorecol)
        p <- p + geom_line(,alpha=input$linestransparency,linetype=input$linetypes)
      if (input$line=="Lines"&input$pointsizein == 'None'&input$lineignorecol)
        p <- p + geom_line(,size=input$linesize,alpha=input$linestransparency,linetype=input$linetypes,colour=input$colline)
      if (input$line=="Lines"&input$pointsizein != 'None'& input$lineignorecol)
        p <- p + geom_line(,alpha=input$linestransparency,linetype=input$linetypes,colour=input$colline)
      
      
      if (input$boxplotaddition){
        if (input$groupin != 'None'& !input$boxplotignoregroup ){
          p <- p + aes_string(group=input$groupin)
          p <- p + geom_boxplot()
        }
        if (input$groupin == 'None'){
          p <- p + geom_boxplot(aes(group=NULL))
        }  
        if (input$boxplotignoregroup ){
          p <- p + geom_boxplot(aes(group=NULL))
        } 
        
        
      }
      
      
      ###### Mean section  START 
      
      
      if (!input$meanignoregroup) {
        if (!input$meanignorecol) {
          
          if (input$Mean=="Mean") {
            if(input$meanlines&input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "line")
            if(input$meanlines&input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "line",size=input$meanlinesize)
            
            
            if(input$meanpoints)           
              p <- p + 
                stat_sum_single(mean, geom = "point")
            
          }
          
          if (input$Mean=="Mean (95% CI)"){
            p <- p + 
              stat_sum_df("mean_cl_normal", geom = "errorbar",fun.args=list(conf.int=input$CI),width=input$errbar)
            if(input$meanlines&input$pointsizein != 'None')  
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "line")
            if(input$meanlines&input$pointsizein == 'None')  
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "line",size=input$meanlinesize)
            if(input$meanpoints)           
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "point")
            
          }
        }
        
        
        if (input$meanignorecol) {
          meancol <- input$colmean
          if (input$Mean=="Mean") {
            if(input$meanlines&input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "line",col=meancol)
            
            if(input$meanlines&input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "line",col=meancol,size=input$meanlinesize)
            
            if(input$meanpoints)           
              p <- p + 
                stat_sum_single(mean, geom = "point",col=meancol)
            
          }
          
          if (input$Mean=="Mean (95% CI)"){
            p <- p + 
              stat_sum_df("mean_cl_normal", geom = "errorbar",fun.args=list(conf.int=input$CI),width=input$errbar, col=meancol)
            if(input$meanlines&input$pointsizein != 'None')  
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "line", col=meancol)
            if(input$meanlines&input$pointsizein == 'None')  
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "line", col=meancol,size=input$meanlinesize)
            
            if(input$meanpoints)           
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "point", col=meancol)
            
          }
        }
      }
      
      if (input$meanignoregroup) {
        if (!input$meanignorecol) {
          
          if (input$Mean=="Mean") {
            if(input$meanlines&input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "line",aes(group=NULL))
            if(input$meanlines&input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "line",aes(group=NULL),size=input$meanlinesize)
            
            if(input$meanpoints)           
              p <- p + 
                stat_sum_single(mean, geom = "point",aes(group=NULL))
            
          }
          
          if (input$Mean=="Mean (95% CI)"){
            p <- p + 
              stat_sum_df("mean_cl_normal", geom = "errorbar",fun.args=list(conf.int=input$CI), width=input$errbar,aes(group=NULL))
            if(input$meanlines&input$pointsizein != 'None')  
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "line",aes(group=NULL))
            if(input$meanlines&input$pointsizein == 'None')  
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "line",aes(group=NULL),size=input$meanlinesize)
            if(input$meanpoints)           
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "point",aes(group=NULL))
            
          }
        }
        
        
        if (input$meanignorecol) {
          meancol <- input$colmean
          if (input$Mean=="Mean") {
            if(input$meanlines&input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "line",col=meancol,aes(group=NULL))
            if(input$meanlines&input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "line",col=meancol,aes(group=NULL),size=input$meanlinesize)
            
            
            if(input$meanpoints)           
              p <- p + 
                stat_sum_single(mean, geom = "point",col=meancol,aes(group=NULL))
            
          }
          
          if (input$Mean=="Mean (95% CI)"){
            p <- p + 
              stat_sum_df("mean_cl_normal", geom = "errorbar",fun.args=list(conf.int=input$CI), width=input$errbar, col=meancol, aes(group=NULL))
            if(input$meanlines&input$pointsizein != 'None')  
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "line",col=meancol,aes(group=NULL))
            if(input$meanlines&input$pointsizein == 'None')  
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "line",col=meancol,aes(group=NULL),size=input$meanlinesize)
            
            if(input$meanpoints)           
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "point",col=meancol,aes(group=NULL))
            
          }
        }
      }
      ###### Mean section  END 
      
      ###### Smoothing Section START
      if(!is.null(input$Smooth) ){
        familyargument <- ifelse(input$smoothmethod=="glm","binomial","gaussian") 
        
        if ( input$ignoregroup) {
          if (!input$ignorecol) {
            spanplot <- input$loessens
            if (input$Smooth=="Smooth")
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=F,span=spanplot,aes(group=NULL))
            
            if (input$Smooth=="Smooth and SE")
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=T,span=spanplot,aes(group=NULL))
            
            if (input$Smooth=="Smooth"& input$weightin != 'None')
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=F,span=spanplot,aes(group=NULL))+  
                aes_string(weight=input$weightin)
            
            if (input$Smooth=="Smooth and SE"& input$weightin != 'None')
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=T,span=spanplot,aes(group=NULL))+  
                aes_string(weight=input$weightin)
          }
          if (input$ignorecol) {
            spanplot <- input$loessens
            colsmooth <- input$colsmooth
            if (input$Smooth=="Smooth")
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=F,span=spanplot,col=colsmooth,aes(group=NULL))
            
            if (input$Smooth=="Smooth and SE")
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=T,span=spanplot,col=colsmooth,aes(group=NULL))
            
            if (input$Smooth=="Smooth"& input$weightin != 'None')
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=F,span=spanplot,col=colsmooth,aes(group=NULL))+  
              aes_string(weight=input$weightin)
            
            if (input$Smooth=="Smooth and SE"& input$weightin != 'None')
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=T,span=spanplot,col=colsmooth,aes(group=NULL))+  
              aes_string(weight=input$weightin)
          }
          
        }
        
        if ( !input$ignoregroup) {
          if (!input$ignorecol) {
            spanplot <- input$loessens
            if (input$Smooth=="Smooth")
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=F,span=spanplot)
            
            if (input$Smooth=="Smooth and SE")
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=T,span=spanplot)
            
            if (input$Smooth=="Smooth"& input$weightin != 'None')
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=F,span=spanplot)+  
                aes_string(weight=input$weightin)
            
            if (input$Smooth=="Smooth and SE"& input$weightin != 'None')
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=T,span=spanplot)+  
                aes_string(weight=input$weightin)
          }
          if (input$ignorecol) {
            spanplot <- input$loessens
            colsmooth <- input$colsmooth
            if (input$Smooth=="Smooth")
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=F,span=spanplot,col=colsmooth)
            
            if (input$Smooth=="Smooth and SE")
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=T,span=spanplot,col=colsmooth)
            
            if (input$Smooth=="Smooth"& input$weightin != 'None')
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=F,span=spanplot,col=colsmooth)+  
              aes_string(weight=input$weightin)
            
            if (input$Smooth=="Smooth and SE"& input$weightin != 'None')
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=T,span=spanplot,col=colsmooth)+  
              aes_string(weight=input$weightin)
          }
          
        }
        
        ###### smooth Section END
      }
      
      
      ###### Median PI section  START  
      if (!input$medianignoregroup) {
        
        if (!input$medianignorecol) {
          
          if (input$Median=="Median") {
            if(input$medianlines&input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(median, geom = "line")
            
            if(input$medianlines&input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(median, geom = "line",size=input$medianlinesize)
            
            
            if(input$medianpoints)           
              p <- p + 
                stat_sum_single(median, geom = "point")
            
          }
          
          if (input$Median=="Median/PI"&input$pointsizein == 'None'){
            p <- p + 
              stat_sum_df("median_hilow", geom = "ribbon",fun.args=list(conf.int=input$PI) ,size=input$medianlinesize,alpha=input$PItransparency,col=NA)+ 
              stat_sum_df("median_hilow", geom = "smooth",fun.args=list(conf.int=input$PI) ,size=input$medianlinesize,alpha=0)
            
            if ( input$sepguides )
              p <-   p + 
                guides(
                  color = guide_legend(paste("Median"),
                                       override.aes = list(shape =NA,fill=NA)),
                  fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                       override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                  ) )
            
          }
          
          if (input$Median=="Median/PI"&input$pointsizein != 'None'){
            p <- p + 
              stat_sum_df("median_hilow", geom = "ribbon",fun.args=list(conf.int=input$PI), alpha=input$PItransparency,col=NA)+
              stat_sum_df("median_hilow", geom = "smooth"  ,fun.args=list(conf.int=input$PI),alpha=0)
            
            if ( input$sepguides )
              p <-   p +
                guides(
                  color = guide_legend(paste("Median"),
                                       override.aes = list(shape =NA,fill=NA)),
                  fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                       override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                  ) )
            
          }
          
          if (input$Median!="None" & input$medianvalues )  {
            p <-   p   +
              stat_summary(fun.data = median.n,geom = "label_repel",alpha=0.1,
                           fun.y = median, fontface = "bold",
                           show.legend=FALSE,size=6)}
          if (input$Median!="None" & input$medianN)  {
            p <-   p   +
              stat_summary(fun.data = give.n, geom = "label_repel",alpha=0.1,
                           fun.y = median, fontface = "bold", 
                           show.legend=FALSE,size=6)      
          }  
        }
        
        
        
        if (input$medianignorecol) {
          mediancol <- input$colmedian
          if (input$Median=="Median") {
            if(input$medianlines&input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(median, geom = "line",col=mediancol)
            
            if(input$medianlines&input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(median, geom = "line",col=mediancol,size=input$medianlinesize)
            
            if(input$medianpoints)           
              p <- p + 
                stat_sum_single(median, geom = "point",col=mediancol)
            
          }
          
          if (input$Median=="Median/PI"&input$pointsizein == 'None'){
            p <- p + 
              stat_sum_df("median_hilow", geom = "ribbon", fun.args=list(conf.int=input$PI), alpha=input$PItransparency,col=NA)+
              stat_sum_df("median_hilow", geom = "smooth", fun.args=list(conf.int=input$PI), size=input$medianlinesize,col=mediancol,alpha=0)
            
            if ( input$sepguides )
              p <-   p +
                guides(
                  color = guide_legend(paste("Median"),
                                       override.aes = list(shape =NA,fill=NA)),
                  fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                       override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                  ) )
          }
          if (input$Median=="Median/PI"&input$pointsizein != 'None'){
            p <- p + 
              stat_sum_df("median_hilow", geom = "ribbon",fun.args=list(conf.int=input$PI),alpha=input$PItransparency,col=NA)+
              stat_sum_df("median_hilow", geom = "smooth",fun.args=list(conf.int=input$PI),col=mediancol,
                          alpha=0)          
            
            if ( input$sepguides )
              p <-   p +
                guides(
                  color = guide_legend(paste("Median"),
                                       override.aes = list(shape =NA,fill=NA)),
                  fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                       override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                  ) )
          }
          if (input$Median!="None" & input$medianvalues )  {
            p <-   p   +
              stat_summary(fun.data = median.n,geom = "label_repel",alpha=0.1,
                           fun.y = median, fontface = "bold",colour=mediancol,
                           show.legend=FALSE,size=6)}
          if (input$Median!="None" & input$medianN)  {
            p <-   p   +
              stat_summary(fun.data = give.n, geom = "label_repel",alpha=0.1,
                           fun.y = median, fontface = "bold", colour=mediancol,
                           show.legend=FALSE,size=6)      
          }       
          
        }
      }
      
      
      if (input$medianignoregroup) {
        if (!input$medianignorecol) {
          if (input$Median=="Median") {
            if(input$medianlines&input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(median, geom = "line",aes(group=NULL))
            if(input$medianlines&input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(median, geom = "line",aes(group=NULL),size=input$medianlinesize)
            
            if(input$medianpoints)           
              p <- p + 
                stat_sum_single(median, geom = "point",aes(group=NULL))
            
          }
          
          if (input$Median=="Median/PI"&input$pointsizein == 'None'){
            p <- p + 
              stat_sum_df("median_hilow", geom = "ribbon",fun.args=list(conf.int=input$PI),aes(group=NULL),alpha=input$PItransparency,col=NA)+ 
              stat_sum_df("median_hilow", geom = "smooth",fun.args=list(conf.int=input$PI),aes(group=NULL),size=input$medianlinesize,alpha=0)   
            if ( input$sepguides )
              p <-   p +
                guides(
                  color = guide_legend(paste("Median"),
                                       override.aes = list(shape =NA,fill=NA)),
                  fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                       override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                  ) )
          }
          
          if (input$Median=="Median/PI"&input$pointsizein != 'None'){
            p <- p + 
              stat_sum_df("median_hilow", geom = "ribbon",fun.args=list(conf.int=input$PI),aes(group=NULL),alpha=input$PItransparency,col=NA)+ 
              stat_sum_df("median_hilow", geom = "smooth",fun.args=list(conf.int=input$PI),aes(group=NULL),alpha=0)
            if ( input$sepguides )
              p <-   p +
                guides(
                  color = guide_legend(paste("Median"),
                                       override.aes = list(shape =NA,fill=NA)),
                  fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                       override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                  ) )
          }
          if (input$Median!="None" & input$medianvalues )  {
            p <-   p   +
              stat_summary(fun.data = median.n, aes(group=NULL),geom = "label_repel",alpha=0.1,
                           fun.y = median, fontface = "bold",fill="white",
                           show.legend=FALSE,
                           size=6)}
          if (input$Median!="None" & input$medianN)  {
            p <-   p   +
              stat_summary(fun.data = give.n, aes(group=NULL), geom = "label_repel",alpha=0.1,
                           fun.y = median, fontface = "bold", fill="white",
                           show.legend=FALSE,size=6)      
          }
          
          
        }
        
        
        if (input$medianignorecol) {
          mediancol <- input$colmedian
          if (input$Median=="Median") {
            if(input$medianlines&input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(median, geom = "line",col=mediancol,aes(group=NULL))
            if(input$medianlines&input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(median, geom = "line",col=mediancol,aes(group=NULL),size=input$medianlinesize)
            
            if(input$medianpoints)           
              p <- p + 
                stat_sum_single(median, geom = "point",col=mediancol,aes(group=NULL))
            
          }
          
          if (input$Median=="Median/PI"&input$pointsizein == 'None'){
            p <- p + 
              stat_sum_df("median_hilow", geom = "ribbon",fun.args=list(conf.int=input$PI),aes(group=NULL),alpha=input$PItransparency,col=NA)+ 
              stat_sum_df("median_hilow", geom = "smooth",fun.args=list(conf.int=input$PI),col=mediancol,aes(group=NULL),size=input$medianlinesize,alpha=0)
            if ( input$sepguides )
              p <-   p +
                guides(
                  color = guide_legend(paste("Median"),
                                       override.aes = list(shape =NA,fill=NA)),
                  fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                       override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                  ) )
          }
          if (input$Median=="Median/PI"&input$pointsizein != 'None'){
            p <- p + 
              stat_sum_df("median_hilow", geom = "ribbon",fun.args=list(conf.int=input$PI),aes(group=NULL),alpha=input$PItransparency,col=NA)+ 
              stat_sum_df("median_hilow", geom = "smooth",fun.args=list(conf.int=input$PI),col=mediancol,aes(group=NULL),alpha=0)
            
            
            
            if ( input$sepguides )
              p <-   p +
                guides(
                  color = guide_legend(paste("Median"),
                                       override.aes = list(shape =NA,fill=NA)),
                  fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                       override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                  ) )
          }
          
          
          if (input$Median!="None" & input$medianvalues )  {
            p <-   p   +
              stat_summary(fun.data = median.n, aes(group=NULL),geom = "label_repel",alpha=0.1,
                           fun.y = median, fontface = "bold",colour=mediancol,
                           show.legend=FALSE,size=6)}
          if (input$Median!="None" & input$medianN)  {
            p <-   p   +
              stat_summary(fun.data = give.n, aes(group=NULL), geom = "label_repel",alpha=0.1,
                           fun.y = median, fontface = "bold", colour=mediancol,
                           show.legend=FALSE,size=6)      
          }
          
        }
      }
      
      
      
      ###### Median PI section  END
      
      
      
      ###### RQSS SECTION START  
      if (!input$ignoregroupqr) {
        if (!input$ignorecolqr) {
          if (input$Tauvalue) {
            if(!input$hidedynamic){
              p <- p +  stat_quantile(method = "rqss",quantiles =input$Tau,size=1.5,
                                      linetype="solid", 
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))       
            }
            
            if (input$mid)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.5,size=1.5,
                                      linetype="solid",
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            if (input$ninetieth)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.90,size=1,
                                      linetype="dashed",
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            if (input$tenth)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.1,size=1,
                                      linetype="dashed",
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            
            if (input$up)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.95,size=1,
                                      linetype="dashed",
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            
            if (input$low) 
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.05,size=1,
                                      linetype="dashed",
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            
            
            
          }
        }
        if (input$ignorecolqr) {
          colqr <- input$colqr
          if (input$Tauvalue) {
            if(!input$hidedynamic){
              p <- p +  stat_quantile(method = "rqss",quantiles =input$Tau,size=1.5,
                                      linetype="solid", col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty)) 
            }
            
            
            if (input$mid)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.5,size=1.5,
                                      linetype="solid", col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            if (input$ninetieth)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.90,size=1,
                                      linetype="dashed", col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            if (input$tenth)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.1,size=1,
                                      linetype="dashed", col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            
            if (input$up)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.95,size=1,
                                      linetype="dashed", col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            
            if (input$low) 
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.05,size=1,
                                      linetype="dashed", col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            
            
            
          }
        }
      }
      
      
      if (input$ignoregroupqr) {
        if (!input$ignorecolqr) {
          if (input$Tauvalue) {
            if(!input$hidedynamic){
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles =input$Tau,size=1.5,
                                      linetype="solid",
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty)) 
            }
            
            if (input$mid)
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.5,size=1.5,
                                      linetype="solid",
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            if (input$ninetieth)
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.90,size=1,
                                      linetype="dashed", 
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            if (input$tenth)
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.1,size=1,
                                      linetype="dashed",
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            
            if (input$up)
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.95,size=1,
                                      linetype="dashed", 
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            
            if (input$low) 
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.05,size=1,
                                      linetype="dashed", 
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
          }
        }
        if (input$ignorecolqr) {
          colqr <- input$colqr
          if (input$Tauvalue) {
            if(!input$hidedynamic){
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles =input$Tau,size=1.5,
                                      linetype="solid",col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))     
            }
            
            
            if (input$mid)
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.5,size=1.5,
                                      linetype="solid", col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            if (input$ninetieth)
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.90,size=1,
                                      linetype="dashed", col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            if (input$tenth)
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.1,size=1,
                                      linetype="dashed", col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            
            if (input$up)
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.95,size=1,
                                      linetype="dashed", col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            
            if (input$low) 
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.05,size=1,
                                      linetype="dashed", col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            
          }
        }
      }
      
      
      ###### RQSS SECTION END
      
      ###### KM SECTION START
      
      if (input$KM!="None") {
        p <- ggplot(plotdata, aes_string(time=input$x, status="yvalues")) 
        if (input$colorin != 'None')
          p <- p + aes_string(color=input$colorin)
        if (input$fillin != 'None')
          p <- p + aes_string(fill=input$fillin)
        if (input$groupin != 'None' & !is.factor(plotdata[,input$x]))
          p <- p + aes_string(group=input$groupin)
      }
      p <- p + xlab(input$x)
      if (input$KM=="KM/CI") {
        p <- p +
          geom_kmband(alpha=input$KMCItransparency,conf.int = input$KMCI,trans=input$KMtrans)                 }
      
      
      if (input$KM!="None") {
        p  <- p +
          geom_smooth(stat="km",trans=input$KMtrans)
      }
      if (input$censoringticks) {
        p  <- p +
          geom_kmticks(trans=input$KMtrans)
      }
      
      
      
      
      ###### KM SECTION END
      
      
      facets <- paste(input$facetrowin,'~', input$facetcolin)
      
      if (input$facetrowextrain !="."&input$facetrowin !="."){
        facets <- paste(input$facetrowextrain ,"+", input$facetrowin, '~', input$facetcolin)
      }  
      if (input$facetrowextrain !="."&input$facetrowin =="."){
        facets <- paste( input$facetrowextrain, '~', input$facetcolin)
      }  
      
      if (input$facetcolextrain !="."){
        facets <- paste( facets, "+",input$facetcolextrain)
      }  
      if (facets != '. ~ .')
        p <- p + facet_grid(facets,scales=input$facetscalesin,space=input$facetspace
                            ,labeller=input$facetlabeller,margins=input$facetmargin )
      
      if (facets != '. ~ .' & input$facetswitch!="" )
        
        p <- p + facet_grid(facets,scales=input$facetscalesin,space=input$facetspace,
                            switch=input$facetswitch
                            , labeller=input$facetlabeller,
                            margins=input$facetmargin )
      
      if (facets != '. ~ .'&input$facetwrap) {
        p <- p + facet_wrap(    c(input$facetrowextrain ,input$facetrowin,input$facetcolin,input$facetcolextrain ) [
          c(input$facetrowextrain ,input$facetrowin,input$facetcolin,input$facetcolextrain )!="."]
          ,scales=input$facetscalesin)
        
        if (input$facetwrap&input$customncolnrow) {
          p <- p + facet_wrap(    c(input$facetrowextrain ,input$facetrowin,input$facetcolin,input$facetcolextrain ) [
            c(input$facetrowextrain ,input$facetrowin,input$facetcolin,input$facetcolextrain )!="."]
            ,scales=input$facetscalesin,ncol=input$wrapncol,nrow=input$wrapnrow)
        }
      }
      
      
      
      
      if (input$logy)
        p <- p + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                               labels = trans_format("log10", math_format(10^.x))) 
      
      if (input$logx)
        p <- p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                               labels = trans_format("log10", math_format(10^.x))) 
      
      
      
      if (input$scientificy & is.numeric(plotdata[,"yvalues"]) )
        p <- p  + 
        scale_y_continuous(labels=comma )
      
      if (input$scientificx  &  is.numeric(plotdata[,input$x]) )
        p <- p  + 
        scale_x_continuous(labels=comma) 
      
      
      
      
      if (length(input$y) >= 2 & input$ylab=="" ){
        p <- p + ylab("Y variable(s)")
      }
      if (length(input$y) < 2 & input$ylab=="" ){
        p <- p + ylab(input$y)
      }
      
      if (input$xlab!="")
        p <- p + xlab(input$xlab)
      if (input$ylab!="")
        p <- p + ylab(input$ylab)
      
      
      if (input$horizontalzero)
        p <-    p+
        geom_hline(aes(yintercept=0))
      
      if (input$customline1)
        p <-    p+
        geom_vline(xintercept=input$vline)
      
      
      if (input$customline2)
        p <-    p+
        geom_hline(yintercept=input$hline)
      
      
      
      if (input$identityline)
        p <-    p+ geom_abline(intercept = 0, slope = 1)
      
      if (input$themebw) {
        p <-    p+
          theme_bw(base_size=input$themebasesize)     
      }
      
      
      if (!input$themebw){
        p <- p +
          theme_gray(base_size=input$themebasesize)+
          theme(  
            #axis.title.y = element_text(size = rel(1.5)),
            #axis.title.x = element_text(size = rel(1.5))#,
            #strip.text.x = element_text(size = 16),
            #strip.text.y = element_text(size = 16)
          )
      }
      
      
      p <-    p+theme(
        legend.position=input$legendposition,
        legend.box=input$legendbox,
        legend.direction=input$legenddirection,
        panel.background = element_rect(fill=input$backgroundcol))
      
      if (input$labelguides)
        p <-    p+
        theme(legend.title=element_blank())
      if (input$themeaspect)
        p <-    p+
        theme(aspect.ratio=input$aspectratio)
      if (!input$themetableau){
        p <-  p +
          scale_colour_hue(drop=!input$themecolordrop)+
          scale_fill_hue(drop=!input$themecolordrop)
      }
      
      if (grepl("^\\s+$", input$ylab) ){
        p <- p + theme(
          axis.title.y=element_blank())
      }
      if (grepl("^\\s+$", input$xlab) ){
        p <- p + theme(
          axis.title.x=element_blank())
      }
      
      if (input$rotatexticks ){
        p <-  p+
          theme(axis.text.x = element_text(angle = input$xticksrotateangle,
                                           hjust = input$xtickshjust,
                                           vjust = input$xticksvjust) )
        
      }
      if (input$rotateyticks ){
        p <-  p+
          theme(axis.text.y = element_text(angle = input$yticksrotateangle,
                                           hjust = input$ytickshjust,
                                           vjust = input$yticksvjust) )                              
      }    
      
      if (!is.null(input$xaxiszoomin[1])&
          is.numeric(plotdata[,input$x] )&
          input$facetscalesin!="free_x"&
          input$facetscalesin!="free"
      ){
        p <- p +
          coord_cartesian(xlim= c(input$xaxiszoomin[1],input$xaxiszoomin[2])  )
      }
      
      if (input$userxzoom&
          is.numeric(plotdata[,input$x] )&
          input$facetscalesin!="free_x"&
          input$facetscalesin!="free"
      ){
        p <- p +
          coord_cartesian(xlim= c(input$lowerxin,input$upperxin)  )
      }
      
      #p <- ggplotly(p)
      p
    }
  })
  
  output$plot <- renderPlot({
    plotObject()
  })
  
  
  output$ui_plot <-  renderUI({                 
    plotOutput('plot',  width = "100%" ,height = input$height,
               click = "plot_click",
               hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
               brush = brushOpts(id = "plot_brush"))
  })
  
  output$plotinfo <- renderPrint({
    df<- recodedata5()  
    if (is.null(df)) return(NULL)
    nearPoints( reorderdata2(), input$plot_click, threshold = 5, maxpoints = 5,
                addDist = TRUE) #,xvar=input$x, yvar=input$y
  })
  
  
  output$clickheader <-  renderUI({
    df <-recodedata5()
    if (is.null(df)) return(NULL)
    h4("Clicked points")
  })
  
  output$brushheader <-  renderUI({
    df <- recodedata5()
    if (is.null(df)) return(NULL)
    h4("Brushed points")
    
  })
  
  output$plot_clickedpoints <- renderTable({
    # For base graphics, we need to specify columns, though for ggplot2,
    # it's usually not necessary.
    df<- recodedata5()  
    if (is.null(df)) return(NULL)
    
    res <- nearPoints(recodedata5(), input$plot_click, input$x, "yvalues")
    if (nrow(res) == 0|is.null(res))
      return(NULL)
    res
  })
  output$plot_brushedpoints <- renderTable({
    df<- recodedata5()  
    if (is.null(df)) return(NULL)
    res <- brushedPoints(recodedata5(), input$plot_brush, input$x,"yvalues")
    if (nrow(res) == 0|is.null(res))
      return(NULL)
    res
  })
  
  
  
  
  downloadPlotType <- reactive({
    input$downloadPlotType  
  })
  
  observe({
    plotType    <- input$downloadPlotType
    plotTypePDF <- plotType == "pdf"
    plotUnit    <- ifelse(plotTypePDF, "inches", "pixels")
    plotUnitDef <- ifelse(plotTypePDF, 7, 480)
    
    updateNumericInput(
      session,
      inputId = "downloadPlotHeight",
      label = sprintf("Height (%s)", plotUnit),
      value = plotUnitDef)
    
    updateNumericInput(
      session,
      inputId = "downloadPlotWidth",
      label = sprintf("Width (%s)", plotUnit),
      value = plotUnitDef)
    
  })
  
  
  # Get the download dimensions.
  downloadPlotHeight <- reactive({
    input$downloadPlotHeight
  })
  
  downloadPlotWidth <- reactive({
    input$downloadPlotWidth
  })
  
  # Get the download file name.
  downloadPlotFileName <- reactive({
    input$downloadPlotFileName
  })
  
  # Include a downloadable file of the plot in the output list.
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(downloadPlotFileName(), downloadPlotType(), sep=".")   
    },
    # The argument content below takes filename as a function
    # and returns what's printed to it.
    content = function(con) {
      # Gets the name of the function to use from the 
      # downloadFileType reactive element. Example:
      # returns function pdf() if downloadFileType == "pdf".
      plotFunction <- match.fun(downloadPlotType())
      plotFunction(con, width = downloadPlotWidth(), height = downloadPlotHeight())
      print(plotObject())
      dev.off(which=dev.cur())
    }
  )
  
  
}

shinyApp(ui = ui, server = server,  options = list(height = 3000))
