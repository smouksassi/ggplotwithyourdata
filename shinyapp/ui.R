fluidPage(
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
                                      h6("Combined variables can be used for colour, fill, group, size and facets. They cannot be used as X or Y variables."),
                                      
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
                                        conditionalPanel(condition = "input.userxzoom" ,uiOutput("lowerx"),uiOutput("upperx")),
                                        h6("Y Axis Zoom only works if you have one y variable and facet y scales are not set to be free. The slider has limits between your y variable min/max otherwise select manual yvalues zoom to input your own."),
                                        uiOutput("yaxiszoom"),
                                        checkboxInput('useryzoom', 'Manual y values zoom', value = FALSE),
                                        conditionalPanel(condition = "input.useryzoom" ,uiOutput("lowery"),uiOutput("uppery"))
                                        
                             ),
                             
                             tabPanel(  "Background Color and Legend(s)",
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
                                                    multiple=FALSE, selectize=TRUE,selected="vertical"),
                                        checkboxInput('sepguides', 'Separate Legend Items for Median/PI ?',value = TRUE),       
                                        checkboxInput('labelguides', 'Hide the Names of the Legend Items ?',value = FALSE),
                                        checkboxInput('customlegendtitle', 'Customization of Legend Titles, number of columns of items and reversing the legend items ?',value = FALSE),
                                        conditionalPanel(condition = "input.customlegendtitle",
                                                         textInput("customcolourtitle", label ="Colour Legend Title",value="colour"),
                                                         numericInput("legendncolcol",label = "Colour Legend N columns",value =1,min=1,max =10) ,
                                                         checkboxInput('legendrevcol', 'Reverse Colour Legend ?',value = FALSE),
                                                         checkboxInput('legendalphacol', 'Override Colour Transparency ?',value = TRUE),
                                                         textInput("customfilltitle", label ="Fill Legend Title",value="fill"),
                                                         numericInput("legendncolfill",label = "Fill Legend N columns",value =1,min=1,max =10) ,
                                                         checkboxInput('legendrevfill', 'Reverse Fill Legend?',value = FALSE),
                                                         checkboxInput('legendalphafill', 'Override Fill Transparency ?',value = FALSE),
                                                         textInput("customsizetitle", label ="Size Legend Title",value="size"),
                                                         numericInput("legendncolsize",label = "Size Legend N columns",value =1,min=1,max =10) ,
                                                         checkboxInput('legendrevsize','Reverse Size Legend ?',value = FALSE),
                                                         
                                                         selectizeInput('legendordering',
                                                                        label = paste("Drag/Drop to reorder","Colour, Fill, Size Legends"),
                                                                        choices = c("colour","fill","size"),
                                                                        selected = c("colour","fill","size"),
                                                                        multiple=TRUE,  options = list(
                                                                          plugins = list('drag_drop')
                                                                        )
                                                         ) 
                                        )
                             ),
                             tabPanel(  "Facets Options",
                                        
                                        uiOutput("facetscales"),
                                        selectInput('facetspace' ,'Facet Spaces:',c("fixed","free_x","free_y","free")),
                                        
                                        
                                        selectInput('facetordering' ,'Facet Ordering:',c(
                                          "Top to Bottom, Left to Right Ordering like a Table" ="table",
                                          "Bottom to Top, Left to Right Ordering like a Plot" ="plot"),
                                          selected="table"),
                                        
                                        
                                        conditionalPanel(condition = "!input.facetwrap" ,
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
                                                           selected="label_both")),
                                        checkboxInput('facetwrap', 'Use facet_wrap?'),
                                        conditionalPanel(condition = "input.facetwrap" ,
                                                         checkboxInput('facetwrapmultiline', 'facet_wrap strip labels on multiple lines?',value=FALSE) ),
                                        conditionalPanel(condition = "input.facetwrap" ,
                                                         checkboxInput('customncolnrow', 'Control N columns an N rows?')),
                                        conditionalPanel(condition = "input.customncolnrow" ,
                                                         h6("An error (nrow*ncol >= n is not TRUE) will show up if the total number of facets/panels
                                                            is greater than the product of the specified  N columns x N rows. Increase the N columns and/or N rows to avoid the error.
                                                            The default empty values will use ggplot automatic algorithm."),        
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
                                                                      value = 1,min=0.1,max=10,step=0.01)) 
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
                    to input in a comma separated list of new labels. By default, value1, value2,...valuen will be populated in the field.
                    Make sure to edit/type keeping the correct number of new labels otherwise the plot will not be generated as the recoding will fail.
                    You can combine levels by repeating a value with special attention to spaces e.g. 1,1,2."),
                 
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
        tabPanel("X/Y Plot"  , 
                 uiOutput('ui_plot'),
                 hr(),
                 uiOutput("clickheader"),
                 tableOutput("plot_clickedpoints"),
                 uiOutput("brushheader"),
                 tableOutput("plot_brushedpoints"),
                 #actionButton("plotButton", "Update Plot"),
                 
                 tabPanel("Types of Graphs",
                          tabsetPanel(id = "graphicaltypes",selected = "Color/Group/Split/Size/Fill Mappings (?)",
                                      tabPanel(  "Plot types, Points, Lines (?)",
                                                 
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
                                                                            checkboxInput('lineignorecol', 'Ignore Mapped Color'),
                                                                            checkboxInput('lineignoresize', 'Ignore Mapped Size')
                                                          )
                                                          
                                                   ),
                                                   column(3,
                                                          conditionalPanel( " input.line== 'Lines' ",
                                                                            sliderInput("linesize", "Lines Size:", min=0, max=4, value=c(1),step=0.1),
                                                                            selectInput('linetypes','Lines Type:',c("solid","dotted")),
                                                                            conditionalPanel( " input.lineignorecol ",
                                                                                              selectInput('colline', label ='Lines Color', choices=colors(),multiple=FALSE, selectize=TRUE,selected="black") 
                                                                            )
                                                          )
                                                   ),
                                                   column (12, h6("Points and Lines Size will apply only if Size By: in the Color Group Split Size Fill Mappings are set to None"))
                                                   
                                                 )#fluidrow
                                      ), # tabpanel
                                      tabPanel(  "Color/Group/Split/Size/Fill Mappings (?)",
                                                 fluidRow(
                                                   column (12, hr()),
                                                   column (3, uiOutput("colour"),uiOutput("group")),
                                                   column(3, uiOutput("facet_col"),uiOutput("facet_row")),
                                                   column (3, uiOutput("facet_col_extra"),uiOutput("facet_row_extra")),
                                                   column (3, uiOutput("pointsize"),uiOutput("fill")),
                                                   column (12, h6("Make sure not to choose a variable that is in the y variable(s) list otherwise you will get an error Variable not found. These variables are stacked and become yvars and yvalues.This ensures that colour/group/etc. are kept intact when you apply a new filter or recode a variable. When you combine variables all mappings will be updated so you can choose the newly formed variable and as such the previous state will be lost." ))
                                                   
                                                 )
                                      ),#tabpanel
                                      tabPanel(  "Boxplots",
                                                 fluidRow(
                                                   column (12, h6("Limited Boxplots support. Options are to be added as per users requests.")),
                                                   
                                                   column (4,
                                                           checkboxInput('boxplotaddition', 'Add a Boxplot ? (makes sense if x variable is categorical and
                                                                         you Group By a sensible choice. By default the x variable is used for grouping)'),
                                                           checkboxInput('boxplotignoregroup', 'Ignore Mapped Group ? (can me helpful to superpose a loess or median on top of the boxplot)',value = TRUE)
                                                   ),
                                                   column (4,
                                                           checkboxInput('boxplotvarwidh', "Boxes proportional to the square-roots of the number of observations ?" ),
                                                           checkboxInput('boxplotnotch', "Notched Boxes ?.
                                                                         Notches are used to compare groups; if the notches of two boxes do not overlap, this suggests that the medians are significantly different." ),
                                                           checkboxInput('boxplotshowlegend', "Show Legend ?", value=TRUE)
                                                           ),
                                                   
                                                   column(4,
                                                          checkboxInput('boxplotignorecol', 'Ignore Mapped Color'),
                                                          conditionalPanel( " input.boxplotignorecol " ,
                                                                            selectInput('boxcolline', label ='Box Outlines Color', choices=colors(),multiple=FALSE, selectize=TRUE,selected="black")
                                                          )
                                                   )
                                                   
                                                 )#fluidrow 
                                                 
                                                   ),
                                      tabPanel(  "Histograms/Density",
                                                 fluidRow(
                                                   column (12, h6("A plot of the mapped x variable
                                                                  will be produced when no y variable(s) are selected.This is still very limited. Options are to be added as per users requests.")),
                                                   
                                                   column (3,
                                                           checkboxInput('histogramaddition', 'Add a Histogram ?',value = FALSE),
                                                           checkboxInput('densityaddition', 'Add a Density Curve ?',value = TRUE)
                                                   )
                                                   )
                                                 
                                      ),
                                      
                                      
                                      #rqss quantile regression
                                      tabPanel(  "Quantile Regression (?)",
                                                 
                                                 fluidRow(
                                                   column(12,hr()),
                                                   column(3,
                                                          checkboxInput('Tauvalue', 'Dynamic and Preset Quantiles', value = FALSE),
                                                          h5("Preset Quantiles"),
                                                          checkboxInput('ninetyseventh', '97%'),
                                                          checkboxInput('up', '95%'),
                                                          checkboxInput('ninetieth', '90%'),
                                                          checkboxInput('mid', '50%', value = FALSE),
                                                          checkboxInput('tenth', '10%'),
                                                          checkboxInput('low', '5%'),
                                                          checkboxInput('third', '3%')
                                                   ),
                                                   column(5,
                                                          sliderInput("Tau", label = "Dynamic Quantile Value:",
                                                                      min = 0, max = 1, value = 0.5, step = 0.01)  ,
                                                          sliderInput("Penalty", label = "Spline sensitivity adjustment:",
                                                                      min = 0, max = 100, value = 1, step = 0.1)  ,
                                                          selectInput("Constraints", label = "Spline constraints:",
                                                                      choices = c("None"="N","Increasing"="I","Decreasing"="D","Convex"="V","Concave"="C",
                                                                                  "Convex and Increasing"="VI", "Convex and Decreasing"= "VD",
                                                                                  "Concave and Increasing"="CI","Concave and Decreasing"= "CD"),
                                                                      selected = "N")
                                                          
                                                          
                                                   ),
                                                   column(3,
                                                          checkboxInput('ignorecolqr', 'Ignore Mapped Color'),
                                                          checkboxInput('ignoregroupqr', 'Ignore Mapped Group',value = TRUE),
                                                          checkboxInput('hidedynamic', 'Hide Dynamic Quantile'),
                                                          conditionalPanel(
                                                            condition = "input.ignorecolqr" ,
                                                            selectInput('colqr', label ='QR Color', choices=colors(),multiple=FALSE, selectize=TRUE,selected="black")
                                                          ))
                                                   
                                                 )#fluidrow
                                      ),
                                      
                                      tabPanel(  "Smooth/Linear/Logistic Regressions (?)",
                                                 
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
                                      tabPanel(  "Mean CI (?)",
                                                 
                                                 
                                                 fluidRow(
                                                   column(12,hr()),
                                                   column (3, 
                                                           radioButtons("Mean", "Mean:",
                                                                        c("Mean" = "Mean",
                                                                          "Mean/CI" = "Mean/CI",
                                                                          "None" = "None") ,selected="None") 
                                                   ),
                                                   column (3,
                                                           
                                                           conditionalPanel( " input.Mean== 'Mean/CI' ",
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
                                      ), # tab panel for mean
                                      
                                      ### median PI section
                                      
                                      
                                      tabPanel(  "Median PIs (?)",
                                                 
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
                                      
                                      
                                      tabPanel(  "Kaplan-Meier (?)",
                                                 
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
                                                           conditionalPanel( " input.KM== 'KM/CI' ",
                                                                             sliderInput("KMCI", "KM CI:", min=0, max=1, value=c(0.95),step=0.01),
                                                                             sliderInput("KMCItransparency", "KM CI Transparency:", min=0, max=1, value=c(0.2),step=0.01)
                                                                             
                                                           )),
                                                   
                                                   column (3,
                                                           conditionalPanel( " input.KM!= 'None' ",
                                                                             selectInput('KMtrans', label ='KM Transformation',
                                                                                         choices=c("None" ="identity","event"="event",
                                                                                                   "cumhaz"="cumhaz","cloglog"="cloglog"),
                                                                                         multiple=FALSE, selectize=TRUE,selected="identity"),
                                                                             checkboxInput('censoringticks', 'Show Censoring Ticks?') 
                                                           )
                                                   )
                                                 )#fluidrow
                                      ) #tabpanel km
                                      ### KM section
                                      
        )#tabsetPanel
        )#tabPanel
        
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