inline_ui <- function(tag) {
  div(style = "display: inline-block", tag)
}

fluidPage(
  useShinyjs(),
  includeCSS("www/app.css"),
  includeCSS("www/table1-style.css"),
  titlePanel("Hello GHAP HBGDki Member!"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel(
          "Inputs", 
          br(),
          tags$div(
            tags$strong("Choose csv file to upload"),
            "or", actionLink("sample_data_btn", "use sample data")
          ),
          fileInput("datafile", NULL,
                    multiple = FALSE, accept = c("csv")),
          uiOutput("ycol"),uiOutput("xcol"),
          tabsetPanel(
            id = "filtercategorize",
            type = "pills",
            tabPanel(
              "Categorize/Recode", 
              uiOutput("catvar"),
              uiOutput("ncuts"),
              uiOutput("catvar2"),
              uiOutput("catvar3"),
              uiOutput("ncuts2"),
              uiOutput("asnumeric"),
              textOutput("bintext"),
              shinyjs::hidden(div(
                id = "factor_lvl_change_section",
                tags$h4("Change the labels of a variable"),
                div(id = "factor_lvl_change_placeholder"),
                actionButton("factor_lvl_change_add", "Add another variable", icon("plus"))
              ))
            ),
            
            tabPanel("Combine Two Variables",
                     h6("Combined variables can be used for colour, fill, group, size and facets. They cannot be used as X or Y variables."),
                     
                     uiOutput("pastevar")
            ),
            tabPanel(
              "Filters", 
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
            tabPanel(
              "Simple Rounding",
              uiOutput("roundvar"),
              numericInput("rounddigits",label = "N Digits",value = 0,min=0,max=10) 
            ),
            tabPanel(
              "Reorder Variables", 
              uiOutput("reordervar"),
              conditionalPanel(
                condition = "input.reordervarin!='' " ,
                selectizeInput(
                  "functionordervariable", 'By the:',
                  choices =c("Median","Mean","Minimum","Maximum") ,multiple=FALSE)
              ),
              uiOutput("variabletoorderby"),
              conditionalPanel(
                condition = "input.reordervarin!='' " ,
                checkboxInput('reverseorder', 'Reverse Order ?', value = FALSE)
              ),
              
              uiOutput("reordervar2"),
              uiOutput("reordervar2values"),
              uiOutput("catvar5"),
              textOutput("labeltext5"),
              uiOutput("nlabels5")
            ),
            tabPanel(
              "One Row by ID(s)",
              checkboxInput('filtertoonerowbyid', 'Filter to One Row by ID(s)?', value = FALSE),
              conditionalPanel(
                condition = "input.filtertoonerowbyid" ,
                uiOutput("onerowidgroup")
              )
              
            )
          ),
          hr()
        ), # tabsetPanel
        
        
        tabPanel(
          "Graph Options",
          tabsetPanel(
            id = "graphicaloptions",
            tabPanel(
              "X/Y Log /Labels",
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
            tabPanel(
              "Graph Size/Zoom",
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
            
            tabPanel(
              "Background Color and Legend(s)",
              selectInput('backgroundcol', label ='Background Color',
                          choices=c("Gray" ="grey95","White"="white","Dark Gray"="grey90"),
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
              conditionalPanel(
                condition = "input.customlegendtitle",
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
                
                selectizeInput(
                  'legendordering',
                  label = paste("Drag/Drop to reorder","Colour, Fill, Size Legends"),
                  choices = c("colour","fill","size"),
                  selected = c("colour","fill","size"),
                  multiple=TRUE,  options = list(
                    plugins = list('drag_drop')
                  )
                ) 
              )
            ),
            tabPanel(
              "Facets Options",
              
              uiOutput("facetscales"),
              selectInput('facetspace' ,'Facet Spaces:',c("fixed","free_x","free_y","free")),
              
              
              selectInput('facetordering' ,'Facet Ordering:',c(
                "Top to Bottom, Left to Right Ordering like a Table" ="table",
                "Bottom to Top, Left to Right Ordering like a Plot" ="plot"),
                selected="table"),
              
              conditionalPanel(
                condition = "!input.facetwrap" ,
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
              conditionalPanel(
                condition = "input.facetwrap" ,
                checkboxInput('facetwrapmultiline', 'facet_wrap strip labels on multiple lines?',value=FALSE)
              ),
              conditionalPanel(
                condition = "input.facetwrap" ,
                checkboxInput('customncolnrow', 'Control N columns an N rows?')
              ),
              conditionalPanel(
                condition = "input.customncolnrow" ,
                h6("An error (nrow*ncol >= n is not TRUE) will show up if the total number of facets/panels
                   is greater than the product of the specified  N columns x N rows. Increase the N columns and/or N rows to avoid the error.
                   The default empty values will use ggplot automatic algorithm."),        
                numericInput("wrapncol",label = "N columns",value =NA,min=1,max =10) ,
                numericInput("wrapnrow",label = "N rows",value = NA,min=1,max=10) 
                )
              ) ,
            
            tabPanel(
              "Reference Lines/Target",
              checkboxInput('identityline', 'Identity Line')    ,   
              checkboxInput('horizontalzero', 'Horizontol Zero Line'),
              checkboxInput('customvline1', 'Vertical Line 1'),
              conditionalPanel(condition = "input.customvline1" , 
                               numericInput("vline1",label = "",value = 1) ),
              checkboxInput('customvline2', 'Vertical Line 2'),
              conditionalPanel(condition = "input.customvline2" , 
                               numericInput("vline2",label = "",value = 1) ),
              checkboxInput('customhline1', 'Horizontal Line 1'),
              conditionalPanel(condition = "input.customhline1" , 
                               numericInput("hline1",label = "",value = 1) ),
              checkboxInput('customhline2', 'Horizontal Line 2'),
              conditionalPanel(condition = "input.customhline2" , 
                               numericInput("hline2",label = "",value = 1) ),
              checkboxInput('showtarget', 'Add Target Window', value = FALSE) ,
              conditionalPanel(condition = "input.showtarget" , 
                               numericInput("lowerytarget",label = "Lower Target Value",
                                            value = 1,min=NA,max=NA,width='50%'),
                               numericInput("uppertarget",label = "Upper Target Value",
                                            value = 1,min=NA,max=NA,width='50%'),                 
                               
                               sliderInput("targetopacity", label = "Target Opacity:",
                                           min = 0, max = 1, value = 0.7, step = 0.05)
              ),
              checkboxInput('showtargettext', 'Add Target Text', value = FALSE),
              conditionalPanel(condition = "input.showtargettext" ,
                               textInput('targettext', 'Target Text', value = "Target: XX-XXX"))
              
              
              
            ),
            tabPanel(
              "Additional Themes Options",
              sliderInput("themebasesize", "Theme Size (affects all text elements in the plot):", min=1, max=100, value=c(16),step=1),
              checkboxInput('themetableau', 'Use Tableau Colors and Fills ? (maximum of 10 colours are provided)',value=TRUE),
              conditionalPanel(condition = "input.themetableau" ,
                               h6("If you have more than 10 color groups the plot will not work and you get /Error: Insufficient values in manual scale. ## needed but only 10 provided./  Uncheck Use Tableau Colors and Fills to use default ggplot2 colors.")),
              checkboxInput('colorblind', 'Use Colour Blind Safe Colors ? (maximum of 10 colours are provided)',value=TRUE) , 
              conditionalPanel(condition = "input.colorblind" ,
                               h6("This will override the Tableau colors
                                  if checed and If you have more than 8 color groups the plot will not work
                                  and you get /Error: Insufficient values in manual scale. ## needed but only 8 provided./  Uncheck to use default ggplot2 colors.")),
              
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
        
        tabPanel(
          "How To",
          includeMarkdown(file.path("text", "howto.md"))
        )# tabpanel 
          )
      ), #sidebarPanel
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "X/Y Plot"  , 
          uiOutput('ui_plot'),
          shinyjs::hidden(div(
            id = "update_plot_area",
            inline_ui(
              checkboxInput("auto_update_plot",
                            "Update plot automatically", value = TRUE)
            ),
            actionButton("update_plot_btn", "Update plot",
                         icon = icon("refresh"))
          )),
          shinyjs::hidden(div(
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
          )),
          hr(),
          uiOutput("clickheader"),
          tableOutput("plot_clickedpoints"),
          uiOutput("brushheader"),
          tableOutput("plot_brushedpoints"),
          
          tabPanel(
            "Types of Graphs",
            tabsetPanel(
              id = "graphicaltypes",selected = "Color/Group/Split/Size/Fill Mappings (?)",
              tabPanel(
                "Plot types, Points, Lines (?)",
                
                fluidRow(
                  
                  column (12, hr()),
                  column (
                    3,
                    radioButtons("Points", "Points/Jitter:",
                                 c("Points" = "Points",
                                   "Jitter" = "Jitter",
                                   "None" = "None")),
                    conditionalPanel(
                      " input.Points!= 'None' ",
                      sliderInput("pointstransparency", "Points Transparency:", min=0, max=1, value=c(0.5),step=0.01),
                      checkboxInput('pointignorecol', 'Ignore Mapped Color')
                    )
                  ),
                  column(
                    3,
                    conditionalPanel(
                      " input.Points!= 'None' ",
                      sliderInput("pointsizes", "Points Size:", min=0, max=4, value=c(1),step=0.1),
                      numericInput('pointtypes','Points Type:',16, min = 1, max = 25),
                      conditionalPanel(
                        " input.pointignorecol ",
                        selectInput('colpoint', label ='Points Color', choices=colors(),
                                    multiple=FALSE, selectize=TRUE, selected="black") 
                      )
                    )
                  ),
                  column(
                    3,
                    radioButtons("line", "Lines:",
                                 c("Lines" = "Lines",
                                   "None" = "None"),selected="None"),
                    conditionalPanel(
                      " input.line== 'Lines' ",
                      sliderInput("linestransparency", "Lines Transparency:", min=0, max=1, value=c(0.5),step=0.01),
                      checkboxInput('lineignorecol', 'Ignore Mapped Color'),
                      checkboxInput('lineignoresize', 'Ignore Mapped Size')
                    )
                  ),
                  column(
                    3,
                    conditionalPanel(
                      " input.line== 'Lines' ",
                      sliderInput("linesize", "Lines Size:", min=0, max=4, value=c(1),step=0.1),
                      selectInput('linetypes','Lines Type:',c("solid","dotted")),
                      conditionalPanel(
                        " input.lineignorecol ",
                        selectInput('colline', label ='Lines Color', choices=colors(),
                                    multiple=FALSE, selectize=TRUE,selected="black") 
                      )
                    )
                  ),
                  column (12,
                          h6("Points and Lines Size will apply only if Size By: in the Color Group Split Size Fill Mappings are set to None"))
                  
                )#fluidrow
              ), # tabpanel
              tabPanel(
                "Color/Group/Split/Size/Fill Mappings (?)",
                fluidRow(
                  column (12, hr()),
                  column (3, uiOutput("colour"),uiOutput("group")),
                  column(3, uiOutput("facet_col"),uiOutput("facet_row")),
                  column (3, uiOutput("facet_col_extra"),uiOutput("facet_row_extra")),
                  column (3, uiOutput("pointsize"),uiOutput("fill")),
                  column (12, h6("Make sure not to choose a variable that is in the y variable(s) list otherwise you will get an error Variable not found. These variables are stacked and become yvars and yvalues.This ensures that colour/group/etc. are kept intact when you apply a new filter or recode a variable. When you combine variables all mappings will be updated so you can choose the newly formed variable and as such the previous state will be lost." ))
                  
                )
              ),#tabpanel
              tabPanel(
                "Boxplots",
                fluidRow(
                  column (12, h6("Limited Boxplots support. Options are to be added as per users requests.")),
                  
                  column (
                    4,
                    checkboxInput('boxplotaddition', 'Add a Boxplot ? (makes sense if x variable is categorical and
                                  you Group By a sensible choice. By default the x variable is used for grouping)'),
                    checkboxInput('boxplotignoregroup', 'Ignore Mapped Group ? (can me helpful to superpose a loess or median on top of the boxplot)',value = TRUE)
                  ),
                  column (
                    4,
                    checkboxInput('boxplotvarwidh', "Boxes proportional to the square-roots of the number of observations ?" ),
                    checkboxInput('boxplotnotch', "Notched Boxes ?.
                                  Notches are used to compare groups; if the notches of two boxes do not overlap, this suggests that the medians are significantly different." ),
                    checkboxInput('boxplotshowlegend', "Show Legend ?", value=TRUE)
                    ),
                  
                  column(
                    4,
                    checkboxInput('boxplotignorecol', 'Ignore Mapped Color'),
                    conditionalPanel(
                      " input.boxplotignorecol " ,
                      selectInput('boxcolline', label ='Box Outlines Color',
                                  choices=colors(),multiple=FALSE, selectize=TRUE,selected="black")
                    )
                  )
                  
                )#fluidrow 
                
                  ),
              tabPanel(
                "Histograms/Density/Bar",
                value = "histograms_density",
                fluidRow(
                  column (12, h6("A plot of the mapped x variable
                                 will be produced when no y variable(s) are selected.This is still limited. Options are to be added as per users requests.")),
                  
                  column (
                    3,
                    checkboxInput('histogramaddition', 'Add a Histogram ?',value = FALSE)
                    ),
                  column (
                    3,
                    checkboxInput('densityaddition', 'Add a Density Curve ?',value = TRUE)
                  ),
                  
                  column (
                    3,
                    checkboxInput('barplotaddition', 'Add a Barplot ?',value = TRUE),
                    selectInput("positionbar", label = "Bar positioning:",
                                choices = c("Stacked"="position_stack(vjust = 0.5)",
                                            "Side By Side"="position_dodge(width = 0.9)",
                                            "Sum to 100%"="position_fill(vjust = 0.5)"),
                                selected = "position_stack(vjust = 0.5)"),
                    checkboxInput('barplotpercent', 'Show Percentage instead of Counts ?',value = FALSE),
                    checkboxInput('barplotlabel', 'Show Labels ?',value = FALSE),
                    checkboxInput('barplotflip', 'Flip the Barplot ?',value = FALSE)
                    
                  )
                  )
              ),
              
              
              #rqss quantile regression
              tabPanel(
                "Quantile Regression (?)",
                
                fluidRow(
                  column(12,hr()),
                  column(
                    3,
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
                  column(
                    5,
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
                  column(
                    3,
                    checkboxInput('ignorecolqr', 'Ignore Mapped Color'),
                    checkboxInput('ignoregroupqr', 'Ignore Mapped Group',value = TRUE),
                    checkboxInput('hidedynamic', 'Hide Dynamic Quantile'),
                    conditionalPanel(
                      condition = "input.ignorecolqr" ,
                      selectInput('colqr', label ='QR Color', choices=colors(),multiple=FALSE, selectize=TRUE,selected="black")
                    ))
                  
                )#fluidrow
              ),
              
              tabPanel(
                "Smooth/Linear/Logistic Regressions (?)",
                
                fluidRow(
                  column(12,hr()),
                  column (
                    3, 
                    radioButtons("Smooth", "Smooth:",
                                 c("Smooth" = "Smooth",
                                   "Smooth and SE" = "Smooth and SE",
                                   "None" = "None"),selected="None")
                  ),
                  column (
                    3, 
                    conditionalPanel(
                      " input.Smooth!= 'None' ",
                      selectInput('smoothmethod', label ='Smoothing Method',
                                  choices=c("Loess" ="loess","Linear Fit"="lm","Logistic"="glm"),
                                  multiple=FALSE, selectize=TRUE,selected="loess"),
                      
                      sliderInput("loessens", "Loess Span:", min=0, max=1, value=c(0.75),step=0.05),
                      selectInput('loessfamily', label ='Loess Family:',
                                  choices=c("Gaussian" ="gaussian","Symmetric"="symmetric"),
                                  multiple=FALSE, selectize=TRUE,selected="gaussian"),
                      sliderInput("loessdegree", "Loess Degree:", min=0, max=2, value=c(1),step=1)
                      
                    ) 
                  ),
                  
                  column (
                    3,  conditionalPanel( " input.Smooth!= 'None' ",
                                          checkboxInput('ignorecol', 'Ignore Mapped Color'),
                                          conditionalPanel(
                                            " input.ignorecol ",
                                            selectInput('colsmooth', label ='Smooth Color', choices=colors(),
                                                        multiple=FALSE, selectize=TRUE,selected="black") )
                                         
                    )
                  ),
                  column (
                    3, conditionalPanel(
                      " input.Smooth!= 'None' ",
                      checkboxInput('ignoregroup', 'Ignore Mapped Group',value = TRUE)
                    ) ,
                    uiOutput("weight"))
                  
                )#fluidrow
              )
              ,
              ### Mean CI section
              tabPanel(
                "Mean CI (?)",
                
                fluidRow(
                  column(12,hr()),
                  column (3, 
                          radioButtons("Mean", "Mean:",
                                       c("Mean" = "Mean",
                                         "Mean/CI" = "Mean/CI",
                                         "None" = "None") ,selected="None") 
                  ),
                  column (
                    3,
                    
                    conditionalPanel(
                      " input.Mean== 'Mean/CI' ",
                      sliderInput("CI", "CI %:", min=0, max=1, value=c(0.95),step=0.01),
                      numericInput( inputId = "errbar",label = "CI bar width:",value = 2,min = 1,max = NA)      
                    )
                    
                  ),
                  column (
                    3,
                    conditionalPanel(
                      " input.Mean!= 'None' ",
                      checkboxInput('meanpoints', 'Show points') ,
                      checkboxInput('meanlines', 'Show lines', value=TRUE),
                      checkboxInput('meanignorecol', 'Ignore Mapped Color') ,
                      conditionalPanel( " input.meanignorecol ",
                                        selectInput('colmean', label ='Mean Color', choices=colors(),multiple=FALSE, selectize=TRUE,selected="black") )
                      
                    ) ),
                  
                  
                  column(
                    3,
                    conditionalPanel(
                      " input.Mean!= 'None' ",
                      checkboxInput('meanignoregroup', 'Ignore Mapped Group',value = TRUE),
                      sliderInput("meanlinesize", "Mean(s) Line(s) Size:", min=0, max=3, value=1,step=0.05)
                    ) 
                  )
                ) #fluidrow
              ), # tab panel for mean
              
              ### median PI section
              
              
              tabPanel(
                "Median PIs (?)",
                
                fluidRow(
                  column(12,hr()),
                  column (
                    3,
                    radioButtons("Median", "Median:",
                                 c("Median" = "Median",
                                   "Median/PI" = "Median/PI",
                                   "None" = "None") ,selected="None") ,
                    conditionalPanel( " input.Median!= 'None' ",
                                      checkboxInput('medianvalues', 'Label Values?') ,
                                      checkboxInput('medianN', 'Label N?') )
                    
                  ),
                  column (
                    3,
                    conditionalPanel( " input.Median== 'Median' ",
                                      checkboxInput('medianpoints', 'Show points') ,
                                      checkboxInput('medianlines', 'Show lines',value=TRUE)),
                    conditionalPanel( " input.Median== 'Median/PI' ",
                                      sliderInput("PI", "PI %:", min=0, max=1, value=c(0.95),step=0.01),
                                      sliderInput("PItransparency", "PI Transparency:", min=0, max=1, value=c(0.2),step=0.01)
                    )
                  ),
                  column (
                    3,
                    conditionalPanel( " input.Median!= 'None' ",
                                      checkboxInput('medianignorecol', 'Ignore Mapped Color'),
                                      conditionalPanel(
                                        " input.medianignorecol ",
                                        selectInput('colmedian', label ='Median Color', choices=colors(),multiple=FALSE, selectize=TRUE,selected="black") )
                                      
                    ) ),
                  column (
                    3,
                    conditionalPanel(
                      " input.Median!= 'None' ",
                      
                      checkboxInput('medianignoregroup', 'Ignore Mapped Group',value = TRUE),
                      sliderInput("medianlinesize", "Median(s) Line(s) Size:", min=0, max=4, value=c(1),step=0.1)
                      
                    )
                  )
                  
                )#fluidrow
              ),
              ### median PI section
              
              ### KM section
              
              
              tabPanel(
                "Kaplan-Meier (?)",
                
                fluidRow(
                  column(12,hr()),
                  column (12, h6("KM curves support is currently experimental some features might not work. When a KM curve is added nothing else will be plotted (e.g. points, lines etc.).Color/Fill/Group/Facets are expected to work." )),
                  column (
                    3,
                    radioButtons("KM", "KM:",
                                 c("KM" = "KM",
                                   "KM/CI" = "KM/CI",
                                   "None" = "None") ,selected="None") 
                  ),
                  column (
                    3,
                    conditionalPanel(
                      " input.KM== 'KM/CI' ",
                      sliderInput("KMCI", "KM CI:", min=0, max=1, value=c(0.95),step=0.01),
                      sliderInput("KMCItransparency", "KM CI Transparency:", min=0, max=1, value=c(0.2),step=0.01)
                      
                    )),
                  
                  column (
                    3,
                    conditionalPanel(
                      " input.KM!= 'None' ",
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
        tabPanel(
          "Export Plots", 
          conditionalPanel(
            condition = "!output.saved_plots_exist",
            h2("You do not have any saved plots to export")
          ),
          conditionalPanel(
            condition = "output.saved_plots_exist",
            fluidRow(
              column(
                4,
                h2("Export Options"),
                div(
                  id = "exporting_plots_options",
                  selectInput("export_file_type", "File type",
                              c("PDF" = "pdf", "JPEG" = "jpeg", "PNG" = "png")),
                  conditionalPanel(
                    condition = "input.export_file_type == 'pdf'",
                    selectInput("export_pdf_orientation", "Page orientation",
                                c("Portrait (8.5\" x 11\")" = "portrait",
                                  "Landscape (11\" x 8.5\")" = "landscape",
                                  "Custom dimensions" = "custom")
                    ),
                    conditionalPanel(
                      condition = "input.export_pdf_orientation == 'custom'",
                      numericInput("export_pdf_width", "Page width (inches)",
                                   value = 8.5, min = 1, max = 50, step = 0.5),
                      numericInput("export_pdf_height", "Page height (inches)",
                                   value = 11, min = 1, max = 50, step = 0.5)
                    )
                  ),
                  conditionalPanel(
                    condition = "input.export_file_type != 'pdf'",
                    numericInput("export_file_width", "Image width (pixels)",
                                 value = 480, min = 100, max = 2000),
                    numericInput("export_file_height", "Image height (pixels)",
                                 value = 480, min = 100, max = 2000)
                  ),
                  checkboxInput("export_multiple", "Multiple plots per page"),
                  conditionalPanel(
                    condition = "input.export_multiple",
                    selectInput("export_arrangement", NULL,
                                c("Arrange plots by row" = "byrow",
                                  "Arrange plots by column" = "bycol")),
                    numericInput("export_nrow", "Rows per page",
                                 value = 1, min = 1, max = 20),
                    numericInput("export_ncol", "Columns per page",
                                 value = 1, min = 1, max = 20)
                    
                  ),
                  uiOutput("export_btn_ui")
                )
              ),
              column(
                8,
                h2("Preview"),
                strong("Remove plot"), br(),
                inline_ui(uiOutput("plots_remove_ui")),
                actionButton("remove_plot_btn", "Remove"),
                uiOutput("plots_order_ui"),
                div(
                  id = "preview_plots_options",
                  uiOutput("plots_select_page_ui"),
                  plotOutput("plot_preview", height = "auto")
                )
              )
            )
          )
        ),
        
        tabPanel("Descriptive Stats",
                 p("Note: use y for variables of interest (rows) and x for stratification (columns). Drag and Drop the y variable(s) list on the left to the order of your liking"),
                 htmlOutput("dstats"),
                 shinyjs::hidden(div(
                     id = "table_options_area",
                     inline_ui(
                         checkboxInput("auto_update_table",
                                       "Update table automatically", value = TRUE)
                         ),
                     actionButton("update_table_btn", "Update table",
                                  icon = icon("refresh")),
                     fluidRow(
                       column(3,
                         div(id="quick_relabel_placeholder")
                       ),
                       column(3,
                              div(id="quick_reorder_placeholder")
                       ),
                       column(6,
                         checkboxInput("table_incl_overall",
                                       label="Include Overall column?",
                                       value=TRUE),
                         selectInput("table_style",
                           label="Style",
                           choices=c("Default"="t1default",
                                     "Zebra"="t1zebra",
                                     "Grid"="t1grid")),
                         selectizeInput("dstats_cont_list",
                           label="Statistics to display for continuous variables (per line)",
                           choices=allstats,
                           selected=c("Mean (SD)", "Median [Min, Max]"),
                           multiple=TRUE,
                           options=list(plugins=list('drag_drop','remove_button'))),
                         numericInput("dstats_sigfig",
                           label="Number of significant figures (for Mean, SD, ...)",
                           value=3, min=1, max=10, step=1),
                         checkboxInput("round_median_min_max",
                                       label="Also round median, min, max?",
                                       value=TRUE)
                       )
                     )
                 ))
        ),

        tabPanel(
          'Data',
          dataTableOutput("mytablex") 
        ),#tabPanel2
        
        tabPanel(
          'Plot Code',
          h5("Plot reproducibility initial support. To reproduce a plot, in the data tab save the plotdata into a csv, read back to R naming it plotdata then copy paste the code below. Some inputs might not be yet supported we will be adding those during the coming weeks."),
          verbatimTextOutput("plotcode")
        )
    )#tabsetPanel
      )#mainPanel
    )#sidebarLayout
)#fluidPage
