#The great shiny app revamp

#EMAApp1.00

#Plan is to code directly from dataset into app.

#Will start with EMA Master dataset


#Next steps: Add in scatterplot and subject look-up



#Shiny App
library(shiny)
library(tidyverse)
library(DT)
library(grid)
library(plyr)
library(shinythemes)
library(plotly)
library(Hmisc)
library(lazyeval)
library(mosaic)

#options(warn = -1)



#setwd('C:\\Users\\Mike\\Documents\\Rstuff\\ShinyApps\\EMAApp1.0')

#UI#############################################################################################

ui <- navbarPage("EMA Explorer 1.0", theme = shinytheme("cosmo"),
                 
                 tabPanel("Dataset",
                          
                          pageWithSidebar(
                            
                            headerPanel("Upload and Select EMA Dataset"),
                            
                            sidebarPanel(width = 3, 
                                         
                                         h4('Select dataset:'),  
                                         
                                         selectInput("dataset", 
                                                     label = NULL, 
                                                     choices = c("NIMH Merged", "NIMH Palm", "NIMH Droid", "NCCR", "CoLaus", "Upload"),
                                                     selected = "NIMH Merged", multiple = FALSE,
                                                     selectize = TRUE, width = NULL, size = NULL),
                                         actionButton("go", "Select Dataset"),
                                         
                                         hr(), 
                                         
                                         #Upload file 1
                                         fileInput("file1", "Upload EMA Dataset",
                                                   multiple = FALSE,
                                                   accept = c("text/csv",
                                                              "text/comma-separated-values,text/plain",
                                                              ".csv")),
                                         #Upload file 2
                                         fileInput("file2", "Upload EMA Max Values",
                                                   multiple = FALSE,
                                                   accept = c("text/csv",
                                                              "text/comma-separated-values,text/plain",
                                                              ".csv")),
                                         
                                         #Upload file 3
                                         fileInput("file3", "Upload Covariates",
                                                   multiple = FALSE,
                                                   accept = c("text/csv",
                                                              "text/comma-separated-values,text/plain",
                                                              ".csv"))
                                         
                                         
                            ),
                            
                            mainPanel(width = 9)
                          )
                        ),
                 
                 tabPanel("Compliance",
                           
                           pageWithSidebar(
                            
                           headerPanel("Visualizing EMA Compliance"),
                           
                           sidebarPanel(width = 2,
                                        
                                        h4('Adjust compliance criteria:'), 
                                        
                                        sliderInput("num", 
                                                    label = "Threshold", value = 0.5, min = 0, max = 1), 
                                        
                                        hr(),
                                        
                                        conditionalPanel(condition = "input.conditionedPanels == 'Compliance Barplot'",
                                        
                                          h4('Barplot Options:'), 
                                        
                                          sliderInput("slider", 
                                                      label = "View variables with frequency between:", 
                                                      min = 0, max = 100, value = c(0,100)),
                                        
                                          selectInput("sort", 
                                                      label = "Sort By:", 
                                                      choices = list("Original Order"= 1, "Freq Perc" = 2, "Alphabetical" = 3), 
                                                      selected = 1)
                                        ),
                                        
                                        conditionalPanel(condition = "input.conditionedPanels == 'Compliance Boxplot'",
                                                        
                                                         h4('Main Variable Options'),
                                                         
                                                         uiOutput("boxplotvar"),
                                                         
                                                         hr(),
                                                         
                                                         h4('Color Variable Options'), 
                                                         
                                                         uiOutput("boxplotcolor"),
                                                         
                                                         radioButtons("boxplotradio", "Quantiles:",
                                                                      c("Auto", "On", "Off"), inline = T),
                                                         
                                                         sliderInput("boxplotntile", "Number of Quantiles:",
                                                                     min = 2, max = 10,
                                                                     value = 4, step = 1),
                                                         hr(),
                                                         
                                                         h4('Random Variable Options'),
                                                         
                                                         checkboxGroupInput("boxplotrand_choice", label = "Choose Random Inputs:", choices = c("Main Var.", "Color Var."), selected = c("Main Var.", "Color Var."),
                                                                            inline = T, width = 300), 
                                                         actionButton("boxplotrandom", "Random Vars"),
                                                         
                                                         hr(),
                                                         
                                                         actionButton("boxplot1", "Create/Update Plot")
                                        
                                        ),
                                        
                                        conditionalPanel(condition = "input.conditionedPanels == 'Compliance Heatmap'",
                                                         
                                                         h4('Main Variable Options'),
                                                         
                                                         uiOutput("heatmapvar"),
                                                         
                                                         hr(),
                                                         
                                                         h4('Ordering Variable Options'), 
                                                         
                                                         uiOutput("heatmaporder"),
                                                         
                                                         hr(),
                                                         
                                                         h4('Plot 2 Options'),
                                                         
                                                         selectInput("heatmapraw", 
                                                                     label = "Data Type:", 
                                                                     choices = c("raw", "subject normalized"),
                                                                     selected = "raw"),
                                                         
                                                         selectInput("heatmapstrat", 
                                                                     label = "Stratify By:", 
                                                                     choices = c("timeofday", "weekday"),
                                                                     selected = "timeofday"),
                                                         
                                                         hr(),
                                                         
                                                         h4('Random Variable Options'),
                                                         
                                                         checkboxGroupInput("heatmaprand_choice", label = "Choose Random Inputs:", choices = c("Main Var.", "Order Var."), selected = c("Main Var.", "Order Var."),
                                                                            inline = T, width = 300), 
                                                         actionButton("heatmaprandom", "Random Vars"),
                                                         
                                                         hr(),
                                                         
                                                         actionButton("heatmap1", "Create/Update Plot")
                                                         
                                                         
                                                         
                                        )
                                        
                           ),
                           
                           mainPanel(
                        
                            tabsetPanel(id = "conditionedPanels", 
                              
                              tabPanel("Compliance Barplot", 

                                fluidRow(
                                      
                                  column(8,
                                         plotOutput("Bar")),
                                      
                                  column(1),
                                      
                                  column(3,
                                         h6("Quick summary:"),
                                         verbatimTextOutput("median"),
                                         h6("Get names:"),
                                         verbatimTextOutput("namespalm"))
                                           
                                )
                              ),
                              
                              tabPanel("Compliance Boxplot", 

                                fluidRow(
                                           
                                  column(3,
                                         div(style = "height:25px"),
                                         div(DT::dataTableOutput("table1"), style = "font-size: 75%; width: 75%")),
                                 
                                
                                           
                                  column(9,
                                         div(style = "height:10px"),
                                         verbatimTextOutput("boxplot_instr"),
                                         plotlyOutput("boxplot", height = 350),
                                         plotlyOutput("boxplotTOD", height = 200),
                                         div(style = "height:25px"))
                                           
                                  #column(4,
                                         #h6("Survey Question/Variable Description:"),
                                         #verbatimTextOutput("palmq")),
                                      
                                  #column(4, 
                                         #h6("Response:"),
                                         #verbatimTextOutput("palmr"))
                                )
                              ),
  
                              tabPanel("Compliance Heatmap", 

                                fluidRow(
                                          
                                  column(3,
                                         div(style = "height:25px"),
                                         div(DT::dataTableOutput("table2"), style = "font-size: 75%; width: 75%")),
                                  
                            
                                  
                                  column(9,
                                         verbatimTextOutput("heatmap_instr")),
                                  
                                  column(2,
                                         plotlyOutput("heatmap", height = 600)),
                                       
                                  column(7,
                                         plotlyOutput("heatmapTOD", height = 600))
                                  
                            
                                )
                              )
                            )
                          )
                        )
                 ),
                 
                 tabPanel("Responses",
                          
                          pageWithSidebar(
                            
                            headerPanel("Visualizing EMA Responses"),
                                        
                            sidebarPanel(width = 2,
                                         
                                         conditionalPanel(condition = "input.conditionedPanels2 == 'Response Histogram'",
                                                          
                                                          h4('Main Variable Options'),
                                                          
                                                          uiOutput("rhistvar"),
                                                          
                                                          hr(),
                                                          
                                                          h4('Color Variable Options'), 
                                                          
                                                          uiOutput("rhistcolor"),
                                                          
                                                          radioButtons("rhistradio", "Quantiles:",
                                                                       c("Auto", "On", "Off"), inline = T),
                                                          
                                                          sliderInput("rhistntile", "Number of Quantiles:",
                                                                      min = 2, max = 10,
                                                                      value = 4, step = 1),
                                                          hr(),
                                                          
                                                          h4('Random Variable Options'),
                                                          
                                                          checkboxGroupInput("rhistrand_choice", label = "Choose Random Inputs:", choices = c("Main Var.", "Color Var."), selected = c("Main Var.", "Color Var."),
                                                                             inline = T, width = 300), 
                                                          actionButton("rhistrandom", "Random Vars"),
                                                          
                                                          hr(),
                                                          
                                                          actionButton("rhist1", "Create/Update Plot")
                                                          
                                                      
                                         ),
                                         
                                         conditionalPanel(condition = "input.conditionedPanels2 == 'Response Boxplot'",
                                                          
                                                          h4('Main Variable Options'),
                                                          
                                                          uiOutput("rboxplotvar"),
                                                          
                                                          hr(),
                                                          
                                                          h4('Color Variable Options'), 
                                                          
                                                          uiOutput("rboxplotcolor"),
                                                          
                                                          radioButtons("rboxplotradio", "Quantiles:",
                                                                       c("Auto", "On", "Off"), inline = T),
                                                          
                                                          sliderInput("rboxplotntile", "Number of Quantiles:",
                                                                      min = 2, max = 10,
                                                                      value = 4, step = 1),
                                                          hr(),
                                                          
                                                          h4('Random Variable Options'),
                                                          
                                                          checkboxGroupInput("rboxplotrand_choice", label = "Choose Random Inputs:", choices = c("Main Var.", "Color Var."), selected = c("Main Var.", "Color Var."),
                                                                                                                                                  inline = T, width = 300), 
                                                          actionButton("rboxplotrandom", "Random Vars"),
                                                          
                                                          hr(),
                                                          
                                                          actionButton("rboxplot1", "Create/Update Plot")
                                                          
                                                          
                                                          
                                                         
                                                          
                                         ),
                                         
                                         conditionalPanel(condition = "input.conditionedPanels2 == 'Response Heatmap'",
                                                          
                                                          h4('Main Variable Options'),
                                                          
                                                          uiOutput("rheatmapvar"),
                                                          
                                                          hr(),
                
                                                          h4('Ordering Variable Options'), 
                                                          
                                                          uiOutput("rheatmaporder"),
                                                          
                                                          hr(),
                                                          
                                                          h4('Plot 2 Options'),
                                                          
                                                          selectInput("rheatmapraw", 
                                                                      label = "Data Type:", 
                                                                      choices = c("raw", "subject normalized"),
                                                                      selected = "raw"),
                                                          
                                                          selectInput("rheatmapstrat", 
                                                                      label = "Stratify By:", 
                                                                      choices = c("timeofday", "weekday"),
                                                                      selected = "timeofday"),
                                                          
                                                          hr(),
                                                          
                                                          h4('Random Variable Options'),
                                                          
                                                          checkboxGroupInput("rheatmaprand_choice", label = "Choose Random Inputs:", choices = c("Main Var.", "Order Var."), selected = c("Main Var.", "Order Var."),
                                                                             inline = T, width = 300), 
                                                          actionButton("rheatmaprandom", "Random Vars"),
                                                          
                                                          hr(),
                                                          
                                                          actionButton("rheatmap1", "Create/Update Plot")
                                                      
                                                          
                                         ),
                                         
                                         conditionalPanel(condition = "input.conditionedPanels2 == 'Response Trajectory'",
                                                                           
                                                                           h4('Plot Options'),
                                                                           
                                                                           selectInput("rtrajaxis", 
                                                                                       label = "X-Axis Time Variable:", 
                                                                                       choices = c("timepoint", "weekday_n", "weektime_n", "day"),
                                                                                       selected = "timepoint"),
                                                                           
                                                                           radioButtons("rtrajtraces", "Plot Line Type:",
                                                                                        c("Group Means", "Subject Traces"), inline = T),
                                                                           
                                                                           hr(),
                                                                          
                                                                           h4('Main Variable Options'),
                                                                           
                                                                           uiOutput("rtrajvar"),
                                                                           
                                                                           radioButtons("rtrajraw", "Data Type:",
                                                                                        c("Raw", "Subject Normalized"), inline = T, selected = "Subject Normalized"),
                                                                           
                        
                                                                           hr(),
                                                                           
                                                                           h4('Color Variable Options'), 
                                                                           
                                                                           uiOutput("rtrajcolor"),
                                                                           
                                                                           radioButtons("rtrajradio", "Quantiles:",
                                                                                        c("Auto", "On", "Off"), inline = T),
                                                                           
                                                                           sliderInput("rtrajntile", "Number of Quantiles:",
                                                                                       min = 2, max = 10,
                                                                                       value = 4, step = 1),
                                                                           hr(),
                                                                       
                                                                           
                                                                           actionButton("rtraj1", "Create/Update Plot")
                                                                          
                                                          ),
                                         
                                         conditionalPanel(condition = "input.conditionedPanels2 == 'Response Scatterplot'",
                                                          
                                                          h4("Main Variables Options"),
                                                          
                                                          uiOutput("scatterplot_y_var"),
                                                          
                                                          uiOutput("scatterplot_x_var"),
                                                          
                                                          uiOutput("scatterplotraw"),
                                                          
                                                          radioButtons("scatterplotlevel", "Main Variables Level:", c("Subject", "Day", "Assessment"), selected = "Subject", inline = T),
                                                          
                                                          hr(),
                                                          
                                                          h4("Color Variable Options"),
                                                          
                                                          uiOutput("scatterplotcolor"),
                                                          
                                                          radioButtons("scatterplotcolortype", 
                                                                       label = "Color Variable Type:", 
                                                                       choices = c("Raw", "Subject Normalized"),
                                                                       selected = "Raw", inline = T),
                                                          
                                                          uiOutput("scatterplotcolorlevel"),
                                                          
                                                          
                                                          radioButtons("scatterplotradio", "Quantiles:",
                                                                       c("Auto", "On", "Off"), inline = T),
                                                          
                                                          sliderInput("scatterplotntile", "Number of Quantiles:",
                                                                      min = 2, max = 10,
                                                                      value = 4, step = 1),
                                                          
                                                          hr(),
                                        
                                                          actionButton("scatterplot1", "Create/Update Plot")
                                                      
                                                          
                                                          )
                                                          
                                                          
                                                          
                                         ),
                                         
                            
                            
                            mainPanel(
                              
                              tabsetPanel(id = "conditionedPanels2",
                                          
                                          tabPanel("Response Histogram", 
                                                   
                                                   fluidRow(
                                                     
                                                     column(
                                                       3, div(DT::dataTableOutput("table3"), style = "font-size: 75%; width: 75%")),
                                                     
                                                     
                                                     column(9,
                                                            div(style = "height:10px"),
                                                            verbatimTextOutput("rhist_instr"),
                                                            div(plotlyOutput("histR", height = 500)),
                                                            div(style = "height:10px"),
                                                            div(plotlyOutput("missinghistR", height = 200)))
                                                     
                                               
                                                     
                                                   )
                                          ),
                                          
                                          tabPanel("Response Boxplot",
                                                   fluidRow(
                                                     column(3,
                                                            div(style = "height:25px"),
                                                            div(DT::dataTableOutput("table4"), style = "font-size: 75%; width: 75%")),
                                                     
                                                   
                                                     
                                                     column(9,
                                                            div(style = "height:10px"),
                                                            verbatimTextOutput("rboxplot_instr"),
                                                            plotlyOutput("boxplot_dR", height = 350),
                                                            plotlyOutput("boxplotR", height = 200),
                                                            div(style = "height:25px"))
                                                            
                                                   )
                                          ), 
                                          
                                          
                                          tabPanel("Response Heatmap",
                                                   fluidRow(
                                                     
                                                     column(3,
                                                            div(style = "height:25px"),
                                                            div(DT::dataTableOutput("table5"), style = "font-size: 75%; width: 75%")),
                                                     
                                                     column(1),
                                                     
                                                     column(8,
                                                            div(style = "height:10px"),
                                                            verbatimTextOutput("rheatmap_instr")),
                                                  
                                                     column(2,
                                                            plotlyOutput("heatmapR", height = 600)),
                                                     
                                                     column(6,
                                                            plotlyOutput("heatmapTODR", height = 600))
                                                     
                                                    #column(4,
                                                            #h6("Survey Question/Variable Description:"),
                                                            #verbatimTextOutput("code5")),
                                                     
                                                     #column(4, 
                                                            #h6("Response:"),
                                                            #verbatimTextOutput("code6"))
                                                     
                                                   )
                                           ),
                          
                                       
                                          tabPanel("Response Trajectory",
                                                   fluidRow(
                                                     
                                                     column(3,
                                                            div(style = "height:25px"),
                                                            div(DT::dataTableOutput("table6"), style = "font-size: 75%; width: 75%")),
                                                     
                                                     column(9,
                                                            div(style = "height:10px"),
                                                            div(style="display: inline-block;vertical-align:top; width: 300px;", checkboxGroupInput("rtrajrand_choice", label = "Choose Random Inputs:", choices = c("Main Var.", "Color Var."), selected = c("Main Var.", "Color Var."),
                                                                                                                                                    inline = T, width = 300)), 
                                                            
                                                            div(style="display: inline-block;vertical-align:top; width: 200px;", actionButton("rtrajrandom", "Random Vars")),
                                                            verbatimTextOutput("rtraj_instr"),
                                                            plotlyOutput("trajR", height = 700))
                                                   )
                                          ),
                                          
                                          tabPanel("Response Scatterplot",
                                                   fluidRow(
                                                     column(3,
                                                            div(style = "height:25px"),
                                                            div(DT::dataTableOutput("table7"), style = "font-size: 75%; width: 75%")),
                                                     
                                                
                                                     
                                                     column(9,
                                                            div(style = "height:10px"),
                                                            div(style="display: inline-block;vertical-align:top; width: 300px;", checkboxGroupInput("scatterplotrand_choice", label = "Choose Random Inputs:", choices = c("Y-Axis Var.", "X-Axis Var.", "Color Var."), selected = c("Y-Axis Var.", "X-Axis Var.", "Color Var."),
                                                                                                                                                    inline = T, width = 300)), 
                                                                
                                                            div(style="display: inline-block;vertical-align:top; width: 200px;", actionButton("scatterplotrandom", "Random Vars")),
                                                            verbatimTextOutput("scatterplot_instr"),
                                                            plotlyOutput("scatterplot", height = 500),
                                                            div(style = "height:25px"))
                                                   )
                                          )
                              )
                            )
                          )
                          
                 ),
                 
                 tabPanel("Subject Dashboard",
                   
                   pageWithSidebar(
                   
                   headerPanel("Subject Level Visuals"),
                   
                   sidebarPanel(width = 2,
                                
                                conditionalPanel(condition = "input.conditionedPanels3 == 'Subject Mean Trajectory Browse'",
                                                 
                                                 h4('Main Variable Options'),
                                                 
                                                 uiOutput("meanbrowsevar"),
                                                 
                                                 radioButtons("meanbrowseraw", 
                                                             label = "Select Data Type:", 
                                                             choices = c("Raw", "Subject Normalized"),
                                                             selected = "Raw", inline = T),
                                                 
                                                 radioButtons("meanbrowseaxis", 
                                                             label = "X-Axis:",
                                                             choices = c("Daily Timepoints"="timepoint", "Weekday"="weekday_n"),
                                                             selected = "timepoint", inline = T),
                                                 
                                                 hr(),
                                                 
                                                 h4('Order Variable Options'), 
                                                 
                                                 uiOutput("meanbrowseorder"),
                                                 
                                                 hr(),
                                                 
                                                 h4('Color Variable Options'), 
                                                 
                                                 uiOutput("meanbrowsecolor"),
                                                 
                                                 radioButtons("meanbrowseradio", "Color Variable Quantiles:",
                                                              c("Auto", "On", "Off"), inline = T),
                                                 
                                                 sliderInput("meanbrowsentile", "Number of Quantiles:",
                                                             min = 2, max = 10,
                                                             value = 4, step = 1)
                                                 
                                ),
                                
                                conditionalPanel(condition = "input.conditionedPanels3 == 'Subject Trajectory Browse'",
                                                 
                                                 h4('Main Variable Options'),
                                                 
                                                 uiOutput("subbrowsevar"),
                                                
                                                 radioButtons("subbrowseraw", 
                                                              label = "Main Variable Type:", 
                                                              choices = c("Raw", "Subject Normalized"),
                                                              selected = "Raw", inline = T),
                                                 
                                                 radioButtons("subbrowselevel", "Main Variable Level:", c("Day", "Assessment"), selected = "Assessment", inline = T),
                              
                                                 hr(),
                                                 
                                                 h4('Order Variable Options'), 
                                                 
                                                 uiOutput("subbrowseorder"),
                                                 
                                                 hr(),
                                                 
                                                 h4('Color Variable Options'), 
                                                 
                                                 uiOutput("subbrowsecolor"),
                                                 
                                                 radioButtons("subbrowsecolortype", 
                                                              label = "Color Variable Type:", 
                                                              choices = c("Raw", "Subject Normalized"),
                                                              selected = "Raw", inline = T),
                                                 
                                                 uiOutput("subbrowsecolorlevel"),
                                                 
                                                 radioButtons("subbrowseradio", "Color Variable Quantiles:",
                                                              c("Auto", "On", "Off"), inline = T),
                                                 
                                                 sliderInput("subbrowsentile", "Number of Quantiles:",
                                                             min = 2, max = 10,
                                                             value = 4, step = 1)
                                                 
                                                 
                                                 
                                ),
                                
                                conditionalPanel(condition = "input.conditionedPanels3 == 'Subject Variable Compare'",
                                                 
                                                 h4('Find Subject'),
                                                 
                                                 uiOutput("subcomparesubject"),
                                                 
                                                 h4('Main Variable Options'),
                                                 
                                                 uiOutput("subcomparevar1"),
                                                 
                                                 uiOutput("subcomparevar2"),
                                                 
                                                 radioButtons("subcompareraw", 
                                                              label = "Main Variable Type:", 
                                                              choices = c("Raw", "Subject Normalized"),
                                                              selected = "Raw", inline = T),
                                                 
                                                 radioButtons("subcomparelevel", "Main Variable Level:", c("Day", "Assessment"), selected = "Assessment", inline = T),
                                                 
                                                 hr(),
                                                
                                                 
                                                 h4('Color Variable Options'), 
                                                 
                                                 uiOutput("subcomparecolor"),
                                                 
                                                 radioButtons("subcomparecolortype", 
                                                              label = "Color Variable Type:", 
                                                              choices = c("Raw", "Subject Normalized"),
                                                              selected = "Raw", inline = T),
                                                 
                                                 uiOutput("subcomparecolorlevel"),
                                                 
                                                 radioButtons("subcompareradio", "Color Variable Quantiles:",
                                                              c("Auto", "On", "Off"), inline = T),
                                                 
                                                 sliderInput("subcomparentile", "Number of Quantiles:",
                                                             min = 2, max = 10,
                                                             value = 4, step = 1)
                                                 
                                            
                                              
                                )
                                
                   ),
                   
                   
                   
                   mainPanel(
                     
                     tabsetPanel(id = "conditionedPanels3",
                                 
                                 tabPanel("Subject Mean Trajectory Browse", 
                                          
                                          fluidRow(
                                            
                                            column(10,
                                                   div(style = "height:15px; width:150px;"),
                                                   div(style="display: inline-block;vertical-align:top; width: 80px;", actionButton("meanbrowseprev", "Prev")),
                                                   div(style="display: inline-block;vertical-align:top; width: 80px;", actionButton("meanbrowsenext", "Next")),
                                                   div(style="display: inline-block;vertical-align:top; width: 100px;", strong("Current Page:"), textOutput("pagenum3.1_display")),
                                                   div(style="display: inline-block;vertical-align:top; width: 100px;", uiOutput("meanbrowsepage")),
                                                   div(style="display: inline-block;vertical-align:top; width: 50px;"),
                                                   div(style="display: inline-block;vertical-align:top; width: 400px;", checkboxGroupInput("meanbrowserand_choice", label = "Choose Random Inputs:", choices = c("Main Var.", "Color Var.", "Order Var."), selected = c("Main Var.", "Color Var.", "Order Var."),
                                                                                                                                           inline = T, width = 300)), 
                                                   div(style="display: inline-block;vertical-align:top; width: 100px;", actionButton("meanbrowserandom", "Random Vars")),
                                                   div(style="display: inline-block;vertical-align:top; width: 20px;"),
                                                   div(style="display: inline-block;vertical-align:top; width: 100px;", actionButton("meanbrowse1", "Create/Update Plot")),
                                                   div(style="display: inline-block;vertical-align:top; width: 100px;"),
                                                   verbatimTextOutput("meanbrowseplot_instr"), 
                                                   plotOutput("meanbrowseplot", height = 800)),
                                            
                                            column(2)
                                            
                                            
                                            
                                          )
                                 ),
                                 
                                 tabPanel("Subject Trajectory Browse",
                                          fluidRow(
                                            column(10,
                                                   div(style = "height:15px; width:150px;"),
                                                   div(style="display: inline-block;vertical-align:top; width: 80px;", actionButton("subbrowseprev", "Prev")),
                                                   div(style="display: inline-block;vertical-align:top; width: 80px;", actionButton("subbrowsenext", "Next")),
                                                   div(style="display: inline-block;vertical-align:top; width: 100px;", strong("Current Page:"), textOutput("pagenum3.2_display")),
                                                   div(style="display: inline-block;vertical-align:top; width: 100px;", uiOutput("subbrowsepage")),
                                                   div(style="display: inline-block;vertical-align:top; width: 50px;"),
                                                   div(style="display: inline-block;vertical-align:top; width: 400px;", checkboxGroupInput("subbrowserand_choice", label = "Choose Random Inputs:", choices = c("Main Var.", "Color Var.", "Order Var."), selected = c("Main Var.", "Color Var.", "Order Var."),
                                                                        inline = T, width = 300)), 
                                                   div(style="display: inline-block;vertical-align:top; width: 100px;", actionButton("subbrowserandom", "Random Vars")),
                                                   div(style="display: inline-block;vertical-align:top; width: 20px;"),
                                                   div(style="display: inline-block;vertical-align:top; width: 100px;", actionButton("subbrowse1", "Create/Update Plot")),
                                                   div(style="display: inline-block;vertical-align:top; width: 100px;"),
                                                   verbatimTextOutput("subbrowseplot_instr"), 
                                                   plotOutput("subbrowseplot", height = 800)
                                                   ),
                                            column(2)
                                          )
                                 ), 
                                 
                                 
                                 tabPanel("Subject Variable Compare",
                                          fluidRow(
                                            
                                            column(12,
                                                   div(style="display: inline-block;vertical-align:top; width: 550px;", checkboxGroupInput("subcomparerand_choice", label = "Choose Random Inputs:", choices = c("Top Var.", "Bottom Var.", "Color Var.", "Subject"), selected = c("Top Var.", "Bottom Var.", "Color Var."),
                                                                                                                                           inline = T, width = 500)), 
                                                   div(style="display: inline-block;vertical-align:top; width: 100px;", actionButton("subcomparerandom", "Random Vars")),
                                                   div(style="display: inline-block;vertical-align:top; width: 20px;"),
                                                   div(style="display: inline-block;vertical-align:top; width: 100px;", actionButton("subcompare1", "Create/Update Plot")),
                                                   verbatimTextOutput("subcompareplot_instr"), 
                                                   plotlyOutput("subcompareplot", height = 600)
                                                   )
                                            
                                          )
                                 )
                     )
                   )
                 )
              )
                 
               
                 
                 
                 
                 
)




#SERVER###################################################################################################

server <- function(input, output){
  
  options(shiny.maxRequestSize=1000*1024^2) 
  
  
  #setwd("C:\\Users\\Mike\\Documents\\Rstuff\\ShinyApps\\EMAApp1.0")
  
  # A) Functions-----------------------------------------------------------------------------------------------------
  
  ##1) "countna": count number of non-misisng reponses for a question (x)
  
  countna <- function(x, na.rm = FALSE, ...){length(which(!is.na(x)))} 
  
  ##2) "newcount": find number of participants who met criteria for compliance for question (x), 
  #     the criteria is (y)*(max possible reponses for (x)), y in [0,1]
  
  newcount <- function(x, y, na.rm = FALSE, ...){length(which(x[1:(length(x) - 1)]>=x[length(x)]*y))}
  
  ##3) "Freqperc": generate frequency table for dataset (x) according to the criteria specified in 
  #    "newcount". (z)=(y) in "newcount".
  
  Freqperc <- function(x,z){                               
    g <- as_data_frame(apply(x, 2, newcount, y=z))
    colnames(g)<-"Frequency"
    g$Percent <- round((g$Frequency/(nrow(x) - 1 )*100), 1)
    g$Questions <- rownames(g)
    row.names(g) <- NULL
    g <- g[c(3,1,2)]
    g$maxind <- t(x[nrow(x),])
    g$criteria <- ifelse(g$maxind==1, "At least 1 response",
                         ifelse(g$maxind==999, "Zero response rate", 
                                paste0("At least ", round(z*g$maxind,0), " out of ", g$maxind)))
    
    g$maxind <- NULL
    
    return(g)
  }
  
  ##4) "Freqdata": Uses the three functions above as well as a auxillary dataset containing max possible responses
  #    for each variable to create a Compliance dataset that varies with threshold.
  
  Freqdata <- function(dataset, maxdata, threshold) {
    
    EMA1 <- dataset %>%               #dataset with number of non missing counts for each question
      group_by_(names(dataset)[1]) %>%
      summarise_all(countna)
    
    EMA1[1] <- max(EMA1[2])
    
    #Create new row with the max possible amount of responses for each question
    #Questions will be one of these types:
    # + Questions offered 4 time a day--56 max responses
    # + Questions offered once a day----14 max responses
    # + Conditional questions-----------1 max responses
    # + broken variables/zero response--max set to 999
    
    #Review codebook and create dataset "x" containing max responses for each question, 
    #where rownames(x)=colnames(EMA1)
    
    #Create dataset with a helper column of max responses for each question found in sample
    #EMA1_max <- EMA1 %>% summarise_all(max)
    #EMA1_max <- as_data_frame(t(EMA1_max))
    #EMA1_max$rownames <- colnames(EMA1)
    #EMA1_max$realmax <- 56
    #fix(EMA1_max)
    #maxpalm <- as_data_frame(t(EMA1_max$realmax))
    #colnames(maxpalm) <- colnames(EMA1)
    #write_csv(maxpalm, "data/maxpalm.csv")
    #read in this dataset in datasets section
    
    #append the extra row containing max possible responses to dataset
    
    EMA1 <- rbind.fill(EMA1, maxdata)
    
    #Create frequency table depending on input threshold
    data <- Freqperc(EMA1, threshold)
    
    return(data)
    
  }
  
  
  ##5) "give.n", used to show group n on boxplot, currently not in use
  #give.n <- function(x){
  #  return(c(y = mean(x, na.rm = T), label = length(na.omit(x))))
  #}
  
  
  ##6) "mymean"
  mymean <- function(x) {
    if(!class(x) %in% c("numeric", "integer")) {NA}
    else {mean(x, na.rm = T)}
  }
  ##7) "mysd"
  mysd <- function(x) sd(x, na.rm = T)
  
  ##8) "normalize"
  normalize <- function(x) {
    if (is.numeric(x)) {
      if (sd(x, na.rm = T) %in% 0) {ifelse(!is.na(x), 0, NA)}
      else {(x - mean(x, na.rm = T))/sd(x, na.rm = T)}
    }
    
    else {x}
    
  }
  
  ##9) "getmode"
  getmode <- function(v) {
    v <- as.character(v)
    uniqv <- unique(na.omit(v))
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  ##10) "my_ntiles" modified version of mosaic::ntiles, handles repeated ntile limits and missings
  my_ntiles <- function (x, n = 3, digits = 3) 
    
  {
    x <- as.numeric(x)
    xrank <- rank(x, na.last = TRUE, ties.method = "first")
    xrank[is.na(x)] <- NA
    size <- max(xrank, na.rm = TRUE)
    
    if(size[[1]] < n + 1) {res <- rep(NA, length(x))}
    
    else{
      
      cts <- round(seq(1, size, length.out = (n + 1)))
      bin <- as.numeric(cut(xrank, breaks = cts, include.lowest = TRUE))
      left <- min(x ~ bin, na.rm = TRUE)
      right <- max(x ~ bin, na.rm = TRUE)
      res <- factor(bin, labels = paste0("[", signif(left, digits = digits), " to ", signif(right, digits = digits), "]", " Q", max(bin ~ bin, na.rm = T)), ordered = TRUE)
    }
    
    return(res)
  }
  
  
  ##11) "Ordershow" function for ordering variable display, rounds numeric to 2 digits
  ordershow <- function (x) {if (is.numeric(x)) {round(x, 2)} else  {x}}
  
  

  # B) Datasets-------------------------------------------------------------------------------------------------
  
  ##NIMH Merged
  NIMHMerged <- read.csv("data/EMA_Merge_2_20_18.csv", na.strings = c("NA", "NaN", ""), stringsAsFactors = F) 
  NIMHMerged_max <- read.csv("data/maxmerged.csv",  stringsAsFactors = F) 
  NIMHvars <- read.csv("data/NIMHvars.csv", stringsAsFactors = F)
  
  ##NIMH Palm
  NIMHPalm <- read.csv("data/EMAPalm_1_25_18.csv", na.strings = c("NA", "NaN", ""), stringsAsFactors = F) 
  NIMHPalm_max <- read.csv("data/maxpalm.csv", stringsAsFactors = F) 
  
  ##NIMH Droid
  NIMHDroid <- read.csv("data/EMADroid_1_25_18.csv", na.strings = c("NA", "NaN", ""), stringsAsFactors = F) 
  NIMHDroid_max <- read.csv("data/maxdroid.csv", stringsAsFactors = F) 
  
  ##NCCR
  NCCR <- read.csv("data/EMANCCR.csv", na.strings = c("NA", "NaN", ""), stringsAsFactors = F) 
  NCCR_max <- read.csv("data/maxnccr.csv", stringsAsFactors = F) 
  NCCRvars <- read.csv("data/NCCRvars.csv", stringsAsFactors = F) 
  
  ##Colaus
  Colaus <- read.csv("data/EMAColaus.csv", na.strings = c("NA", "NaN", ""), stringsAsFactors = F) 
  Colaus_max <- read.csv("data/maxcolaus.csv", stringsAsFactors = F) 
  Colausvars <- read.csv("data/Colausvars.csv", stringsAsFactors = F)
  
  ##Upload
  
  Upload <- reactiveValues(df = NULL)
  
  observeEvent(input$file1, {
    Upload$df <- read.csv(input$file1$datapath, na.strings = c("NA", "NaN", ""), stringsAsFactors = F)
  })
  
  Upload_max <- reactive({ read.csv(input$file2$datapath, stringsAsFactors = F) })
  
  Upload_vars <- reactiveValues(df = NULL)
  
  observeEvent(input$file3, {
    Upload_vars$df <-  read.csv(input$file3$datapath, stringsAsFactors = F) 
  })
  
  
  
  
  
  ##Add extra variables to each dataset: timeofday, weekday | diagnosis to vars datasets-----------------------------------------------------------------------
  
  ###NIMHPalm-------------------------------------------------------------------------------------------------------------------------------------------
  NIMHPalm$timeofday  <- ifelse(NIMHPalm$Morning==1, "Morning",
                                ifelse(NIMHPalm$Noon==1, "Noon",
                                       ifelse(NIMHPalm$Afternoon==1, "Afternoon",
                                              ifelse(NIMHPalm$Evening==1, "Evening", NA)))) 
  
  
  NIMHPalm$timeofday <- factor(NIMHPalm$timeofday, levels = c("Morning", "Noon", "Afternoon", "Evening"))
  
  NIMHPalm$timepoint <- ifelse(NIMHPalm$Morning==1, 1,
                              ifelse(NIMHPalm$Noon==1, 2,
                                     ifelse(NIMHPalm$Afternoon==1, 3,
                                            ifelse(NIMHPalm$Evening==1, 4, NA))))
  
  NIMHPalm$weekday <- weekdays(as.Date(NIMHPalm$time))
  NIMHPalm$weekday <- factor(NIMHPalm$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  NIMHPalm$weekday_n <- ifelse(NIMHPalm$weekday %in% "Monday", 1, 
                            ifelse(NIMHPalm$weekday %in% "Tuesday", 2,
                                   ifelse(NIMHPalm$weekday %in% "Wednesday", 3, 
                                          ifelse(NIMHPalm$weekday %in% "Thursday", 4,
                                                 ifelse(NIMHPalm$weekday %in% "Friday", 5,
                                                        ifelse(NIMHPalm$weekday %in% "Saturday", 6,
                                                               ifelse(NIMHPalm$weekday %in% "Sunday", 7, NA)))))))

  
  NIMHPalm$weektime_n <- (NIMHPalm$weekday_n - 1) * 4 + NIMHPalm$timepoint
  
  NIMHPalm$timeindex <- NIMHPalm$day * 4 - (4 - NIMHPalm$timepoint)
  
  NIMHPalm <- dplyr::rename(NIMHPalm, Control = control)  
  
  NIMHPalm$ID <- as.character(NIMHPalm$ID)
  
  ###NIMHDroid------------------------------------------------------------------------------------------------------------------------------------------
  NIMHDroid$timeofday  <- ifelse(NIMHDroid$daySignal==0, "Morning",
                                 ifelse(NIMHDroid$daySignal==1, "Noon",
                                        ifelse(NIMHDroid$daySignal==2, "Afternoon",
                                               ifelse(NIMHDroid$daySignal==3, "Evening", NA)))) 
  
  NIMHDroid$timeofday <- factor(NIMHDroid$timeofday, levels = c("Morning", "Noon", "Afternoon", "Evening"))
  
  NIMHDroid$timepoint <- NIMHDroid$daySignal + 1
  
  NIMHDroid$weekday <- weekdays(as.Date(NIMHDroid$time))
  NIMHDroid$weekday <- factor(NIMHDroid$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  NIMHDroid$weekday_n <- ifelse(NIMHDroid$weekday %in% "Monday", 1, 
                               ifelse(NIMHDroid$weekday %in% "Tuesday", 2,
                                      ifelse(NIMHDroid$weekday %in% "Wednesday", 3, 
                                             ifelse(NIMHDroid$weekday %in% "Thursday", 4,
                                                    ifelse(NIMHDroid$weekday %in% "Friday", 5,
                                                           ifelse(NIMHDroid$weekday %in% "Saturday", 6,
                                                                  ifelse(NIMHDroid$weekday %in% "Sunday", 7, NA)))))))
  
  NIMHDroid$weektime_n <- (NIMHDroid$weekday_n - 1) * 4 + NIMHDroid$timepoint
  
  names(NIMHDroid)[1] <- "ID"
  
  NIMHDroid$ID <- as.character(NIMHDroid$ID)
  
  NIMHDroid$day <- NIMHDroid$studyDay + 1
  
  NIMHDroid$timeindex <- NIMHDroid$day * 4 - (4 - NIMHDroid$timepoint)
  
  ###NIMHMerged---------------------------------------------------------------------------------------------------------------------------------------
  NIMHMerged$timeofday  <- ifelse(NIMHMerged$PD_signal==1, "Morning",
                                  ifelse(NIMHMerged$PD_signal==2, "Noon",
                                         ifelse(NIMHMerged$PD_signal==3, "Afternoon",
                                                ifelse(NIMHMerged$PD_signal==4, "Evening", NA)))) 
  
  NIMHMerged$timeofday <- factor(NIMHMerged$timeofday, levels = c("Morning", "Noon", "Afternoon", "Evening"))
  
  NIMHMerged$timepoint <- ifelse(NIMHMerged$PD_signal==1, 1,
                                ifelse(NIMHMerged$PD_signal==2, 2,
                                       ifelse(NIMHMerged$PD_signal==3, 3,
                                              ifelse(NIMHMerged$PD_signal==4, 4, NA))))
  
  NIMHMerged$weekday <- weekdays(as.Date(NIMHMerged$PD_time))
  NIMHMerged$weekday <- factor(NIMHMerged$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  NIMHMerged$weekday_n <- ifelse(NIMHMerged$weekday %in% "Monday", 1, 
                                ifelse(NIMHMerged$weekday %in% "Tuesday", 2,
                                       ifelse(NIMHMerged$weekday %in% "Wednesday", 3, 
                                              ifelse(NIMHMerged$weekday %in% "Thursday", 4,
                                                     ifelse(NIMHMerged$weekday %in% "Friday", 5,
                                                            ifelse(NIMHMerged$weekday %in% "Saturday", 6,
                                                                   ifelse(NIMHMerged$weekday %in% "Sunday", 7, NA)))))))
  
  NIMHMerged$weektime_n <- (NIMHMerged$weekday_n - 1) * 4 + NIMHMerged$timepoint
  
  names(NIMHMerged)[1] <- "ID"
  
  NIMHMerged$ID <- as.character(NIMHMerged$ID)
  
  NIMHMerged <- NIMHMerged %>% dplyr::rename(day = PD_day, time = PD_time)
  
  NIMHMerged$timeindex <- NIMHMerged$day * 4 - (4 - NIMHMerged$timepoint)
  
  ###NIMHvars------------------------------------------------------------------------------------------------------------------------------------------
  NIMHvars$diagnosis <-  ifelse(NIMHvars$BIP1 %in% 1, "bipolar I", 
                                ifelse(NIMHvars$BIP2 %in% 1, "bipolar II",
                                       ifelse(NIMHvars$MDD_Dx %in% 1, "MDD",
                                              ifelse(NIMHvars$ANX %in% 1, "Anxiety",
                                                     ifelse(NIMHvars$control %in% 1, "control", "other")))))
  
  NIMHvars$diagnosis <- factor(NIMHvars$diagnosis, levels = c("bipolar I", "bipolar II", "MDD", "Anxiety", "control", "other"))
  
  names(NIMHvars)[1] <- "ID"
  #NIMHvars <- as_data_frame(apply(NIMHvars, 2, factor))
  
  NIMHvars <- NIMHvars %>% mutate(ID = as.character(ID)) %>% 
                           mutate_if(is.integer, function (x) if (length(unique(x)) <= 20)  {as.character(x)} 
                                                              else {x})
  

  
  #NIMHvars <- NIMHvars %>% mutate_all(as.factor)
  
  ###NCCR----------------------------------------------------------------------------------------------------------------------------------------------
  NCCR$timeofday  <- ifelse(NCCR$daySignal==0, "Morning",
                            ifelse(NCCR$daySignal==1, "Noon",
                                   ifelse(NCCR$daySignal==2, "Afternoon",
                                          ifelse(NCCR$daySignal==3, "Evening", NA)))) 
  
  NCCR$timeofday <- factor(NCCR$timeofday, levels = c("Morning", "Noon", "Afternoon", "Evening"))
  
  NCCR$timepoint <- NCCR$daySignal + 1
  
  NCCR$weekday <- weekdays(as.Date(NCCR$time))
  NCCR$weekday <- factor(NCCR$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  NCCR$weekday_n <- ifelse(NCCR$weekday %in% "Monday", 1, 
                                 ifelse(NCCR$weekday %in% "Tuesday", 2,
                                        ifelse(NCCR$weekday %in% "Wednesday", 3, 
                                               ifelse(NCCR$weekday %in% "Thursday", 4,
                                                      ifelse(NCCR$weekday %in% "Friday", 5,
                                                             ifelse(NCCR$weekday %in% "Saturday", 6,
                                                                    ifelse(NCCR$weekday %in% "Sunday", 7, NA)))))))
  
  NCCR$weektime_n <- (NCCR$weekday_n - 1) * 4 + NCCR$timepoint
  
  NCCRvars$subjectID <- as.character(NCCRvars$subjectID)
  
  names(NCCRvars)[1] <- "ID"
  
  names(NCCR)[1] <- "ID"
  
  NCCR$day <- NCCR$studyDay + 1

  NCCR$timeindex <- NCCR$day * 4 - (4 - NCCR$timepoint)
  
  ###Colaus------------------------------------------------------------------------------------------------------------------------------------------
  Colaus$timeofday  <- ifelse(Colaus$daySignal==0, "Morning",
                              ifelse(Colaus$daySignal==1, "Noon",
                                     ifelse(Colaus$daySignal==2, "Afternoon",
                                            ifelse(Colaus$daySignal==3, "Evening", NA)))) 
  
  Colaus$timeofday <- factor(Colaus$timeofday, levels = c("Morning", "Noon", "Afternoon", "Evening"))
  
  Colaus$timepoint <- Colaus$daySignal + 1
  
  Colaus$weekday <- weekdays(as.Date(Colaus$time))
  Colaus$weekday <- factor(Colaus$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  Colaus$weekday_n <- ifelse(Colaus$weekday %in% "Monday", 1, 
                           ifelse(Colaus$weekday %in% "Tuesday", 2,
                                  ifelse(Colaus$weekday %in% "Wednesday", 3, 
                                         ifelse(Colaus$weekday %in% "Thursday", 4,
                                                ifelse(Colaus$weekday %in% "Friday", 5,
                                                       ifelse(Colaus$weekday %in% "Saturday", 6,
                                                              ifelse(Colaus$weekday %in% "Sunday", 7, NA)))))))
  
  Colaus$weektime_n <- (Colaus$weekday_n - 1) * 4 + Colaus$timepoint
  
  Colausvars <- Colausvars %>% mutate_if(is.integer, as.character)
  
  Colausvars$subjectID <- as.character(Colausvars$subjectID)
  
  Colaus$subjectID <- as.character(Colaus$subjectID)
  
  names(Colausvars)[1] <- "ID"
  
  names(Colaus)[1] <- "ID"
  
  Colaus$day <- Colaus$studyDay + 1
  
  Colaus$timeindex <- Colaus$day * 4 - (4 - Colaus$timepoint)
  
  ###Upload dataset variable creation-----------------------------------------------------------------------------------------------------------------
  
  #Rules: ID variable is called "ID"
  #1)time variable called "time", in any MM/DD/YY HH:MM:SS format (pick a standard format for time plots)
  #2)daily index variable called "timepoint", 1:D, where D is the max number of assessments per day
  #3) timeofday is a factor version of "timepoint"
  #4)assessment day variable called "day", from 1:T, where T is the max number of assessment per subject
  
  
  ####Main dataset
  
  observeEvent(input$file1, {
  
  Upload$df <- Upload$df %>% mutate(timepoint = ifelse(Morning==1, 1,
                                         ifelse(Noon==1, 2,
                                                ifelse(Afternoon==1, 3,
                                                       ifelse(Evening==1, 4, NA)))),
                        
                        #factor version of timepoint variable
                        timeofday = as.factor(timepoint),
                        
                        #weekday variable
                        weekday = factor(weekdays(as.Date(time)), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
                        
                        #numeric weekday variable
                        weekday_n = ifelse(weekday %in% "Monday", 1, 
                                           ifelse(weekday %in% "Tuesday", 2,
                                                  ifelse(weekday %in% "Wednesday", 3, 
                                                         ifelse(weekday %in% "Thursday", 4,
                                                                ifelse(weekday %in% "Friday", 5,
                                                                       ifelse(weekday %in% "Saturday", 6,
                                                                              ifelse(weekday %in% "Sunday", 7, NA))))))),
                        #numeric variable for day of week/time of day
                        weektime_n = (weekday_n - 1) * max(timepoint) + timepoint,
                        
                        timeindex = day * 4 - (4 - timepoint),
                        
                        #coerce ID variable to be character
                        ID = as.character(ID)
                      )
  })
  
  ####Vars dataset
  
  observeEvent(input$file3, {
    
  Upload_vars$df <- Upload_vars$df %>% mutate(diagnosis = ifelse(BIP1 %in% 1, "bipolar I", 
                                                               ifelse(BIP2 %in% 1, "bipolar II",
                                                                      ifelse(MDD_Dx %in% 1, "MDD",
                                                                             ifelse(ANX %in% 1, "Anxiety",
                                                                                    ifelse(control %in% 1, "control", "other"))))),
                                            
                                            diagnosis = factor(diagnosis, levels = c("bipolar I", "bipolar II", "MDD", "Anxiety", "control", "other"))) %>%
                                     dplyr::rename(ID = studyid) %>%
                                     mutate(ID = as.character(ID)) %>%
                                     mutate_if(is.integer, function (x) if (length(unique(x)) <= 20)  {as.character(x)} 
                                                                        else {x})
  
  })
    
 
  ##Get dataset and dataset_max based on input$dataset--------------------------------------------------------------------------------------------------
  dataset <- eventReactive(input$go, {switch(input$dataset,
                              "NIMH Merged" = NIMHMerged,
                              "NIMH Palm" = NIMHPalm,
                              "NIMH Droid" = NIMHDroid,
                              "NCCR" = NCCR,
                              "CoLaus" = Colaus,
                              "Upload" = Upload$df)})
  
  dataset_max <- eventReactive(input$go, {switch(input$dataset,
                                  "NIMH Merged" = NIMHMerged_max,
                                  "NIMH Palm" = NIMHPalm_max,
                                  "NIMH Droid" = NIMHDroid_max,
                                  "NCCR" = NCCR_max,
                                  "CoLaus" = Colaus_max,
                                  "Upload" = Upload_max())})
  
  dataset_vars <- eventReactive(input$go, {switch(input$dataset,
                                   "NIMH Merged" = NIMHvars,
                                   "NIMH Palm" = NIMHvars,
                                   "NIMH Droid" = NIMHvars,
                                   "NCCR" = NCCRvars,
                                   "CoLaus" = Colausvars,
                                   "Upload" = Upload_vars$df)})
  
  
  observeEvent(input$go, {showNotification(
                           paste0(input$dataset, " Selected"),
                           duration = 2, 
                           type = "message")})
  
  
  
  
  #Merge main dataset and vars dataset to make final vars datasets for data subset, create dataset at full, day, and subject levels
  
  stratify_vars <- reactiveValues(df_sub = NULL, df_day = NULL, df_full = NULL)
  
  observeEvent(input$go, { withProgress(message = 'Creating Datasets', {
    
    stratify_vars$df_full <- dataset() %>%
      mutate(month = substr(time, 6, 7),
             year = substr(time, 1, 4),
             season = ifelse(month %in% c("03", "04", "05"), "spring",
                             ifelse(month %in% c("06", "07", "08"), "summer",
                                    ifelse(month %in% c("09", "10", "11"), "fall",
                                           ifelse(month %in% c("12", "01", "02"), "winter", NA))))
      ) %>%
      left_join(dataset_vars(), by="ID")
    
    
    stratify_vars$df_sub <- stratify_vars$df_full %>%
                            group_by(ID) %>%
                            summarise_at(2, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                   ifelse(is.numeric(x), mean(x, na.rm = T), NA))
                            return(y)}) %>% 
                            ungroup()
                           
                           
    
    
    stratify_vars$df_day <- stratify_vars$df_full %>%
                            group_by(ID, day) %>%
                            summarise_at(2, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                ifelse(is.numeric(x), mean(x, na.rm = T), NA))
                            return(y)}) %>% 
                            ungroup()
    
    
  })
  })
  

  #C) Compliance Page 1: frequency histogram, summary box, and getnames---------------------------------------------------------------------------
  
  ##data for frequency histogram
  data <- reactive({withProgress(message = 'Loading Data', {
                                 Freqdata(dataset(), dataset_max(), input$num)
                                 })
                                 })
  
  ##parameters for frequency historgram
  data2 <- reactive({data()[order(-data()$Percent),]})
  data3 <- reactive({data()[order(data()$Questions),]})
  limits <- reactive({data()$Questions[data()$Percent>=input$slider[1] & data()$Percent<=input$slider[2]]})
  limits2 <- reactive({data2()$Questions[data2()$Percent>=input$slider[1] & data2()$Percent<=input$slider[2]]})
  limits3 <- reactive({data3()$Questions[data3()$Percent>=input$slider[1] & data3()$Percent<=input$slider[2]]})
  height <- reactive({length(limits())*14+500})
  
  ##frequency histogram output
  output$Bar <- renderPlot({
    if(input$sort==1){ggplot(data=data()[data()$Questions %in% limits(),], aes(x=Questions, y=Percent, fill=Percent))+
        ggtitle(paste0("Frequency percents (n=",length(limits())," out of ", nrow(data()), ")"))+
        guides(fill=FALSE)+
        geom_bar(stat="identity")+
        scale_x_discrete(limits=rev(limits()))+
        scale_y_continuous(expand=c(0,0), limits=(c(0,110)))+
        scale_fill_distiller(palette = "RdYlGn", trans = "reverse", limits = c(100,0))+
        geom_text(aes(label=Percent), hjust = 0, nudge_x = 0.05)+
        theme_bw()+
        theme(axis.title.y=element_blank(),
              panel.border = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank())+
        coord_flip()
    }
    else if(input$sort==2){ggplot(data=data2()[data2()$Questions %in% limits(),], aes(x=Questions, y=Percent, fill=Percent))+
        ggtitle(paste0("Frequency percents (n=",length(limits2())," out of ", nrow(data()), ")"))+
        guides(fill=FALSE)+
        geom_bar(stat="identity")+
        scale_x_discrete(limits=rev(limits2()))+
        scale_y_continuous(expand=c(0,0), limits=(c(0,110)))+
        scale_fill_distiller(palette = "RdYlGn", trans = "reverse", limits = c(100,0))+
        geom_text(aes(label=Percent), hjust = 0, nudge_x = 0.05)+
        theme_bw()+
        theme(axis.title.y=element_blank(),
              panel.border = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank())+
        coord_flip()
    }
    else if(input$sort==3){ggplot(data=data3()[data3()$Questions %in% limits(),], aes(x=Questions, y=Percent, fill=Percent))+
        ggtitle(paste0("Frequency percents (n=",length(limits3())," out of ", nrow(data()), ")"))+
        guides(fill=FALSE)+
        geom_bar(stat="identity")+
        scale_x_discrete(limits=rev(limits3()))+
        scale_y_continuous(expand=c(0,0), limits=(c(0,110)))+
        scale_fill_distiller(palette = "RdYlGn", trans = "reverse", limits = c(100,0))+
        geom_text(aes(label=Percent), hjust = 0, nudge_x = 0.05)+
        theme_bw()+
        theme(axis.title.y=element_blank(),
              panel.border = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank())+
        coord_flip()
    }
  },
  height = height , width = 800)
  
  ##Median box
  output$median <- renderPrint({cat('Median Frequency:\n')
    print(median(data()[data()$Questions %in% limits(),]$Percent))
    cat('Mean Frequency:\n')
    print(round(mean(data()[data()$Questions %in% limits(),]$Percent), 2))
  })
  
  ##Get names box
  output$namespalm <- renderPrint(if (input$sort==1) {cat(paste(shQuote(limits(), type="cmd"), collapse=", "))}
                                  else if (input$sort==2) {cat(paste(shQuote(limits2(), type="cmd"), collapse=", "))}
                                  else if (input$sort==3) {cat(paste(shQuote(limits3(), type="cmd"), collapse=", "))})
  
  
  #Compliance Page 2, boxplot of compliance-----------------------------------------------------------------------------------------------------
  
  ##data table for boxplot
  output$table1 <- DT::renderDataTable(data(), selection = list(selected = 8, mode = 'single'),
                                       options = list(columnDefs = list(list(
                                         targets = 1,
                                         render = JS(
                                           "function(data, type, row, meta) {",
                                           "return type === 'display' && data.length > 12 ?",
                                           "'<span title=\"' + data + '\">' + data.substr(0, 12) + '...</span>' : data;",
                                           "}")
                                       ))), callback = JS('table.page(3).draw(false);'))
  
  
  #Select main variable
  output$boxplotvar <- renderUI({selectInput('boxplotvar', 'Main Variable:', c(names(dataset())), selected = boxplotvariables$var, selectize=TRUE)})
  
  
  
  #Select coloring variable
  output$boxplotcolor <- renderUI({selectInput('boxplotcolor', 'Color By:', c("None", varnames1.2$df), selected = boxplotvariables$color, selectize=TRUE)})
  
  
  #boxplot color variable
  varcolor1.2 <- reactiveValues(l="ID")
  
  observeEvent(input$boxplot1, {if (input$boxplotcolor %in% c("ID", "None")) {varcolor1.2$l <- input$boxplotcolor}
    else {varcolor1.2$l <- paste0(input$boxplotcolor, "_s")} 
  })
  
  # output$boxselectR <- renderUI({selectInput('boxselectR', 'Stratify by:', c("None", varnames$l), selectize=TRUE)})
  
  
  ##Use varnames so updating stratify_vars3.1$df2 doesn't refresh subbrowsecolor
  varnames1.2 <- reactiveValues(df=NULL)
  
  
  ##Color datasets
  stratify_vars1.2 <- reactiveValues(df = NULL, df2 = NULL)
  
  ##Default datasets
  observeEvent(input$go, priority = -1, {
    
    stratify_vars1.2$df <- stratify_vars$df_sub %>% select("ID") %>% mutate(dummy_s=1)
    stratify_vars1.2$df2 <- stratify_vars1.2$df
    
    varnames1.2$df <- names(stratify_vars$df_full)
    
    
  })
  
  
  ##create the appropriate stratify_vars1.2$df 
  observeEvent(input$boxplot1, {
    
    if(!input$boxplotcolor %in% c("ID", "None")) {
      stratify_vars1.2$df <- stratify_vars$df_full %>% select_("ID", input$boxplotcolor) %>%
        group_by(ID) %>%
        summarise_at(input$boxplotcolor, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                   ifelse(is.numeric(x), mean(x, na.rm = T), NA))
        return(y)}) %>% 
        ungroup() 
    }
    
    else {stratify_vars1.2$df <- stratify_vars$df_sub %>% select_("ID") %>% mutate(dummy_s = 1)}
    
  })
  
  ##Color vars dataset, upon action button, stratify_vars1.2$df2 will update based on variable and quantile selection
  
  observeEvent(input$boxplot1, ignoreInit = T, {
    
    stratify_vars1.2$df2 <-  
      
      
      if(input$boxplotcolor %in% c("ID", "None")) {stratify_vars1.2$df}
    
    else {
      if(input$boxplotradio %in% "Auto"){
        stratify_vars1.2$df %>% 
          mutate_at(input$boxplotcolor, function (x) { if (is.character(x) | is.factor(x)) {x}
            else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) > 20) {my_ntiles(x, input$boxplotntile)}
            else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) <= 20) {factor(x, ordered = T, exclude = c(NA, "NaN"))}
            else {x=NA}
          }) %>% 
          rename_at(vars(input$boxplotcolor), ~ paste0(input$boxplotcolor, "_s"))
      }
      
      else if (input$boxplotradio %in% "On"){
        stratify_vars1.2$df %>% 
          mutate_at(input$boxplotcolor, ~my_ntiles(.x, input$boxplotntile)) %>%
          rename_at(vars(input$boxplotcolor), ~ paste0(input$boxplotcolor, "_s"))
        
      }
      
      else if (input$boxplotradio %in% "Off"){
        stratify_vars1.2$df %>% 
          rename_at(vars(input$boxplotcolor), ~ paste0(input$boxplotcolor, "_s"))
        
      }
    }
    
  })
  
  
  #Random plot
  
  #make plot update after new variables are selected
  
  boxplotvariables <- reactiveValues(var = "ID", color = "None")
  
  #Default boxplot variable
  observeEvent(input$go, priority = -1, {
    
    boxplotvariables$var <- names(dataset())[[sample(1:length(names(dataset())), 1)]]
    
  })
  
  boxplotrandbutton <- reactiveValues(r1=NULL, r2=NULL)
  
  observeEvent(input$boxplotrandom, priority = 2, ignoreInit = T, {
    
    boxplotrandbutton$r1 <- sample(1:length(names(dataset())), 1)
    
    boxplotrandbutton$r2 <- sample(1:length(varnames1.2$df), 1)
    
    
    if("Main Var." %in% input$boxplotrand_choice) {boxplotvariables$var <- names(dataset())[[boxplotrandbutton$r1]]}
    
    if("Color Var." %in% input$boxplotrand_choice) {boxplotvariables$color <- varnames1.2$df[[boxplotrandbutton$r2]]}
    
  })
  
  
  ##Data for response boxplots
  
  boxplotdata <- reactiveValues(l=NULL, m=NULL)
  
  observeEvent(input$boxplot1, ignoreInit = T, {
    
    
    boxplotdata$l <- 
      dataset() %>% 
      select_("ID", "timepoint", input$boxplotvar) %>%
      group_by_("ID") %>%
      summarise_all(countna) %>%
      left_join(stratify_vars1.2$df2, by="ID")
    
    #time of day boxplot
    boxplotdata$m <- 
      dataset() %>% 
      select_("ID", "timepoint", "timeofday", input$boxplotvar) %>%
      group_by_("ID", "timeofday") %>%
      summarise_all(countna)
    
  })
  
  
  
  #dummy variable to control plot, since plot runs automatically when page is selected, will make it dependent on a reactive value that updates 
  #once the "create plot" button is clicked.
  
  boxplot_dummy <- reactiveValues(l=0)
  
  observeEvent(input$boxplot1, ignoreInit = T, {
    if (boxplot_dummy$l==0) {boxplot_dummy$l <- 1}
    else NULL
  })
  
  ##Reset plot when dataset changes
  
  observeEvent(input$go, {
    boxplot_dummy$l <- 0
    
  })
  
  ##Instructions that appear before create plot button is clicked
  
  output$boxplot_instr <-  renderText(
    if(boxplot_dummy$l==0) {"Click the [Create/Update Plot] button to generate plot!"}
    else NULL 
  )
  
  
  ##Main boxplot code
  output$boxplot <- renderPlotly({
    
    input$boxplot1
    
    isolate(
      
      if(boxplot_dummy$l==0) NULL
      
      else {
        
        if (varcolor1.2$l  %in% "None") {
          
          ggplotly(ggplot(data=boxplotdata$l, aes_string(x=factor(0),
                                                         y=input$boxplotvar,
                                                         label="ID"))+ 
                     geom_boxplot(size=.5, fatten=1)+
                     geom_jitter(alpha=0.2, size=1.2, shape=19, position = position_jitter(w = 0.5, h = 0.1))+
                     stat_summary(fun.data = mean_cl_normal, geom="pointrange", size=2, alpha=.8, 
                                  position=position_dodge(width=0.75), shape=4, color="firebrick")+
                     labs(y=NULL, x=NULL, title=input$boxplotvar)+
                     theme_bw()+
                     theme(axis.title.x=element_blank(), axis.title.y=element_text(size=8), 
                           axis.text.x = element_text(size=8, vjust = 1, color="gray65"), 
                           axis.text.y = element_blank(),
                           axis.line = element_line(color="gray65", size=0.5),
                           axis.ticks.y = element_blank(),
                           axis.ticks.x = element_line(colour = "gray"),
                           panel.grid.major = element_blank(), 
                           panel.grid.minor = element_blank(), 
                           panel.background = element_blank(),
                           panel.border = element_blank(),
                           #plot.margin = unit( c(0,3,3,0) , "in"),
                           aspect.ratio = 0.3) +
                     #legend.position="none")+
                     coord_flip()
                   #coord_flip(ylim=c(0, max(boxdata_dR()[4])), expand = c(0.1,0))+
                   #scale_y_continuous(breaks=seq(0, max(boxdata_dR()[2]), by = 4))
          )
        }
        
        else {  
          
          ggplotly(ggplot(data=boxplotdata$l, aes_string(x=varcolor1.2$l ,
                                                         y=input$boxplotvar,
                                                         label="ID", color=varcolor1.2$l))+ 
                     geom_boxplot(size=.5, fatten=1)+
                     geom_jitter(alpha=0.2, size=1.2, shape=19, position = position_jitter(w = 0.5, h = 0.1))+
                     stat_summary(fun.data = mean_cl_normal, geom="pointrange", size=2, alpha=.8, 
                                  position=position_dodge(width=0.75), shape=4, color="firebrick")+
                     #stat_summary(fun.data = give.n, geom = "text", color="firebrick", size=4) +
                     labs(y=NULL, x=NULL, title=input$boxplotvar)+
                     theme_bw()+
                     theme(axis.title.x=element_blank(), axis.title.y=element_text(size=8), 
                           axis.text.x = element_text(size=8, vjust = 1, color="gray65"), 
                           axis.text.y = element_text(size=10),
                           axis.line = element_line(color="gray65", size=0.5),
                           axis.ticks.y = element_blank(),
                           axis.ticks.x = element_line(colour = "gray"),
                           panel.grid.major = element_blank(), 
                           panel.grid.minor = element_blank(), 
                           panel.background = element_blank(),
                           panel.border = element_blank(),
                           plot.margin = unit( c(0.5,0.5,0.5,1) , "cm"),
                           aspect.ratio = 0.3)+
                     #legend.position="none")+
                     coord_flip()
                   #coord_flip(ylim=c(0, max(boxdata_dR()[4])), expand = c(0.1,0))+
                   #scale_y_continuous(breaks=seq(0, max(boxdata_dR()[2]), by = 4))
          )
          
          
        }
      }
      
    )
    
  })
  
  
  ##Timeofday boxplot code
  
  output$boxplotTOD <- renderPlotly({
    
    input$boxplot1
    
    isolate(
      
      if(boxplot_dummy$l==0) NULL
      
      else {
        
        ggplotly(ggplot(data=boxplotdata$m, aes_string(x="timeofday", 
                                                       y=input$boxplotvar, 
                                                       color="timeofday", label="ID"))+
                   geom_boxplot(size=.5, fatten=1)+
                   geom_jitter(alpha=0.2, size=1.2, shape=19, position = position_jitter(w = 0.5, h = 0.1))+
                   stat_summary(fun.data = mean_cl_normal, geom="pointrange", size=2, alpha=.8, 
                                position=position_dodge(width=0.75), shape=4, color="firebrick")+
                   labs(y=NULL, x=NULL, title="")+
                   scale_x_discrete(limits = rev(levels(boxplotdata$m[["timeofday"]])))+
                   theme_bw()+
                   theme(axis.title.x=element_blank(), axis.title.y=element_text(size=8), 
                         axis.text.x = element_text(size=8, vjust = 1, color="gray65"), 
                         axis.text.y = element_text(size=10),
                         axis.line = element_line(color="gray65", size=0.5),
                         axis.ticks.y = element_blank(),
                         axis.ticks.x = element_line(colour = "gray"),
                         panel.grid.major = element_blank(), 
                         panel.grid.minor = element_blank(), 
                         panel.background = element_blank(),
                         panel.border = element_blank(),
                         #plot.margin = unit( c(0,3,3,0) , "in"),
                         aspect.ratio = 0.2,
                         legend.position="none") +
                   coord_flip()
                 # scale_y_continuous(breaks=c(0:max(boxdataR()[3])))
        )
        
      }
      
    )
    
  })
  
  
  
  #Compliances Page 3, heatmap of Compliances-----------------------------------------------------------------------------------------------------
  
  ##datatable for heatmaps
  output$table2 <- DT::renderDataTable(data(), selection = list(selected = 8, mode = 'single'),
                                       options = list(columnDefs = list(list(
                                         targets = 1,
                                         render = JS(
                                           "function(data, type, row, meta) {",
                                           "return type === 'display' && data.length > 12 ?",
                                           "'<span title=\"' + data + '\">' + data.substr(0, 12) + '...</span>' : data;",
                                           "}")
                                       ))), callback = JS('table.page(3).draw(false);'))
  
  
  #Select main variable
  output$heatmapvar <- renderUI({selectInput('heatmapvar', 'Main Variable:', c(names(dataset())), selected = heatmapvariables$var, selectize=TRUE)})
  
  
  
  #Select ordering variable
  output$heatmaporder <- renderUI({selectInput('heatmaporder', 'Order By:', c("None", "Compliance", varnames1.4$df), selected = heatmapvariables$order, selectize=TRUE)})
  
  
  #heatmap order variable
  varorder1.4 <- reactiveValues(l="ID")
  
  observeEvent(input$heatmap1, {if (input$heatmaporder %in% c("ID", "None", "Compliance")) {varorder1.4$l <- input$heatmaporder}
    else {varorder1.4$l <- paste0(input$heatmaporder, "_s")} 
  })
  
  
  ##Use varnames so updating stratify_vars3.1$df2 doesn't refresh subbrowseorder
  varnames1.4 <- reactiveValues(df=NULL)
  
  
  ##order datasets
  stratify_vars1.4 <- reactiveValues(df = NULL, df2 = NULL)
  
  ##Default datasets
  observeEvent(input$go, priority = -1, {
    
    stratify_vars1.4$df <- stratify_vars$df_sub %>% select("ID") %>% mutate(dummy_s=1)
    stratify_vars1.4$df2 <- stratify_vars1.4$df
    
    varnames1.4$df <- names(stratify_vars$df_full)
    
    
  })
  
  
  ##create the appropriate stratify_vars1.4$df 
  observeEvent(input$heatmap1, {
    
    if(!input$heatmaporder %in% c("ID", "None", "Compliance")) {
      stratify_vars1.4$df <- stratify_vars$df_full %>% select_("ID", input$heatmaporder) %>%
        group_by(ID) %>%
        summarise_at(input$heatmaporder, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                   ifelse(is.numeric(x), mean(x, na.rm = T), NA))
        return(y)}) %>% 
        ungroup() 
    }
    
    else {stratify_vars1.4$df <- stratify_vars$df_sub %>% select_("ID") %>% mutate(dummy_s = 1)}
    
  })
  
  ##Color vars dataset, upon action button, stratify_vars2.2$df2 will update based on variable and quantile selection
  
  observeEvent(input$heatmap1, ignoreInit = T, {
    
    stratify_vars1.4$df2 <-  
      
      if(input$heatmaporder %in% c("None", "Compliance")) {stratify_vars1.4$df}
    
    else {stratify_vars1.4$df %>% 
        rename_at(vars(input$heatmaporder), ~ paste0(input$heatmaporder, "_s"))
      
    }
  })
  
  #Random plot
  
  #make plot update after new variables are selected
  heatmapvariables <- reactiveValues(var = "ID", order = "Compliance")
  
  #default heatmap var
  observeEvent(input$go, priority = -1, {
    
    heatmapvariables$var <- names(dataset())[[sample(1:length(names(dataset())), 1)]]
    
  })
  
  
  
  heatmaprandbutton <- reactiveValues(r1=NULL, r2=NULL)
  
  observeEvent(input$heatmaprandom, priority = 2, ignoreInit = T, {
    
    heatmaprandbutton$r1 <- sample(1:length(names(dataset())), 1)
    
    heatmaprandbutton$r2 <- sample(1:length(varnames1.4$df), 1)
    
    
    if("Main Var." %in% input$heatmaprand_choice) {heatmapvariables$var <- names(dataset())[[heatmaprandbutton$r1]]}
    
    if("Order Var." %in% input$heatmaprand_choice) {heatmapvariables$order <- varnames1.4$df[[heatmaprandbutton$r2]]}
    
  })
  
  
  ##Data for Compliance boxplots
  
  heatmapdata <- reactiveValues(l=NULL, m=NULL)
  
  observeEvent(input$heatmap1, ignoreInit = T, {
    
    #Main heatmap
    
    heatmapdata$l <-   
      
      if(input$heatmaporder %in% "None"){
        
        dataset() %>% 
          select_("ID", "timepoint", input$heatmapvar) %>%
          group_by_("ID") %>%
          summarise_all(countna) %>%
          left_join(stratify_vars1.4$df2, by="ID")
        
      }
    
    else if (input$heatmaporder %in% "Compliance") {
      
      dataset() %>% 
        select_("ID", "timepoint", input$heatmapvar) %>%
        group_by_("ID") %>%
        summarise_all(countna) %>%
        left_join(stratify_vars1.4$df2, by="ID") %>%
        arrange_(input$heatmapvar)
    }
    
    else {
      
      dataset() %>% 
        select_("ID", "timepoint", input$heatmapvar) %>%
        group_by_("ID") %>%
        summarise_all(countna) %>%
        left_join(stratify_vars1.4$df2, by="ID") %>%
        arrange_(varorder1.4$l, input$heatmapvar)
    }
    
    #Time of day plot
    
    heatmapdata$m <- 
      
      if (input$heatmapraw %in% "raw") {
        
        dataset() %>% 
          select_("ID", "timeofday", "weekday", input$heatmapvar) %>%
          group_by_("ID", input$heatmapstrat) %>%
          summarise_all(countna) %>%
          left_join(stratify_vars1.4$df2, by="ID")
      }
    
    else if (input$heatmapraw %in% "subject normalized") {
      
      if (input$heatmapvar %in% "ID") {
        
        dataset() %>% 
          select_("ID", "timeofday", "weekday", input$heatmapvar) %>%
          group_by_("ID", input$heatmapstrat) %>%
          summarise_all(countna) %>%
          left_join(stratify_vars1.4$df2, by="ID")
      }
      
      else {
        
        dataset() %>% 
          select_("ID", "timeofday", "weekday", input$heatmapvar) %>%
          group_by_("ID") %>%
          mutate_at(input$heatmapvar, .funs = funs(normalize)) %>%
          ungroup() %>%
          group_by_("ID", input$heatmapstrat) %>%
          summarise_all(countna) %>%
          left_join(stratify_vars1.4$df2, by="ID")
      }
    }
    
  })
  
  
  #dummy variable to control plot, since plot runs automatically when page is selected, will make it dependent on a reactive value that updates 
  #once the "create plot" button is clicked.
  
  heatmap_dummy <- reactiveValues(l=0)
  
  observeEvent(input$heatmap1, ignoreInit = T, {
    if (heatmap_dummy$l==0) {heatmap_dummy$l <- 1}
    else NULL
  })
  
  ##Reset plot when dataset changes
  
  observeEvent(input$go, {
    heatmap_dummy$l <- 0
    
  })
  
  ##Instructions that appear before create plot button is clicked
  
  output$heatmap_instr <-  renderText(
    if(heatmap_dummy$l==0) {"Click the [Create/Update Plot] button to generate plot!"}
    else NULL 
  )
  
  output$heatmap <- renderPlotly({
    
    input$heatmap1
    
    isolate(
      
      if(heatmap_dummy$l==0) NULL
      
      else {
        
        if (varorder1.4$l  %in% c("None", "Compliance")) {
          
          ggplotly(ggplot(heatmapdata$l) + 
                     geom_tile(aes_string(y="ID", x=factor("All"), fill=input$heatmapvar)) +
                     labs(y="", x="", title=input$heatmapvar, fill="") +
                     scale_fill_distiller(palette = "RdYlGn", direction = 1) +
                     scale_y_discrete(limits=eval(parse(text=paste0("heatmapdata$l$ID")))) +
                     theme_bw() +
                     theme(plot.margin = margin(t = 30, b = 10),
                           axis.text.y = element_blank(),
                           axis.ticks.x = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.title.y = element_blank(),
                           legend.title = element_blank(),
                           panel.border = element_blank(),
                           panel.background = element_blank(),
                           panel.grid = element_blank())
          )
        }
        
        else {
          
          ggplotly(ggplot(heatmapdata$l, aes_string(label=varorder1.4$l)) + 
                     geom_tile(aes_string(y="ID", x=factor("All"), fill=input$heatmapvar)) +
                     labs(y="", x="", title=input$heatmapvar, fill="") +
                     scale_fill_distiller(palette = "RdYlGn", direction = 1) +
                     scale_y_discrete(limits=eval(parse(text=paste0("heatmapdata$l$ID")))) +
                     theme_bw() +
                     theme(plot.margin = margin(t = 30, b = 10),
                           axis.text.y = element_blank(),
                           axis.ticks.x = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.title.y = element_blank(),
                           legend.title = element_blank(),
                           panel.border = element_blank(),
                           panel.background = element_blank(),
                           panel.grid = element_blank())
                   
          )
        } 
      }
    )
  })
  
  
  ##Code for timeofday/weekday heatmap
  output$heatmapTOD <- renderPlotly({
    
    input$heatmap1
    
    isolate(
      
      if(heatmap_dummy$l==0) NULL
      
      else {
        
        if (varorder1.4$l  %in% c("None", "Compliance")) {
          
          ggplotly(ggplot(heatmapdata$m) + 
                     geom_tile(aes_string(x=input$heatmapstrat, y="ID", fill=input$heatmapvar)) +
                     labs(x="", y="Subject ID", title=paste0(input$heatmapvar, " ", input$heatmapstrat), fill="") +
                     scale_fill_distiller(palette = "RdYlGn", direction = 1) +
                     scale_x_discrete(limits = levels(eval(parse(text=paste0("heatmapdata$m$", input$heatmapstrat))))) +
                     scale_y_discrete(limits=eval(parse(text=paste0("heatmapdata$m$ID")))) +
                     theme_bw() +
                     theme(plot.margin = margin(t = 30, b = 10),
                           axis.text.y = element_blank(),
                           axis.ticks.x = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.title.y = element_blank(),
                           legend.title = element_blank(),
                           panel.border = element_blank(),
                           panel.background = element_blank(),
                           panel.grid = element_blank())
                   
          ) 
        }
        
        else {
          
          ggplotly(ggplot(heatmapdata$m, aes_string(label=varorder1.4$l)) + 
                     geom_tile(aes_string(x=input$heatmapstrat, y="ID", fill=input$heatmapvar)) +
                     labs(x="", y="Subject ID", title=paste0(input$heatmapvar, " ", input$heatmapstrat), fill="") +
                     scale_fill_distiller(palette = "RdYlGn", direction = 1) +
                     scale_x_discrete(limits = levels(eval(parse(text=paste0("heatmapdata$m$", input$heatmapstrat))))) +
                     scale_y_discrete(limits=eval(parse(text=paste0("heatmapdata$m$ID")))) +
                     theme_bw() +
                     theme(plot.margin = margin(t = 30, b = 10),
                           axis.text.y = element_blank(),
                           axis.ticks.x = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.title.y = element_blank(),
                           legend.title = element_blank(),
                           panel.border = element_blank(),
                           panel.background = element_blank(),
                           panel.grid = element_blank())
          )
        }
      }
    )
  })
  

#Responses Page 1, histogram of responses and missing, codebook--------------------------------------------------------------------------------
  

  ##datatable for histogram
  output$table3 <- DT::renderDataTable(data(), selection = list(selected = 8, mode = 'single'),
                                       options = list(columnDefs = list(list(
                                         targets = 1,
                                         render = JS(
                                           "function(data, type, row, meta) {",
                                           "return type === 'display' && data.length > 12 ?",
                                           "'<span title=\"' + data + '\">' + data.substr(0, 12) + '...</span>' : data;",
                                           "}")
                                       ))), callback = JS('table.page(3).draw(false);'))
  
  
  
  
  #Select main variable
  output$rhistvar <- renderUI({selectInput('rhistvar', 'Main Variable:', c(names(dataset())), selected = rhistvariables$var, selectize=TRUE)})
  
  
  #Select coloring variable
  output$rhistcolor <- renderUI({selectInput('rhistcolor', 'Color By:', c("None", varnames2.1$df), selected = rhistvariables$color, selectize=TRUE)})
  
  
  #hist color variable
  varcolor2.1 <- reactiveValues(l="ID")
  
  observeEvent(input$rhist1, {if (input$rhistcolor %in% c("ID", "None")) {varcolor2.1$l <- input$rhistcolor}
    else {varcolor2.1$l <- paste0(input$rhistcolor, "_s")} 
  })
  
  # output$boxselectR <- renderUI({selectInput('boxselectR', 'Stratify by:', c("None", varnames$l), selectize=TRUE)})
  
  
  ##Use varnames so updating stratify_vars3.1$df2 doesn't refresh subbrowsecolor
  varnames2.1 <- reactiveValues(df=NULL)
  
  
  ##Color datasets
  stratify_vars2.1 <- reactiveValues(df = NULL, df2 = NULL)
  
  ##Default datasets
  observeEvent(input$go, priority = -1, {
    
    stratify_vars2.1$df <- stratify_vars$df_full %>% select("ID", "timeindex") %>% mutate(dummy_s=1)
    stratify_vars2.1$df2 <- stratify_vars2.1$df
    
    varnames2.1$df <- names(stratify_vars$df_full)
    
    
  })
  
  
  ##create the appropriate stratify_vars2.1$df 
  observeEvent(input$rhist1, {
    
    if(!input$rhistcolor %in% c("ID", "None")) {
      stratify_vars2.1$df <- stratify_vars$df_full %>% select_("ID", "timeindex", input$rhistcolor)
      
    }
    
    else {stratify_vars2.1$df <- stratify_vars$df_full %>% select_("ID", "timeindex") %>% mutate(dummy_s = 1)}
    
  })
  
  ##Color vars dataset, upon action button, stratify_vars2.1$df2 will update based on variable and quantile selection
  
  observeEvent(input$rhist1, ignoreInit = T, {
    
    stratify_vars2.1$df2 <-  
      
      
      if(input$rhistcolor %in% c("ID", "None")) {stratify_vars2.1$df}
    
    else {
      if(input$rhistradio %in% "Auto"){
        stratify_vars2.1$df %>% 
          mutate_at(input$rhistcolor, function (x) { if (is.character(x) | is.factor(x)) {x}
            else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) > 20) {as.character(my_ntiles(x, input$rhistntile))}
            else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) <= 20) {as.character(factor(x, ordered = T, exclude = c(NA, "NaN")))}
            else {x=NA}
          }) %>% 
          rename_at(vars(input$rhistcolor), ~ paste0(input$rhistcolor, "_s"))
      }
      
      
  
      else if (input$rhistradio %in% "On"){
        stratify_vars2.1$df %>% 
          mutate_at(input$rhistcolor, ~my_ntiles(.x, input$rhistntile)) %>%
          rename_at(vars(input$rhistcolor), ~ paste0(input$rhistcolor, "_s"))
        
      }
      
      else if (input$rhistradio %in% "Off"){
        stratify_vars2.1$df %>% 
          mutate_at(input$rhistcolor, as.character) %>%
          rename_at(vars(input$rhistcolor), ~ paste0(input$rhistcolor, "_s"))
        
      }
    }
    
  })
  
  
  
  #Random plot
  
  #make plot update after new variables are selected
  
  rhistvariables <- reactiveValues(var = "ID", color = "None")
  
  
  #Default hist variable
  observeEvent(input$go, priority = -1, {
    
    rhistvariables$var <- names(dataset())[[sample(1:length(names(dataset())), 1)]]
    
  })
  
  rhistrandbutton <- reactiveValues(r1=NULL)
  
  observeEvent(input$rhistrandom, priority = 2, ignoreInit = T, {
    
    rhistrandbutton$r1 <- sample(1:length(names(dataset())), 1)
    
    rhistrandbutton$r2 <- sample(1:length(varnames2.2$df), 1)
    
    
    if("Main Var." %in% input$rhistrand_choice) {rhistvariables$var <- names(dataset())[[rhistrandbutton$r1]]}
    
    if("Color Var." %in% input$rhistrand_choice) {rhistvariables$color <- varnames2.1$df[[rhistrandbutton$r2]]}
    
  })
  
  
  ##Data for response hists
  
  rhistdata <- reactiveValues(l=NULL, m=NULL)
  
  
  missinghist <- function(x) {ifelse(is.na(x), 1, NA)}
  
  observeEvent(input$rhist1, ignoreInit = T, {
    
    rhistdata$l <- 
      dataset() %>% 
      select_("ID", "timeindex", input$rhistvar) %>%
      mutate_at(input$rhistvar, as.factor) %>%
      left_join(stratify_vars2.1$df2) %>%
      filter(!is.na((!!sym(input$rhistvar)))) 
    
    
    rhistdata$m <-
      dataset() %>% 
      select_("ID", "timeindex", input$rhistvar) %>%
      mutate_at(input$rhistvar, as.factor) %>%
      left_join(stratify_vars2.1$df2) %>% 
      mutate_at(input$rhistvar, funs(na=missinghist)) %>%
      filter(!is.na(na))
    
    
  })
  
  #output$test1 <- renderText(names(rhistdata$m))
  
  
  #dummy variable to control plot, since plot runs automatically when page is selected, will make it dependent on a reactive value that updates 
  #once the "create plot" button is clicked.
  
  rhist_dummy <- reactiveValues(l=0)
  
  observeEvent(input$rhist1, ignoreInit = T, {
    if (rhist_dummy$l==0) {rhist_dummy$l <- 1}
    else NULL
  })
  
  ##Reset plot when dataset changes
  
  observeEvent(input$go, {
    rhist_dummy$l <- 0
    
  })
  
  ##Instructions that appear before create plot button is clicked
  
  output$rhist_instr <-  renderText(
    if(rhist_dummy$l==0) {"Click the [Create/Update Plot] button to generate plot!"}
    else NULL 
  )
  
  ##code for histogram
  
  output$histR <- renderPlotly({ 
    
    input$rhist1
    
    
    isolate(
      
      if(rhist_dummy$l==0) NULL
      
      else {
      
      if(length(unique(rhistdata$l[[input$rhistvar]])) > 20) {
        
        if(varcolor2.1$l %in% c("ID", "None")) {
          
          ggplotly(ggplot(data=rhistdata$l, aes_string(x=input$rhistvar)) +
                     geom_histogram(color = "blue", fill = "blue", position = "dodge", stat = "count") +
                     theme_bw())
        }
        
        else{
          
          ggplotly(ggplot(data=rhistdata$l, aes_string(x=input$rhistvar, fill = varcolor2.1$l, color = varcolor2.1$l)) +
                     geom_histogram(position = "dodge", stat = "count") +
                     theme_bw())
          
        
        
        }
      }
        
      else {
      
        if(varcolor2.1$l %in% c("ID", "None")) {
        
          ggplotly(ggplot(data=rhistdata$l, aes_string(x=input$rhistvar)) +
                   geom_bar(color = "blue", fill = "blue", position = "dodge", stat = "count") +
                   scale_x_discrete() +
                   theme_bw())
        }
      
        else{
        
        ggplotly(ggplot(data=rhistdata$l, aes_string(x=input$rhistvar, fill = varcolor2.1$l, color = varcolor2.1$l)) +
                   geom_bar(position = "dodge", stat = "count") +
                   scale_x_discrete() +
                   theme_bw())
        
        }
      }
    }
    )
    
  })
  
  ##code for missing hist
  
  output$missinghistR <- renderPlotly({ 
    
    input$rhist1
    
    
    isolate(
      
      if(rhist_dummy$l==0) NULL
      
      else {
        
        
        if(varcolor2.1$l %in% c("ID", "None")) {
          
          ggplotly(ggplot(data=rhistdata$m, aes_string(x="na")) +
                     geom_bar(color = "blue", fill = "blue", position = "dodge", stat = "count") +
                     scale_x_discrete() +
                     theme_bw() +
                     coord_flip())
        }
        
        else{
          
          ggplotly(ggplot(data=rhistdata$m, aes_string(x="na", fill = varcolor2.1$l, color = varcolor2.1$l)) +
                     geom_bar(position = "dodge", stat = "count") +
                     scale_x_discrete() +
                     theme_bw()+ 
                     coord_flip())
          
        }
      }
    )
    
  })
  
  

  #Responses Page 2, boxplot of responses-----------------------------------------------------------------------------------------------------
  
 
                            #legend.position="none")+ ##data table for boxplot
  output$table4 <- DT::renderDataTable(data(), selection = list(selected = 8, mode = 'single'),
                                       options = list(columnDefs = list(list(
                                         targets = 1,
                                         render = JS(
                                           "function(data, type, row, meta) {",
                                           "return type === 'display' && data.length > 12 ?",
                                           "'<span title=\"' + data + '\">' + data.substr(0, 12) + '...</span>' : data;",
                                           "}")
                                       ))), callback = JS('table.page(3).draw(false);'))
  
  
  #Select main variable
  output$rboxplotvar <- renderUI({selectInput('rboxplotvar', 'Main Variable:', c(names(dataset())), selected = rboxplotvariables$var, selectize=TRUE)})
  
  
  
  #Select coloring variable
  output$rboxplotcolor <- renderUI({selectInput('rboxplotcolor', 'Color By:', c("None", varnames2.2$df), selected = rboxplotvariables$color, selectize=TRUE)})
  
  
  #boxplot color variable
  varcolor2.2 <- reactiveValues(l="ID")
  
  observeEvent(input$rboxplot1, {if (input$rboxplotcolor %in% c("ID", "None")) {varcolor2.2$l <- input$rboxplotcolor}
    else {varcolor2.2$l <- paste0(input$rboxplotcolor, "_s")} 
  })
  
 # output$boxselectR <- renderUI({selectInput('boxselectR', 'Stratify by:', c("None", varnames$l), selectize=TRUE)})
  

  ##Use varnames so updating stratify_vars3.1$df2 doesn't refresh subbrowsecolor
  varnames2.2 <- reactiveValues(df=NULL)
  
  
  ##Color datasets
  stratify_vars2.2 <- reactiveValues(df = NULL, df2 = NULL)
  
  ##Default datasets
  observeEvent(input$go, priority = -1, {
    
    stratify_vars2.2$df <- stratify_vars$df_sub %>% select("ID") %>% mutate(dummy_s=1)
    stratify_vars2.2$df2 <- stratify_vars2.2$df
    
    varnames2.2$df <- names(stratify_vars$df_full)
    
    
  })

  
  ##create the appropriate stratify_vars2.2$df 
  observeEvent(input$rboxplot1, {
    
    if(!input$rboxplotcolor %in% c("ID", "None")) {
      stratify_vars2.2$df <- stratify_vars$df_full %>% select_("ID", input$rboxplotcolor) %>%
        group_by(ID) %>%
        summarise_at(input$rboxplotcolor, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                      ifelse(is.numeric(x), mean(x, na.rm = T), NA))
        return(y)}) %>% 
        ungroup() 
    }
    
    else {stratify_vars2.2$df <- stratify_vars$df_sub %>% select_("ID") %>% mutate(dummy_s = 1)}
    
  })
  
  ##Color vars dataset, upon action button, stratify_vars2.2$df2 will update based on variable and quantile selection

  observeEvent(input$rboxplot1, ignoreInit = T, {
    
    stratify_vars2.2$df2 <-  
    
    
    if(input$rboxplotcolor %in% c("ID", "None")) {stratify_vars2.2$df}
    
    else {
      if(input$rboxplotradio %in% "Auto"){
        stratify_vars2.2$df %>% 
          mutate_at(input$rboxplotcolor, function (x) { if (is.character(x) | is.factor(x)) {x}
            else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) > 20) {my_ntiles(x, input$rboxplotntile)}
            else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) <= 20) {factor(x, ordered = T, exclude = c(NA, "NaN"))}
            else {x=NA}
          }) %>% 
          rename_at(vars(input$rboxplotcolor), ~ paste0(input$rboxplotcolor, "_s"))
      }
      
      else if (input$rboxplotradio %in% "On"){
        stratify_vars2.2$df %>% 
          mutate_at(input$rboxplotcolor, ~my_ntiles(.x, input$rboxplotntile)) %>%
          rename_at(vars(input$rboxplotcolor), ~ paste0(input$rboxplotcolor, "_s"))
        
      }
      
      else if (input$rboxplotradio %in% "Off"){
        stratify_vars2.2$df %>% 
          rename_at(vars(input$rboxplotcolor), ~ paste0(input$rboxplotcolor, "_s"))
        
      }
    }
    
  })

  
  #Random plot

  #make plot update after new variables are selected
  
  rboxplotvariables <- reactiveValues(var = "ID", color = "None")
  
  #Default boxplot variable
  observeEvent(input$go, priority = -1, {
    
    rboxplotvariables$var <- names(dataset())[[sample(1:length(names(dataset())), 1)]]
    
  })
  
  rboxplotrandbutton <- reactiveValues(r1=NULL, r2=NULL)
  
  observeEvent(input$rboxplotrandom, priority = 2, ignoreInit = T, {
    
    rboxplotrandbutton$r1 <- sample(1:length(names(dataset())), 1)
    
    rboxplotrandbutton$r2 <- sample(1:length(varnames2.2$df), 1)

    
    if("Main Var." %in% input$rboxplotrand_choice) {rboxplotvariables$var <- names(dataset())[[rboxplotrandbutton$r1]]}
    
    if("Color Var." %in% input$rboxplotrand_choice) {rboxplotvariables$color <- varnames2.2$df[[rboxplotrandbutton$r2]]}
    
  })

  
  ##Data for response boxplots
  
  rboxplotdata <- reactiveValues(l=NULL, m=NULL)
  
  observeEvent(input$rboxplot1, ignoreInit = T, {
  
    
    rboxplotdata$l <- 
      dataset() %>% 
      select_("ID", "timepoint", input$rboxplotvar) %>%
      group_by_("ID") %>%
      summarise_all(mymean) %>%
      left_join(stratify_vars2.2$df2, by="ID")
    
    #time of day boxplot
    rboxplotdata$m <- 
      dataset() %>% 
      select_("ID", "timepoint", "timeofday", input$rboxplotvar) %>%
      group_by_("ID", "timeofday") %>%
      summarise_all(mymean)
      
    })
  
  
  
  #dummy variable to control plot, since plot runs automatically when page is selected, will make it dependent on a reactive value that updates 
  #once the "create plot" button is clicked.
  
  rboxplot_dummy <- reactiveValues(l=0)
  
  observeEvent(input$rboxplot1, ignoreInit = T, {
    if (rboxplot_dummy$l==0) {rboxplot_dummy$l <- 1}
    else NULL
  })
  
  ##Reset plot when dataset changes
  
  observeEvent(input$go, {
    rboxplot_dummy$l <- 0
    
  })
  
  ##Instructions that appear before create plot button is clicked
  
  output$rboxplot_instr <-  renderText(
    if(rboxplot_dummy$l==0) {"Click the [Create/Update Plot] button to generate plot!"}
    else NULL 
  )
  
  
  ##Main boxplot code
  output$boxplot_dR <- renderPlotly({
    
    input$rboxplot1
    
    isolate(
      
      if(rboxplot_dummy$l==0) NULL
      
      else {
        
        if (varcolor2.2$l  %in% "None") {
          
          ggplotly(ggplot(data=rboxplotdata$l, aes_string(x=factor(0),
                                                          y=input$rboxplotvar,
                                                          label="ID"))+ 
                     geom_boxplot(size=.5, fatten=1)+
                     geom_jitter(alpha=0.2, size=1.2, shape=19, position = position_jitter(w = 0.5, h = 0.1))+
                     stat_summary(fun.data = mean_cl_normal, geom="pointrange", size=2, alpha=.8, 
                                  position=position_dodge(width=0.75), shape=4, color="firebrick")+
                     labs(y=NULL, x=NULL, title=input$rboxplotvar)+
                     theme_bw()+
                     theme(axis.title.x=element_blank(), axis.title.y=element_text(size=8), 
                           axis.text.x = element_text(size=8, vjust = 1, color="gray65"), 
                           axis.text.y = element_blank(),
                           axis.line = element_line(color="gray65", size=0.5),
                           axis.ticks.y = element_blank(),
                           axis.ticks.x = element_line(colour = "gray"),
                           panel.grid.major = element_blank(), 
                           panel.grid.minor = element_blank(), 
                           panel.background = element_blank(),
                           panel.border = element_blank(),
                           #plot.margin = unit( c(0,3,3,0) , "in"),
                           aspect.ratio = 0.3) +
                     #legend.position="none")+
                     coord_flip()
                   #coord_flip(ylim=c(0, max(boxdata_dR()[4])), expand = c(0.1,0))+
                   #scale_y_continuous(breaks=seq(0, max(boxdata_dR()[2]), by = 4))
          )
        }
        
        else {  
          
          ggplotly(ggplot(data=rboxplotdata$l, aes_string(x=varcolor2.2$l ,
                                                          y=input$rboxplotvar,
                                                          label="ID", color=varcolor2.2$l))+ 
                     geom_boxplot(size=.5, fatten=1)+
                     geom_jitter(alpha=0.2, size=1.2, shape=19, position = position_jitter(w = 0.5, h = 0.1))+
                     stat_summary(fun.data = mean_cl_normal, geom="pointrange", size=2, alpha=.8, 
                                  position=position_dodge(width=0.75), shape=4, color="firebrick")+
                     #stat_summary(fun.data = give.n, geom = "text", color="firebrick", size=4) +
                     labs(y=NULL, x=NULL, title=input$rboxplotvar)+
                            theme_bw()+
                            theme(axis.title.x=element_blank(), axis.title.y=element_text(size=8), 
                                  axis.text.x = element_text(size=8, vjust = 1, color="gray65"), 
                                  axis.text.y = element_text(size=10),
                                  axis.line = element_line(color="gray65", size=0.5),
                                  axis.ticks.y = element_blank(),
                                  axis.ticks.x = element_line(colour = "gray"),
                                  panel.grid.major = element_blank(), 
                                  panel.grid.minor = element_blank(), 
                                  panel.background = element_blank(),
                                  panel.border = element_blank(),
                                  plot.margin = unit( c(0.5,0.5,0.5,1) , "cm"),
                                  aspect.ratio = 0.3)+
                            coord_flip()
                          #coord_flip(ylim=c(0, max(boxdata_dR()[4])), expand = c(0.1,0))+
                          #scale_y_continuous(breaks=seq(0, max(boxdata_dR()[2]), by = 4))
                     )
          
          
        }
      }
      
    )
    
  })
  
  
  ##Timeofday boxplot code
  
  output$boxplotR <- renderPlotly({
    
    input$rboxplot1
    
    isolate(
      
      if(rboxplot_dummy$l==0) NULL
      
      else {
        
        ggplotly(ggplot(data=rboxplotdata$m, aes_string(x="timeofday", 
                                                        y=input$rboxplotvar, 
                                                        color="timeofday", label="ID"))+
                   geom_boxplot(size=.5, fatten=1)+
                   geom_jitter(alpha=0.2, size=1.2, shape=19, position = position_jitter(w = 0.5, h = 0.1))+
                   stat_summary(fun.data = mean_cl_normal, geom="pointrange", size=2, alpha=.8, 
                                position=position_dodge(width=0.75), shape=4, color="firebrick")+
                   labs(y=NULL, x=NULL, title="")+
                   scale_x_discrete(limits = rev(levels(rboxplotdata$m[["timeofday"]])))+
                   theme_bw()+
                   theme(axis.title.x=element_blank(), axis.title.y=element_text(size=8), 
                         axis.text.x = element_text(size=8, vjust = 1, color="gray65"), 
                         axis.text.y = element_text(size=10),
                         axis.line = element_line(color="gray65", size=0.5),
                         axis.ticks.y = element_blank(),
                         axis.ticks.x = element_line(colour = "gray"),
                         panel.grid.major = element_blank(), 
                         panel.grid.minor = element_blank(), 
                         panel.background = element_blank(),
                         panel.border = element_blank(),
                         #plot.margin = unit( c(0,3,3,0) , "in"),
                         aspect.ratio = 0.2,
                         legend.position="none") +
                   coord_flip()
                 # scale_y_continuous(breaks=c(0:max(boxdataR()[3])))
        )
        
      }
      
    )
    
  })
 
  #Responses Page 3, heatmap of responses-----------------------------------------------------------------------------------------------------
  
  ##datatable for heatmaps
  output$table5 <- DT::renderDataTable(data(), selection = list(selected = 8, mode = 'single'),
                                       options = list(columnDefs = list(list(
                                         targets = 1,
                                         render = JS(
                                           "function(data, type, row, meta) {",
                                           "return type === 'display' && data.length > 12 ?",
                                           "'<span title=\"' + data + '\">' + data.substr(0, 12) + '...</span>' : data;",
                                           "}")
                                       ))), callback = JS('table.page(3).draw(false);'))
  
  
  #Select main variable
  output$rheatmapvar <- renderUI({selectInput('rheatmapvar', 'Main Variable:', c(names(dataset())), selected = rheatmapvariables$var, selectize=TRUE)})
  
  
  
  #Select ordering variable
  output$rheatmaporder <- renderUI({selectInput('rheatmaporder', 'Order By:', c("None", "Response", varnames2.3$df), selected = rheatmapvariables$order, selectize=TRUE)})
  
  
  #heatmap order variable
  varorder2.3 <- reactiveValues(l="ID")
  
  observeEvent(input$rheatmap1, {if (input$rheatmaporder %in% c("ID", "None", "Response")) {varorder2.3$l <- input$rheatmaporder}
    else {varorder2.3$l <- paste0(input$rheatmaporder, "_s")} 
  })
  
  
  ##Use varnames so updating stratify_vars3.1$df2 doesn't refresh subbrowseorder
  varnames2.3 <- reactiveValues(df=NULL)
  
  
  ##order datasets
  stratify_vars2.3 <- reactiveValues(df = NULL, df2 = NULL)
  
  ##Default datasets
  observeEvent(input$go, priority = -1, {
    
    stratify_vars2.3$df <- stratify_vars$df_sub %>% select("ID") %>% mutate(dummy_s=1)
    stratify_vars2.3$df2 <- stratify_vars2.3$df
    
    varnames2.3$df <- names(stratify_vars$df_full)
    
    
  })
  
  
  ##create the appropriate stratify_vars2.3$df 
  observeEvent(input$rheatmap1, {
    
    if(!input$rheatmaporder %in% c("ID", "None", "Response")) {
        stratify_vars2.3$df <- stratify_vars$df_full %>% select_("ID", input$rheatmaporder) %>%
          group_by(ID) %>%
          summarise_at(input$rheatmaporder, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                      ifelse(is.numeric(x), mean(x, na.rm = T), NA))
          return(y)}) %>% 
          ungroup() 
      }
    
    else {stratify_vars2.3$df <- stratify_vars$df_sub %>% select_("ID") %>% mutate(dummy_s = 1)}
    
  })
  
  ##Color vars dataset, upon action button, stratify_vars2.2$df2 will update based on variable and quantile selection
  
  observeEvent(input$rheatmap1, ignoreInit = T, {
    
    stratify_vars2.3$df2 <-  
      
      if(input$rheatmaporder %in% c("None", "Response")) {stratify_vars2.3$df}
    
      else {stratify_vars2.3$df %>% 
            rename_at(vars(input$rheatmaporder), ~ paste0(input$rheatmaporder, "_s"))
      
    }
  })
  
  #Random plot
  
  #make plot update after new variables are selected
  rheatmapvariables <- reactiveValues(var = "ID", order = "Response")
  
  #default heatmap var
  observeEvent(input$go, priority = -1, {
   
  rheatmapvariables$var <- names(dataset())[[sample(1:length(names(dataset())), 1)]]
  
  })
  
  
  
  rheatmaprandbutton <- reactiveValues(r1=NULL, r2=NULL)
  
  observeEvent(input$rheatmaprandom, priority = 2, ignoreInit = T, {
    
    rheatmaprandbutton$r1 <- sample(1:length(names(dataset())), 1)
    
    rheatmaprandbutton$r2 <- sample(1:length(varnames2.3$df), 1)
    
    
    if("Main Var." %in% input$rheatmaprand_choice) {rheatmapvariables$var <- names(dataset())[[rheatmaprandbutton$r1]]}
    
    if("Order Var." %in% input$rheatmaprand_choice) {rheatmapvariables$order <- varnames2.3$df[[rheatmaprandbutton$r2]]}
    
  })
  
  
  ##Data for response boxplots
  
  rheatmapdata <- reactiveValues(l=NULL, m=NULL)
  
  observeEvent(input$rheatmap1, ignoreInit = T, {
    
    #Main heatmap
    
    rheatmapdata$l <-   
    
    if(input$rheatmaporder %in% "None"){
    
        dataset() %>% 
          select_("ID", "timepoint", input$rheatmapvar) %>%
          group_by_("ID") %>%
          summarise_all(mymean) %>%
          left_join(stratify_vars2.3$df2, by="ID")
        
    }
    
    else if (input$rheatmaporder %in% "Response") {
      
      dataset() %>% 
        select_("ID", "timepoint", input$rheatmapvar) %>%
        group_by_("ID") %>%
        summarise_all(mymean) %>%
        left_join(stratify_vars2.3$df2, by="ID") %>%
        arrange_(input$rheatmapvar)
    }
    
    else {
    
      dataset() %>% 
        select_("ID", "timepoint", input$rheatmapvar) %>%
        group_by_("ID") %>%
        summarise_all(mymean) %>%
        left_join(stratify_vars2.3$df2, by="ID") %>%
        arrange_(varorder2.3$l, input$rheatmapvar)
    }
    
    #Time of day plot
    
    rheatmapdata$m <- 
    
    if (input$rheatmapraw %in% "raw") {
      
        dataset() %>% 
        select_("ID", "timeofday", "weekday", input$rheatmapvar) %>%
        group_by_("ID", input$rheatmapstrat) %>%
        summarise_all(mymean) %>%
        left_join(stratify_vars2.3$df2, by="ID")
    }
    
    else if (input$rheatmapraw %in% "subject normalized") {
      
      if (input$rheatmapvar %in% "ID") {
        
        dataset() %>% 
          select_("ID", "timeofday", "weekday", input$rheatmapvar) %>%
          group_by_("ID", input$rheatmapstrat) %>%
          summarise_all(mymean) %>%
          left_join(stratify_vars2.3$df2, by="ID")
      }
      
      else {
   
        dataset() %>% 
        select_("ID", "timeofday", "weekday", input$rheatmapvar) %>%
        group_by_("ID") %>%
        mutate_at(input$rheatmapvar, .funs = funs(normalize)) %>%
        ungroup() %>%
        group_by_("ID", input$rheatmapstrat) %>%
        summarise_all(mymean) %>%
        left_join(stratify_vars2.3$df2, by="ID")
      }
    }
    
  })
  

  #dummy variable to control plot, since plot runs automatically when page is selected, will make it dependent on a reactive value that updates 
  #once the "create plot" button is clicked.
  
  rheatmap_dummy <- reactiveValues(l=0)
  
  observeEvent(input$rheatmap1, ignoreInit = T, {
    if (rheatmap_dummy$l==0) {rheatmap_dummy$l <- 1}
    else NULL
  })
  
  ##Reset plot when dataset changes
  
  observeEvent(input$go, {
    rheatmap_dummy$l <- 0
    
  })
  
  ##Instructions that appear before create plot button is clicked
  
  output$rheatmap_instr <-  renderText(
    if(rheatmap_dummy$l==0) {"Click the [Create/Update Plot] button to generate plot!"}
    else NULL 
  )
  
  output$heatmapR <- renderPlotly({
    
    input$rheatmap1
    
    isolate(
      
      if(rheatmap_dummy$l==0) NULL
      
      else {
        
        if (varorder2.3$l  %in% c("None", "Response")) {
          
          ggplotly(ggplot(rheatmapdata$l) + 
                     geom_tile(aes_string(y="ID", x=factor("All"), fill=input$rheatmapvar)) +
                     labs(y="", x="", title=input$rheatmapvar, fill="") +
                     scale_fill_distiller(palette = "RdYlGn", direction = 1) +
                     scale_y_discrete(limits=eval(parse(text=paste0("rheatmapdata$l$ID")))) +
                     theme_bw() +
                     theme(plot.margin = margin(t = 30, b = 10),
                           axis.text.y = element_blank(),
                           axis.ticks.x = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.title.y = element_blank(),
                           legend.title = element_blank(),
                           panel.border = element_blank(),
                           panel.background = element_blank(),
                           panel.grid = element_blank())
          )
        }
        
        else {
          
          ggplotly(ggplot(rheatmapdata$l, aes_string(label=varorder2.3$l)) + 
                     geom_tile(aes_string(y="ID", x=factor("All"), fill=input$rheatmapvar)) +
                     labs(y="", x="", title=input$rheatmapvar, fill="") +
                     scale_fill_distiller(palette = "RdYlGn", direction = 1) +
                     scale_y_discrete(limits=eval(parse(text=paste0("rheatmapdata$l$ID")))) +
                     theme_bw() +
                     theme(plot.margin = margin(t = 30, b = 10),
                           axis.text.y = element_blank(),
                           axis.ticks.x = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.title.y = element_blank(),
                           legend.title = element_blank(),
                           panel.border = element_blank(),
                           panel.background = element_blank(),
                           panel.grid = element_blank())
                   
          )
        } 
      }
    )
  })
  
  
  ##Code for timeofday/weekday heatmap
  output$heatmapTODR <- renderPlotly({
    
    input$rheatmap1
    
    isolate(
      
      if(rheatmap_dummy$l==0) NULL
      
      else {
        
        if (varorder2.3$l  %in% c("None", "Response")) {
          
          ggplotly(ggplot(rheatmapdata$m) + 
                     geom_tile(aes_string(x=input$rheatmapstrat, y="ID", fill=input$rheatmapvar)) +
                     labs(x="", y="Subject ID", title=paste0(input$rheatmapvar, " ", input$rheatmapstrat), fill="") +
                     scale_fill_distiller(palette = "RdYlGn", direction = 1) +
                     scale_x_discrete(limits = levels(eval(parse(text=paste0("rheatmapdata$m$", input$rheatmapstrat))))) +
                     scale_y_discrete(limits=eval(parse(text=paste0("rheatmapdata$m$ID")))) +
                     theme_bw() +
                     theme(plot.margin = margin(t = 30, b = 10),
                           axis.text.y = element_blank(),
                           axis.ticks.x = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.title.y = element_blank(),
                           legend.title = element_blank(),
                           panel.border = element_blank(),
                           panel.background = element_blank(),
                           panel.grid = element_blank())
                   
          ) 
        }
        
        else {
          
          ggplotly(ggplot(rheatmapdata$m, aes_string(label=varorder2.3$l)) + 
                     geom_tile(aes_string(x=input$rheatmapstrat, y="ID", fill=input$rheatmapvar)) +
                     labs(x="", y="Subject ID", title=paste0(input$rheatmapvar, " ", input$rheatmapstrat), fill="") +
                     scale_fill_distiller(palette = "RdYlGn", direction = 1) +
                     scale_x_discrete(limits = levels(eval(parse(text=paste0("rheatmapdata$m$", input$rheatmapstrat))))) +
                     scale_y_discrete(limits=eval(parse(text=paste0("rheatmapdata$m$ID")))) +
                     theme_bw() +
                     theme(plot.margin = margin(t = 30, b = 10),
                           axis.text.y = element_blank(),
                           axis.ticks.x = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.title.y = element_blank(),
                           legend.title = element_blank(),
                           panel.border = element_blank(),
                           panel.background = element_blank(),
                           panel.grid = element_blank())
          )
        }
      }
    )
  })
  
  
  
  
  
  
  #Responses Page 4, trajectory of responses ------------------------------------------------------------------------------
  
  
  ##datatable for trajectory
  output$table6 <- DT::renderDataTable(data(), selection = list(selected = 8, mode = 'single'),
                                       options = list(columnDefs = list(list(
                                         targets = 1,
                                         render = JS(
                                           "function(data, type, row, meta) {",
                                           "return type === 'display' && data.length > 12 ?",
                                           "'<span title=\"' + data + '\">' + data.substr(0, 12) + '...</span>' : data;",
                                           "}")
                                       ))), callback = JS('table.page(3).draw(false);'))
  
  
  
  
  #Select main variable
  output$rtrajvar <- renderUI({selectInput('rtrajvar', 'Main Variable:', c(names(dataset())), selected = rtrajvariables$var, selectize=TRUE)})
  
  
  #Select coloring variable
  output$rtrajcolor <- renderUI({selectInput('rtrajcolor', 'Color By:', c("None", varnames2.4$df), selected = rtrajvariables$color, selectize=TRUE)})
  
  
  #traj color variable
  varcolor2.4 <- reactiveValues(l="ID")
  
  observeEvent(input$rtraj1, {if (input$rtrajcolor %in% c("ID", "None", "weekday")) {varcolor2.4$l <- input$rtrajcolor}
    else {varcolor2.4$l <- paste0(input$rtrajcolor, "_s")} 
  })
  
  # output$boxselectR <- renderUI({selectInput('boxselectR', 'Stratify by:', c("None", varnames$l), selectize=TRUE)})
  
  
  ##Use varnames so updating stratify_vars3.1$df2 doesn't refresh subbrowsecolor
  varnames2.4 <- reactiveValues(df=NULL)
  
  
  ##Color datasets
  stratify_vars2.4 <- reactiveValues(df = NULL, df2 = NULL)
  
  ##Default color datasets
  observeEvent(input$go, priority = -1, {
    
    stratify_vars2.4$df <- stratify_vars$df_sub %>% select("ID") %>% mutate(dummy_s=1)
    stratify_vars2.4$df2 <- stratify_vars2.4$df
    
    varnames2.4$df <- names(stratify_vars$df_full)
    
    
  })
  
  
  ##create the appropriate stratify_vars2.4$df 
  ##create the appropriate stratify_vars2.2$df 
  observeEvent(input$rtraj1, {
    
    if(!input$rtrajcolor %in% c("ID", "None", "weekday")) {
      stratify_vars2.4$df <- stratify_vars$df_full %>% select_("ID", input$rtrajcolor) %>%
        group_by(ID) %>%
        summarise_at(input$rtrajcolor, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                 ifelse(is.numeric(x), mean(x, na.rm = T), NA))
        return(y)}) %>% 
        ungroup() 
    }
    
    else {stratify_vars2.4$df <- stratify_vars$df_sub %>% select_("ID") %>% mutate(dummy_s = 1)}
    
  })
  
  ##Color vars dataset, upon action button, stratify_vars2.4$df2 will update based on variable and quantile selection
  
  observeEvent(input$rtraj1, ignoreInit = T, {
    
    stratify_vars2.4$df2 <-  
      
      
      if(input$rtrajcolor %in% c("ID", "None", "weekday")) {stratify_vars2.4$df}
    
    else {
      if(input$rtrajradio %in% "Auto"){
        stratify_vars2.4$df %>% 
          mutate_at(input$rtrajcolor, function (x) { if (is.character(x) | is.factor(x)) {x}
            else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) > 20) {as.character(my_ntiles(x, input$rtrajntile))}
            else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) <= 20) {as.character(factor(x, ordered = T, exclude = c(NA, "NaN")))}
            else {x=NA}
          }) %>% 
          rename_at(vars(input$rtrajcolor), ~ paste0(input$rtrajcolor, "_s"))
      }
      
      
      
      else if (input$rtrajradio %in% "On"){
        stratify_vars2.4$df %>% 
          mutate_at(input$rtrajcolor, ~my_ntiles(.x, input$rtrajntile)) %>%
          rename_at(vars(input$rtrajcolor), ~ paste0(input$rtrajcolor, "_s"))
        
      }
      
      else if (input$rtrajradio %in% "Off"){
        stratify_vars2.4$df %>% 
          mutate_at(input$rtrajcolor, as.character) %>%
          rename_at(vars(input$rtrajcolor), ~ paste0(input$rtrajcolor, "_s"))
        
      }
    }
    

    
    
  })
  
  
  
  #Random plot
  
  #make plot update after new variables are selected
  
  rtrajvariables <- reactiveValues(var = "ID", color = "weekday")
  
  
  #Default traj variable
  observeEvent(input$go, priority = -1, {
    
    rtrajvariables$var <- names(dataset())[[sample(1:length(names(dataset())), 1)]]
    
  })
  
  rtrajrandbutton <- reactiveValues(r1=NULL)
  
  observeEvent(input$rtrajrandom, priority = 2, ignoreInit = T, {
    
    rtrajrandbutton$r1 <- sample(1:length(names(dataset())), 1)
    
    rtrajrandbutton$r2 <- sample(1:length(varnames2.2$df), 1)
    
    
    if("Main Var." %in% input$rtrajrand_choice) {rtrajvariables$var <- names(dataset())[[rtrajrandbutton$r1]]}
    
    if("Color Var." %in% input$rtrajrand_choice) {rtrajvariables$color <- varnames2.4$df[[rtrajrandbutton$r2]]}
    
  })
  
  
  ##Data for response trajs
  
  rtrajdata <- reactiveValues(l=NULL, m=NULL)
  
  
  observeEvent(input$rtraj1, ignoreInit = T, {
    
    
    if (input$rtrajraw %in% "Raw" | input$rtrajvar %in% "ID") {
      
      rtrajdata$l <- 
        dataset() %>% 
        select_("ID", "timepoint", "weekday", "weekday_n", "weektime_n", "day",  input$rtrajvar) %>%
        left_join(stratify_vars2.4$df2, by="ID") 
      
      
      
    }
    
    else if (input$rtrajraw %in% "Subject Normalized") {
      
      rtrajdata$l <- 
        dataset() %>% 
        select_("ID", "timepoint", "weekday", "weekday_n", "weektime_n", "day",  input$rtrajvar) %>%
        group_by("ID") %>%
        mutate_at(input$rtrajvar, funs(normalize)) %>%
        ungroup() %>%
        left_join(stratify_vars2.4$df2, by="ID") 
      
    }
    
    
  })

  
  #output$test1 <- renderText(names(rtrajdata$m))
  
  
  #dummy variable to control plot, since plot runs automatically when page is selected, will make it dependent on a reactive value that updates 
  #once the "create plot" button is clicked.
  
  rtraj_dummy <- reactiveValues(l=0)
  
  observeEvent(input$rtraj1, ignoreInit = T, {
    if (rtraj_dummy$l==0) {rtraj_dummy$l <- 1}
    else NULL
  })
  
  ##Reset plot when dataset changes
  
  observeEvent(input$go, {
    rtraj_dummy$l <- 0
    
  })
  
  ##Instructions that appear before create plot button is clicked
  
  output$rtraj_instr <-  renderText(
    if(rtraj_dummy$l==0) {"Click the [Create/Update Plot] button to generate plot!"}
    else NULL 
  )
  
  ##code for trajogram
  
  output$trajR <- renderPlotly({ 
    
    input$rtraj1
    
    
    isolate(
      
      if(rtraj_dummy$l==0) NULL
      
      else {
        
        if(input$rtrajtraces %in% "Group Means") {
          
          if(input$rtrajaxis %in% "weektime_n") {
            
            if(varcolor2.4$l %in% c("ID", "None", "weekday")) {
              
              ggplotly(ggplot(rtrajdata$l, aes_string(x=input$rtrajaxis, y=input$rtrajvar, color = "weekday")) +
                         labs(title = input$rtrajvar) +
                         #stat_summary(fun.y = mean,
                         #fun.ymin = function(x) mean(x) - sd(x),
                         #fun.ymax = function(x) mean(x) + sd(x),
                         #geom = "pointrange", size = .3)+
                         stat_summary(fun.data = mean_cl_normal, geom = "pointrange", size = 1, alpha = 0.7)+
                         stat_summary(fun.y = mean,
                                      geom = "line", size = 1, alpha = 0.7) +
                         
                         #scale_x_discrete(limits=c(7:22))+
                         theme_bw(base_size = 14) +
                         theme(panel.grid = element_blank()))
            }
            
            else{
              
              ggplotly(ggplot(rtrajdata$l, aes_string(x=input$rtrajaxis, y=input$rtrajvar, color = varcolor2.4$l)) +
                         labs(title = input$rtrajvar) +
                         #stat_summary(fun.y = mean,
                         #fun.ymin = function(x) mean(x) - sd(x),
                         #fun.ymax = function(x) mean(x) + sd(x),
                         #geom = "pointrange", size = .3)+
                         stat_summary(fun.data = mean_cl_normal, geom = "pointrange", size = 1, alpha = 0.7)+
                         stat_summary(fun.y = mean,
                                      geom = "line", size = 1,  alpha = 0.7, mapping = aes_string(linetype = "weekday")) +
                         
                         #scale_x_discrete(limits=c(7:22))+
                         theme_bw(base_size = 14) +
                         theme(panel.grid = element_blank()))
              
              
            }
          }
          
          else {
            
            if(varcolor2.4$l %in% c("ID", "None")) {
              
              ggplotly(ggplot(rtrajdata$l, aes_string(x=input$rtrajaxis, y=input$rtrajvar)) +
                         labs(title = input$rtrajvar) +
                         #stat_summary(fun.y = mean,
                         #fun.ymin = function(x) mean(x) - sd(x),
                         #fun.ymax = function(x) mean(x) + sd(x),
                         #geom = "pointrange", size = .3)+
                         stat_summary(fun.data = mean_cl_normal, geom = "pointrange", size = 1, alpha = 0.7)+
                         stat_summary(fun.y = mean,
                                      geom = "line", size = 1, alpha = 0.7) +
                         
                         #scale_x_discrete(limits=c(7:22))+
                         theme_bw(base_size = 14) +
                         theme(panel.grid = element_blank()))
            }
            
            else{
              
              ggplotly(ggplot(rtrajdata$l, aes_string(x=input$rtrajaxis, y=input$rtrajvar, color = varcolor2.4$l)) +
                         labs(title = input$rtrajvar) +
                         #stat_summary(fun.y = mean,
                         #fun.ymin = function(x) mean(x) - sd(x),
                         #fun.ymax = function(x) mean(x) + sd(x),
                         #geom = "pointrange", size = .3)+
                         stat_summary(fun.data = mean_cl_normal, geom = "pointrange", size = 1, alpha = 0.7)+
                         stat_summary(fun.y = mean,
                                      geom = "line", size = 1, alpha = 0.7) +
                         
                         #scale_x_discrete(limits=c(7:22))+
                         theme_bw(base_size = 14) +
                         theme(panel.grid = element_blank()))
              
            }
          }
        }
        
        else if (input$rtrajtraces %in% "Subject Traces") {
          
          if(varcolor2.4$l %in% c("ID", "None")) {
            
            ggplotly(ggplot(rtrajdata$l, aes_string(x=input$rtrajaxis, y=input$rtrajvar, group = "ID")) +
                       labs(title = input$rtrajvar) +
                       #stat_summary(fun.y = mean,
                       #fun.ymin = function(x) mean(x) - sd(x),
                       #fun.ymax = function(x) mean(x) + sd(x),
                       stat_summary(fun.y = mean,
                                    geom = "line", size = 0.5, alpha = 0.2) +
                       
                       #scale_x_discrete(limits=c(7:22))+
                       theme_bw(base_size = 14) +
                       theme(panel.grid = element_blank()))
          }
          
          else{
            
            ggplotly(ggplot(rtrajdata$l, aes_string(x=input$rtrajaxis, y=input$rtrajvar, group = "ID", color = varcolor2.4$l)) +
                       labs(title = input$rtrajvar) +
                       #stat_summary(fun.y = mean,
                       #fun.ymin = function(x) mean(x) - sd(x),
                       #fun.ymax = function(x) mean(x) + sd(x),
                       stat_summary(fun.y = mean,
                                    geom = "line", size = 0.5,  alpha = 0.2) +
                       
                       #scale_x_discrete(limits=c(7:22))+
                       theme_bw(base_size = 14) +
                       theme(panel.grid = element_blank()))
            
            
          }
        }
      }
      
    )
    
  })
  
  
  #Response Page 5: Scatterplot-----------------------------------------------------------------------------------------------------
  
  ##data table for scatterplot
  output$table7 <- DT::renderDataTable(data(), selection = list(selected = 8, mode = 'single'),
                                       options = list(columnDefs = list(list(
                                         targets = 1,
                                         render = JS(
                                           "function(data, type, row, meta) {",
                                           "return type === 'display' && data.length > 12 ?",
                                           "'<span title=\"' + data + '\">' + data.substr(0, 12) + '...</span>' : data;",
                                           "}")
                                       ))), callback = JS('table.page(3).draw(false);'))
  
  ##Select y axis variable:
  output$scatterplot_y_var <- renderUI({selectInput('scatterplot_y_var', 'Y-Axis Variable:', c(names(dataset())), selected = scatterplotvariables$y_var, selectize=TRUE)})
  
  ##Select x axis variable
  output$scatterplot_x_var <- renderUI({selectInput('scatterplot_x_var', 'X-Axis Variable:', c(names(dataset())), selected = scatterplotvariables$x_var, selectize=TRUE)})
  
  ##Color/interaction datasets
  stratify_vars2.5 <- reactiveValues(df = NULL, df2 = NULL)
  
  ##Using varnames so updating stratify_vars2.5$df2 doesn't refresh subbrowsecolor
  varnames2.5 <- reactiveValues(df=NULL)
  
  ##Default datasets for color/interaction
  observeEvent(input$go, priority = -1, {
    
    stratify_vars2.5$df <- stratify_vars$df_full %>% select_("ID", "timeindex") %>% mutate(dummy_s = 1)
    stratify_vars2.5$df2 <- stratify_vars$df_full %>% select_("ID", "timeindex") %>% mutate(dummy_s = 1)
    
    varnames2.5$df <- names(stratify_vars$df_full)
    
  })
  
  
  ##create main dataset with the appropriate level
  dataset_levels2.5 <- reactiveValues(df = NULL)
  
  observeEvent({input$scatterplot1}, priority = 1, ignoreInit = T,  {
    
    if (input$scatterplotlevel %in% "Subject") {
      
      dataset_levels2.5$df <- dataset() %>% 
        select_("ID", input$scatterplot_y_var, input$scatterplot_x_var) %>% mutate(dummy=1) %>%
        group_by(ID) %>%
        summarise_all(function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                ifelse(is.numeric(x), mean(x, na.rm = T), NA))
        return(y)}) %>% 
        ungroup()
      
      
    }
    
    
    else if (input$scatterplotlevel %in% "Day") {
      
      dataset_levels2.5$df <- dataset() %>% 
        select_("ID", "day", input$scatterplot_y_var, input$scatterplot_x_var) %>% mutate(dummy=1) %>%
        group_by(ID, day) %>%
        summarise_all(function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                ifelse(is.numeric(x), mean(x, na.rm = T), NA))
        return(y)}) %>% 
        ungroup()
      
      
    }
    
    else if (input$subbrowselevel %in% "Assessment") {
      
      dataset_levels2.5$df <- dataset() %>% select_("ID", "timeindex", "day", input$scatterplot_y_var, input$scatterplot_x_var) %>% mutate(dummy=1)
    }
  })
  
  
  ##Level select for color variable, choices depend on level of main dataset
  
  output$scatterplotcolorlevel <- renderUI({
    
    if (input$scatterplotlevel %in% "Assessment") {
      radioButtons("scatterplotcolorlevel", "Color Variable Level:", c("Subject", "Day", "Assessment"), selected = "Subject", inline = T)
    }
    
    else if (input$scatterplotlevel %in% "Day") {
      radioButtons("scatterplotcolorlevel", "Color Variable Level:", c("Subject", "Day"), selected = "Subject", inline = T)
    }
    
    else if (input$scatterplotlevel %in% "Subject") {
      radioButtons("scatterplotcolorlevel", "Color Variable Level:", c("Subject"), selected = "Subject", inline = T)
    }
    
  })
  
  ##Data type select
  
  
  output$scatterplotraw <- renderUI({
    
    if (input$scatterplotlevel %in% "Subject") {
      radioButtons("scatterplotraw", 
                   label = "Main Variables Type:", 
                   choices = c("Raw"),
                   selected = "Raw", inline = T)
    }
    
    else {
      radioButtons("scatterplotraw", 
                   label = "Main Variables Type:", 
                   choices = c("Raw", "Subject Normalized"),
                   selected = "Subject Normalized", inline = T)
      
    }
    
  })
  
  
  ##Key variable for each combination of raw/normalized and assessment/day/subject for coloring variable
  colorvarkey2.5 <- reactive({paste0(input$scatterplotcolortype, "-", input$scatterplotcolorlevel)})
  
  ##create the appropriate stratify_vars2.5$df , default color/interaction dataset
  observeEvent({input$scatterplot1}, priority = 1, ignoreInit = T, {
    
    if(!input$scatterplotcolor %in% c("ID", "None")) {
      
      if (colorvarkey2.5() %in% "Raw-Subject") {
        stratify_vars2.5$df <- stratify_vars$df_full %>%
          group_by(ID) %>%
          summarise_at(input$scatterplotcolor, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                         ifelse(is.numeric(x), mean(x, na.rm = T), NA))
          return(y)}) %>% 
          ungroup()
        
      }
      
      else if (colorvarkey2.5() %in% "Raw-Day") {
        stratify_vars2.5$df <- stratify_vars$df_full %>%
          group_by(ID, day) %>%
          summarise_at(input$scatterplotcolor, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                         ifelse(is.numeric(x), mean(x, na.rm = T), NA))
          return(y)}) %>% 
          ungroup() 
        
      }
      
      else if (colorvarkey2.5() %in% "Raw-Assessment") {
        stratify_vars2.5$df <- stratify_vars$df_full %>% select_("ID", "timeindex", input$scatterplotcolor)
      }
      
      else if (colorvarkey2.5() %in% "Subject Normalized-Subject") {
        stratify_vars2.5$df <-  stratify_vars$df_full %>%
          group_by(ID) %>%
          summarise_at(input$scatterplotcolor, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                         ifelse(is.numeric(x), mean(x, na.rm = T), NA))
          return(y)}) %>% 
          ungroup() 
        
      }
      
      else if (colorvarkey2.5() %in% "Subject Normalized-Day") {
        stratify_vars2.5$df <- stratify_vars$df_full %>%
          group_by(ID, day) %>%
          summarise_at(input$scatterplotcolor, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                         ifelse(is.numeric(x), mean(x, na.rm = T), NA))
          return(y)}) %>% 
          ungroup() %>% group_by(ID) %>%
          mutate_at(input$scatterplotcolor, funs(normalize)) %>%
          ungroup()
      }
      
      else if (colorvarkey2.5() %in% "Subject Normalized-Assessment") {
        stratify_vars2.5$df <- stratify_vars$df_full %>% select_("ID", "timeindex", input$scatterplotcolor) %>%
          group_by(ID) %>%
          mutate_at(input$scatterplotcolor, funs(normalize)) %>%
          ungroup()
      }
      
    }
    
    else {
      
      if (colorvarkey2.5() %in% c("Raw-Assessment", "Subject Normalized-Assessment")) {
        
        stratify_vars2.5$df <- stratify_vars$df_full %>% select_("ID", "timeindex") %>% mutate(dummy_s=1)}
      
      else if (colorvarkey2.5() %in% c("Raw-Day", "Subject Normalized-Day")) {
        
        stratify_vars2.5$df <- stratify_vars$df_full %>% mutate(dummy=1) %>% group_by(ID, day) %>% summarise(dummy_s=mean(dummy))}
      
      else if (colorvarkey2.5() %in% c("Raw-Subject", "Subject Normalized-Subject")) {
        
        stratify_vars2.5$df <- stratify_vars$df_full %>% mutate(dummy=1) %>% group_by(ID) %>% summarise(dummy_s=mean(dummy))}
      
    }
    
  })
  
  ##Select coloring variable
  output$scatterplotcolor <- renderUI({selectInput('scatterplotcolor', 'Color By:', c("None", varnames2.5$df), selected=scatterplotvariables$color, selectize=TRUE)})
  
  ###varcolor2.5$l is used so selecting color input doesn't automatically update plot
  varcolor2.5 <- reactiveValues(l="None")
  
  observeEvent({input$scatterplotcolor}, ignoreInit = T, { if(input$scatterplotcolor %in% c("ID", "None")) {varcolor2.5$l <- input$scatterplotcolor}
    else {varcolor2.5$l <- paste0(input$scatterplotcolor, "_s")} 
  })
  
  ##Color variable dataset join vars, depending on level and type
  df2_join_vars2.5 <- reactive({ if (input$scatterplotcolorlevel %in% "Subject") {c("ID")}
    else if (input$scatterplotcolorlevel %in% "Day") {c("ID", "day")}
    else if (input$scatterplotcolorlevel %in% "Assessment") {c("ID", "timeindex")}
  })
  
  
  ##Color vars dataset, upon action button, stratify_vars2.5$df2 will update based on variable and quantile selection
  
  observeEvent(input$scatterplot1, priority = 1, ignoreInit = T, {
    
    stratify_vars2.5$df2 <-  
      
      if(input$scatterplotcolor %in% c("ID", "None")) {stratify_vars2.5$df}
    
    else {
      if(input$scatterplotradio %in% "Auto"){
        stratify_vars2.5$df %>% select_(df2_join_vars2.5()[1], df2_join_vars2.5()[length(df2_join_vars2.5())], input$scatterplotcolor) %>%
          mutate_at(input$scatterplotcolor, function (x) { if (is.character(x) | is.factor(x)) {x}
            else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) > 20) {my_ntiles(x, input$scatterplotntile)}
            else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) <= 20) {factor(x, ordered = T, exclude = c(NA, "NaN"))}
            else {x=NA}
          }) %>% 
          rename_at(vars(input$scatterplotcolor), ~ paste0(input$scatterplotcolor, "_s"))
      }
      
      else if (input$scatterplotradio %in% "On"){
        stratify_vars2.5$df %>% select_(df2_join_vars2.5()[1], df2_join_vars2.5()[length(df2_join_vars2.5())], input$scatterplotcolor) %>%
          mutate_at(input$scatterplotcolor, ~my_ntiles(.x, input$scatterplotntile)) %>%
          rename_at(vars(input$scatterplotcolor), ~ paste0(input$scatterplotcolor, "_s"))
        
      }
      
      else if (input$scatterplotradio %in% "Off"){
        stratify_vars2.5$df %>% 
          select_(df2_join_vars2.5()[1], df2_join_vars2.5()[length(df2_join_vars2.5())], input$scatterplotcolor) %>%
          rename_at(vars(input$scatterplotcolor), ~ paste0(input$scatterplotcolor, "_s"))
        
      }
    }
    
    
  })
  
  #Random plot
  
  #make plot update after new variables are selected
  
  scatterplotvariables <- reactiveValues(x_var = "ID", y_var = "ID", color = "None")
  
  #default scatterplot vars
  observeEvent(input$go, priority = -1, {
    
    scatterplotvariables$x_var <- names(dataset())[[sample(1:length(names(dataset())), 1)]]
    
    scatterplotvariables$y_var <- names(dataset())[[sample(1:length(names(dataset())), 1)]]
    
  })
  
  scatterplotrandbutton <- reactiveValues(r1=NULL, r2=NULL, r3=NULL)
  
  observeEvent(input$scatterplotrandom, priority = 2, ignoreInit = T, {
    
    scatterplotrandbutton$r1 <- sample(1:length(names(dataset())), 1)
    
    scatterplotrandbutton$r2 <- sample(1:length(names(dataset())), 1)
    
    scatterplotrandbutton$r3 <- sample(1:length(varnames2.5$df), 1)
    
    
    if("X-Axis Var." %in% input$scatterplotrand_choice) {scatterplotvariables$x_var <- names(dataset())[[scatterplotrandbutton$r1]]}
    
    if("Y-Axis Var." %in% input$scatterplotrand_choice) {scatterplotvariables$y_var <- varnames2.5$df[[scatterplotrandbutton$r2]]}
    
    if ("Color Var." %in% input$scatterplotrand_choice) {scatterplotvariables$color <- varnames2.5$df[[scatterplotrandbutton$r3]]}
    
    
  })
  
  ##datasets for scatterplot
  
  scatterplotdata <- reactiveValues(l=NULL)
  
  observeEvent(input$scatterplot1, ignoreInit = T, {
    
    scatterplotdata$l <- 
      
      if (input$scatterplotraw %in% "Raw" | input$scatterplot_y_var %in% "ID" | input$scatterplot_x_var %in% "ID" ) {
        
        dataset_levels2.5$df %>% 
          left_join(stratify_vars2.5$df2, by=c(df2_join_vars2.5()))
      }
    
    else if (input$scatterplotraw %in% "Subject Normalized") {
      
      dataset_levels2.5$df %>% 
        group_by_("ID") %>%
        mutate_at(c(input$scatterplot_x_var, input$scatterplot_y_var), funs(normalize)) %>%
        ungroup() %>%
        left_join(stratify_vars2.5$df2, by=c(df2_join_vars2.5())) 
      
    }
    
  })
  
  
  #dummy variable to control plot, since plot runs automatically when page is selected, will make it dependent on a reactive value that updates 
  #once the "create plot" button is clicked.
  
  scatterplot_dummy <- reactiveValues(l=0)
  
  observeEvent(input$scatterplot1, priority = 0, ignoreInit = T, {
    if (scatterplot_dummy$l==0) {scatterplot_dummy$l <- 1}
    else NULL
  })
  
  
  ##Reset plot when dataset changes
  
  observeEvent(input$go, {
    scatterplot_dummy$l <- 0
    
  })
  
  
  
  ##Instructions that appear before create plot button is clicked
  
  output$scatterplot_instr <-  renderText(
    if(scatterplot_dummy$l==0) {"Click the [Create/Update Plot] button to generate plot!"}
    else NULL 
  )
  
  
  ##Scatterplot 
  
  output$scatterplot <- renderPlotly({
    
    input$scatterplot1
    
    isolate(
      
      if(scatterplot_dummy$l==0) NULL
      
      else{
        
        if (varcolor2.5$l %in% "None") {
          
          ggplotly(ggplot(scatterplotdata$l, aes_string(x=input$scatterplot_x_var, y=input$scatterplot_y_var, label = "ID")) +
                     geom_point() +
                     geom_smooth(method='lm', formula=y~x)+
                     theme_bw(),  height=800, width=1000) 
          
        }
        
        else {
          
          ggplotly(ggplot(scatterplotdata$l, aes_string(x=input$scatterplot_x_var, y=input$scatterplot_y_var, label = "ID", color = varcolor2.5$l)) +
                     geom_point() +
                     geom_smooth(method='lm', formula=y~x)+
                     theme_bw(), height=800, width=1000)
          
        }
      }
    )
  })

  #Subject Dashboard Page 1: Mean Trajetory Browse------------------------------------------------------------------------------------------------------------------------------

  ##Select main y axis variable:
  output$meanbrowsevar <- renderUI({selectInput('meanbrowsevar', 'Main Variable:', c(names(dataset())), selected = meanbrowsevariables$var, selectize=TRUE)})
  
  ##Color and order datasets
  stratify_vars3.1 <- reactiveValues(df = NULL, df2 = NULL, df3 = NULL)
  
  ##Using varnames so updating stratify_vars3.1$df2 doesn't refresh subbrowsecolor
  varnames3.1 <- reactiveValues(df=NULL)
  
  ##Default datasets
  observeEvent(input$go, priority = -1, {
    
    stratify_vars3.1$df <- stratify_vars$df_sub %>% select("ID") %>% mutate(dummy_s=1)
    stratify_vars3.1$df2 <- stratify_vars3.1$df
    stratify_vars3.1$df3 <- stratify_vars$df_sub %>% select("ID") %>% mutate(dummy_o=1)
    
    varnames3.1$df <- names(stratify_vars$df_full)
    
    
  })
  
  ##Select coloring variable (subject level, can bin into quantiles)
  output$meanbrowsecolor <- renderUI({selectInput('meanbrowsecolor', 'Color By:', varnames3.1$df, selected = meanbrowsevariables$color, selectize=TRUE)})
  
  ###varcolor3.1$l is used so selecting color input doesn't automatically update plot
  varcolor3.1 <- reactiveValues(l="ID")
  
  observeEvent(input$meanbrowse1, {if (input$meanbrowsecolor %in% "ID") {varcolor3.1$l <- input$meanbrowsecolor}
    else {varcolor3.1$l <- paste0(input$meanbrowsecolor, "_s")} 
  })
  
  
  ##create the appropriate stratify_vars3.1$df 
  observeEvent(input$meanbrowse1, {
        
     if(!input$meanbrowsecolor %in% "ID") {
        stratify_vars3.1$df <- stratify_vars$df_full %>% select_("ID", input$meanbrowsecolor) %>%
          group_by(ID) %>%
          summarise_at(input$meanbrowsecolor, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                       ifelse(is.numeric(x), mean(x, na.rm = T), NA))
          return(y)}) %>% 
          ungroup() 
     }
    
     else {stratify_vars3.1$df <- stratify_vars$df_sub %>% select_("ID") %>% mutate(dummy_s = 1)}
    
  })
    
  ##Color vars dataset, upon action button, stratify_vars3.1$df2 will update based on variable and quantile selection
  observeEvent(input$meanbrowse1, ignoreInit = T, {
    
    stratify_vars3.1$df2 <-  
      
    if(input$meanbrowsecolor %in% "ID") {stratify_vars3.1$df}
    
    else {
      if(input$meanbrowseradio %in% "Auto"){
        stratify_vars3.1$df %>% 
          mutate_at(input$meanbrowsecolor, function (x) { if (is.character(x) | is.factor(x)) {x}
            else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) > 20) {my_ntiles(x, input$meanbrowsentile)}
            else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) <= 20) {factor(x, ordered = T, exclude = c(NA, "NaN"))}
            else {x=NA}
          }) %>% 
          rename_at(vars(input$meanbrowsecolor), ~ paste0(input$meanbrowsecolor, "_s"))
      }
      
      else if (input$meanbrowseradio %in% "On"){
        stratify_vars3.1$df %>% 
          mutate_at(input$meanbrowsecolor, ~my_ntiles(.x, input$meanbrowsentile)) %>%
          rename_at(vars(input$meanbrowsecolor), ~ paste0(input$meanbrowsecolor, "_s"))
        
      }
      
      else if (input$meanbrowseradio %in% "Off"){
        stratify_vars3.1$df %>% 
          rename_at(vars(input$meanbrowsecolor), ~ paste0(input$meanbrowsecolor, "_s"))
        
      }
    }
    
  })
  
  ##Select subject ordering variable 
  output$meanbrowseorder <- renderUI({selectInput('meanbrowseorder', 'Order Subjects By:', varnames3.1$df, selected = meanbrowsevariables$order, selectize=TRUE)})

  
  ##Ordered list of subject ID
  subjectorder3.1 <- reactiveValues(l=NULL, m=NULL)
  
  ##Set default value ordered list of subject ID
  observeEvent(input$go, {
    subjectorder3.1$l=as.character(stratify_vars$df_sub[["ID"]])
  })
  
  
  
  ##Ordering vars dataset, upon action button, stratify_vars3.1$df3 will update based on variable, ordered vector of subjectIDs will be created
  observeEvent(input$meanbrowse1, {
    stratify_vars3.1$df3 <-  
      if (!input$meanbrowseorder %in% "ID"){
        stratify_vars$df_full %>% 
          group_by(ID) %>%
          summarise_at(input$meanbrowseorder, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                       ifelse(is.numeric(x), mean(x, na.rm = T), "Error"))
          return(y)}) %>% 
          ungroup() %>%
          arrange_(input$meanbrowseorder) %>%
          rename_at(vars(input$meanbrowseorder), ~ paste0(input$meanbrowseorder, "_o")) %>%
          mutate(ID_val = paste0(ID, " | ", input$meanbrowseorder, "=", ordershow(.data[[paste0(input$meanbrowseorder, "_o")]])))
      }
    
      else {
        stratify_vars3.1$df3 %>% 
        select(1) %>% 
        arrange_("ID") %>%
        mutate(ID_val = ID)
    }
    
    #mutate(ID_val = paste0(ID, " Order Var: ", ordershow(.data[[paste0(input$meanbrowseorder, "_o")]])))
    #mutate(ID_val = ID)
    
    subjectorder3.1$l <- stratify_vars3.1$df3[["ID"]]
    subjectorder3.1$m <- stratify_vars3.1$df3[["ID_val"]]
    
    
  })
  
  ##Page Selector
  
  pagenum3.1 <- reactiveValues(l=1)
  
  output$meanbrowsepage <- renderUI({selectInput('meanbrowsepage', 'Go To Page:', c(1:ceiling(nrow(stratify_vars$df_sub)/16)), selectize=TRUE)})
  
  observeEvent(input$meanbrowsepage, {pagenum3.1$l <- as.numeric(input$meanbrowsepage)})
  
  observeEvent(input$meanbrowseprev, {
    if (pagenum3.1$l > 1) {pagenum3.1$l <- pagenum3.1$l - 1}
  })
  
  observeEvent(input$meanbrowsenext, {
    if (pagenum3.1$l < ceiling(nrow(stratify_vars$df_sub)/16)) {pagenum3.1$l <- pagenum3.1$l + 1}
  })
  
  ##Pull increments of 16 subjects based on the value of page selector, the IDs are stored as subjectorder16
  subjectorder16_3.1 <- reactive({ 
    #when page number is max:
    if (pagenum3.1$l==ceiling(nrow(stratify_vars$df_sub)/16)) {subjectorder3.1$l[((as.numeric(pagenum3.1$l) - 1)*16 + 1) : length(subjectorder3.1$l)]}
    #otherwise: ie 1-16, 17-32, and so on
    else {subjectorder3.1$l[((pagenum3.1$l - 1)*16 + 1) : (pagenum3.1$l*16)]}
    
  })
  
  subjectorder16_3.1_val <- reactive({ 
    #when page number is max:
    if (pagenum3.1$l==ceiling(nrow(stratify_vars$df_sub)/16)) {subjectorder3.1$m[((as.numeric(pagenum3.1$l) - 1)*16 + 1) : length(subjectorder3.1$m)]}
    #otherwise: ie 1-16, 17-32, and so on
    else {subjectorder3.1$m[((pagenum3.1$l - 1)*16 + 1) : (pagenum3.1$l*16)]}
    
  }) 
  
  ##display page
  output$pagenum3.1_display <- renderText(print(pagenum3.1$l))
  
  #reset page number on new plot
  
  observeEvent(input$meanbrowse1, {pagenum3.1$l <- 1})
  
  
  #Random plot
  
  #make plot update after new variables are selected
  #subbrowserandomcheck <- reactiveValues(l=0, m=0)
  
  
  meanbrowsevariables <- reactiveValues(var = "ID", color = "ID", order = "ID")
  
  #Default random var
  observeEvent(input$go, priority = -1, {
    
    meanbrowsevariables$var <- names(dataset())[[sample(1:length(names(dataset())), 1)]]
    
  })
  
  meanbrowserandbutton <- reactiveValues(r1=NULL, r2=NULL, r3=NULL)
  
  observeEvent(input$meanbrowserandom, priority = 2, ignoreInit = T, {
    
    meanbrowserandbutton$r1 <- sample(1:length(names(dataset())), 1)
    
    meanbrowserandbutton$r2 <- sample(1:length(varnames3.1$df), 1)
    
    meanbrowserandbutton$r3 <- sample(1:length(varnames3.1$df), 1)
    
    
    if("Main Var." %in% input$meanbrowserand_choice) {meanbrowsevariables$var <- names(dataset())[[meanbrowserandbutton$r1]]}
    
    if("Color Var." %in% input$meanbrowserand_choice) {meanbrowsevariables$color <- varnames3.1$df[[meanbrowserandbutton$r2]]}
    
    if ("Order Var." %in% input$meanbrowserand_choice) {meanbrowsevariables$order <- varnames3.1$df[[meanbrowserandbutton$r3]]}
    
    
  })
  
  
  ##Create dataset for mean browser plot, 16 subjects per page
  meanbrowsedata <- reactiveValues(l=NULL)
  
  observeEvent( c(input$meanbrowse1, pagenum3.1$l), ignoreInit = T, {
    
    meanbrowsedata$l <- 
    
    if (input$meanbrowseraw %in% "Raw" | input$meanbrowsevar %in% "ID") {
      
      dataset() %>% 
        select_("ID", "timepoint", "weekday_n", input$meanbrowsevar) %>%
        left_join(stratify_vars3.1$df2, by="ID") %>%
        left_join(stratify_vars3.1$df3, by="ID") %>%
        filter(ID %in% subjectorder16_3.1()) %>%
        mutate_at("ID_val", ~factor(., levels = subjectorder16_3.1_val()))
    }
    
    else if (input$meanbrowseraw %in% "Subject Normalized") {
      
      dataset() %>% 
        select_("ID", "timepoint", "weekday_n", input$meanbrowsevar) %>%
        group_by_("ID") %>%
        mutate_at(input$meanbrowsevar, funs(normalize)) %>%
        ungroup() %>%
        left_join(stratify_vars3.1$df2, by="ID") %>%
        left_join(stratify_vars3.1$df3, by="ID") %>%
        filter(ID %in% subjectorder16_3.1()) %>%
        mutate_at("ID_val", ~factor(., levels = subjectorder16_3.1_val()))
      
    }
    
  })
  
  

  #dummy variable to control plot, since plot runs automatically when page is selected, will make it dependent on a reactive value that updates 
  #once the "create plot" button is clicked.
  
  meanbrowseplot_dummy <- reactiveValues(l=0)
  
  observeEvent(input$meanbrowse1, ignoreInit = T, {
    if (meanbrowseplot_dummy$l==0) {meanbrowseplot_dummy$l <- 1}
    else NULL
  })
  
  ##Reset plot when dataset changes
  
  observeEvent(input$go, {
    meanbrowseplot_dummy$l <- 0
    
  })
  
  ##Instructions that appear before create plot button is clicked
  
  output$meanbrowseplot_instr <-  renderText(
    if(meanbrowseplot_dummy$l==0) {"Click the [Create/Update Plot] button to generate plot!"}
    else NULL 
  )
  
  ##Subject Browser plot, view 16 subject level trends at once  
  
  output$meanbrowseplot <- renderPlot({
    
    input$meanbrowse1
    pagenum3.1$l
    
    isolate(
    
    if(meanbrowseplot_dummy$l==0) NULL
    
    else{
          
          if (is.numeric(meanbrowsedata$l[[varcolor3.1$l]]) | is.integer(meanbrowsedata$l[[varcolor3.1$l]])) {
            
            ggplot(data = meanbrowsedata$l, aes_string(x = input$meanbrowseaxis , y = input$meanbrowsevar, color = varcolor3.1$l)) + 
              stat_summary(fun.y = mean, geom = "line", size = 1.5) +
              stat_summary(fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult=1), size = 1)+
              scale_color_distiller(palette = "Spectral") +
              facet_wrap(~ID_val, nrow=4) +
              theme_bw(base_size = 18) 
            
          }
          
          else {

            ggplot(data = meanbrowsedata$l, aes_string(x = input$meanbrowseaxis , y = input$meanbrowsevar, color = varcolor3.1$l)) + 
              stat_summary(fun.y = mean, geom = "line", size = 1.5) +
              stat_summary(fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult=1), size = 1)+
              facet_wrap(~ID_val, nrow=4) +
              theme_bw(base_size = 18) 
           #theme(legend.position = "none")
          }
        
    }
    
    )
    
  })
  
  
  
  #Subject Dashboard Page 2: Subject Trajetory Browse------------------------------------------------------------------------------------------------------------------------------
  
  ##Select main y axis variable:
  output$subbrowsevar <- renderUI({selectInput('subbrowsevar', 'Main Variable:', c(names(dataset())), selected = subbrowsevariables$var, selectize=TRUE)})
  
  ##Color and order datasets:
  stratify_vars3.2 <- reactiveValues(df = NULL, df2 = NULL, df3 = NULL)
  
  ##Default datasets for color and order
  observeEvent(input$go, priority = -1, {
    
    stratify_vars3.2$df <- stratify_vars$df_full %>% select_("ID", "timeindex") %>% mutate(dummy_s = 1)
    stratify_vars3.2$df2 <- stratify_vars$df_full %>% select_("ID", "weekday") %>% rename(weekday_s = weekday)
    stratify_vars3.2$df3 <- stratify_vars$df_sub %>% select("ID") %>% mutate(dummy_o=1)
    
    varnames3.2$df <- names(stratify_vars$df_full)
    
    
  })
  
  
  ##create main dataset with the appropriate level
  dataset_levels <- reactiveValues(df = NULL)
  
  observeEvent({input$subbrowse1}, priority = 1, ignoreInit = T,  {
      
      if (input$subbrowselevel %in% "Day") {
        
        dataset_levels$df <- dataset() %>% 
          select_("ID", "day", input$subbrowsevar) %>% mutate(dummy=1) %>%
          group_by(ID, day) %>%
          summarise_all(function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                  ifelse(is.numeric(x), mean(x, na.rm = T), NA))
          return(y)}) %>% 
          ungroup()
        
        
      }
      
      else if (input$subbrowselevel %in% "Assessment") {
        
        dataset_levels$df <- dataset() %>% select_("ID", "timeindex", "day", input$subbrowsevar) %>% mutate(dummy=1)
        
        
        
      }
    })
  
  
  ##Using varnames so updating stratify_vars3.2$df2 doesn't refresh subbrowsecolor
  varnames3.2 <- reactiveValues(df=NULL)
  
  ##Level select for color variable, choices depend on the level of main dataset:
  
  output$subbrowsecolorlevel <- renderUI({
    
    if (input$subbrowselevel %in% "Assessment") {
      radioButtons("subbrowsecolorlevel", "Color Variable Level:", c("Subject", "Day", "Assessment"), selected = "Assessment", inline = T)
    }
    
    else if (input$subbrowselevel %in% "Day") {
      radioButtons("subbrowsecolorlevel", "Color Variable Level:", c("Subject", "Day"), selected = "Day", inline = T)
    }
    
  })

  
  ##Key variable for each combination of raw/normalized and assessment/day/suject for coloring variable
  colorvarkey3.2 <- reactive({paste0(input$subbrowsecolortype, "-", input$subbrowsecolorlevel)})
  
  ##create the appropriate stratify_vars3.2$df 
  observeEvent({input$subbrowse1}, priority = 1, ignoreInit = T, {
      
      if(!input$subbrowsecolor %in% "ID") {
        
        if (colorvarkey3.2() %in% "Raw-Subject") {
          stratify_vars3.2$df <- stratify_vars$df_full %>%
            group_by(ID) %>%
            summarise_at(input$subbrowsecolor, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                         ifelse(is.numeric(x), mean(x, na.rm = T), NA))
            return(y)}) %>% 
            ungroup()
          
        }
        
        else if (colorvarkey3.2() %in% "Raw-Day") {
          stratify_vars3.2$df <- stratify_vars$df_full %>%
            group_by(ID, day) %>%
            summarise_at(input$subbrowsecolor, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                         ifelse(is.numeric(x), mean(x, na.rm = T), NA))
            return(y)}) %>% 
            ungroup() 
          
        }
        
        else if (colorvarkey3.2() %in% "Raw-Assessment") {
          stratify_vars3.2$df <- stratify_vars$df_full %>% select_("ID", "timeindex", input$subbrowsecolor)
        }
        
        else if (colorvarkey3.2() %in% "Subject Normalized-Subject") {
          stratify_vars3.2$df <-  stratify_vars$df_full %>%
            group_by(ID) %>%
            summarise_at(input$subbrowsecolor, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                         ifelse(is.numeric(x), mean(x, na.rm = T), NA))
            return(y)}) %>% 
            ungroup() 
          
        }
        
        else if (colorvarkey3.2() %in% "Subject Normalized-Day") {
          stratify_vars3.2$df <- stratify_vars$df_full %>%
            group_by(ID, day) %>%
            summarise_at(input$subbrowsecolor, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                         ifelse(is.numeric(x), mean(x, na.rm = T), NA))
            return(y)}) %>% 
            ungroup() %>% group_by(ID) %>%
            mutate_at(input$subbrowsecolor, funs(normalize)) %>%
            ungroup()
        }
        
        else if (colorvarkey3.2() %in% "Subject Normalized-Assessment") {
          stratify_vars3.2$df <- stratify_vars$df_full %>% select_("ID", "timeindex", input$subbrowsecolor) %>%
            group_by(ID) %>%
            mutate_at(input$subbrowsecolor, funs(normalize)) %>%
            ungroup()
        }
        
      }
      
      else {
        
        if (colorvarkey3.2() %in% c("Raw-Assessment", "Subject Normalized-Assessment")) {
          
          stratify_vars3.2$df <- stratify_vars$df_full %>% select_("ID", "timeindex") %>% mutate(dummy_s=1)}
        
        else if (colorvarkey3.2() %in% c("Raw-Day", "Subject Normalized-Day")) {
          
          stratify_vars3.2$df <- stratify_vars$df_full %>% mutate(dummy=1) %>% group_by(ID, day) %>% summarise(dummy_s=mean(dummy))}
        
        else if (colorvarkey3.2() %in% c("Raw-Subject", "Subject Normalized-Subject")) {
          
          stratify_vars3.2$df <- stratify_vars$df_full %>% mutate(dummy=1) %>% group_by(ID) %>% summarise(dummy_s=mean(dummy))}
        
      }
      
    })
  
  
  ##Select coloring variable (subject level, can bin into quantiles)
  output$subbrowsecolor <- renderUI({selectInput('subbrowsecolor', 'Color By:', varnames3.2$df[!varnames3.2$df %in% "timeindex"], selected=subbrowsevariables$color, selectize=TRUE)})
  
  ###varcolor3.2$l is used so selecting color input doesn't automatically update plot
  varcolor3.2 <- reactiveValues(l="weekday")
  
  observeEvent({input$subbrowsecolor}, ignoreInit = T, { if(input$subbrowsecolor %in% "ID") {varcolor3.2$l <- input$subbrowsecolor}
      else {varcolor3.2$l <- paste0(input$subbrowsecolor, "_s")} 
    })
  
  #output$test1 <- renderText(names(subbrowsedata$l))
  
  #output$test2 <- renderText(input$subbrowsecolor)
  
  ##Color variable dataset join vars, depending on level and type
  df2_join_vars <- reactive({ if (input$subbrowsecolorlevel %in% "Subject") {c("ID")}
    else if (input$subbrowsecolorlevel %in% "Day") {c("ID", "day")}
    else if (input$subbrowsecolorlevel %in% "Assessment") {c("ID", "timeindex")}
  })
  
  ##Color vars dataset, upon action button, stratify_vars3.2$df2 will update based on variable and quantile selection
  
  observeEvent(input$subbrowse1, priority = 1, ignoreInit = T, {
      
      stratify_vars3.2$df2 <-  
        
        if(input$subbrowsecolor %in% "ID") {stratify_vars3.2$df}
      
      else {
        if(input$subbrowseradio %in% "Auto"){
          stratify_vars3.2$df %>% select_(df2_join_vars()[1], df2_join_vars()[length(df2_join_vars())], input$subbrowsecolor) %>%
            mutate_at(input$subbrowsecolor, function (x) { if (is.character(x) | is.factor(x)) {x}
              else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) > 20) {my_ntiles(x, input$subbrowsentile)}
              else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) <= 20) {factor(x, ordered = T, exclude = c(NA, "NaN"))}
              else {x=NA}
            }) %>% 
            rename_at(vars(input$subbrowsecolor), ~ paste0(input$subbrowsecolor, "_s"))
        }
        
        else if (input$subbrowseradio %in% "On"){
          stratify_vars3.2$df %>% select_(df2_join_vars()[1], df2_join_vars()[length(df2_join_vars())], input$subbrowsecolor) %>%
            mutate_at(input$subbrowsecolor, ~my_ntiles(.x, input$subbrowsentile)) %>%
            rename_at(vars(input$subbrowsecolor), ~ paste0(input$subbrowsecolor, "_s"))
          
        }
        
        else if (input$subbrowseradio %in% "Off"){
          stratify_vars3.2$df %>% 
            select_(df2_join_vars()[1], df2_join_vars()[length(df2_join_vars())], input$subbrowsecolor) %>%
            rename_at(vars(input$subbrowsecolor), ~ paste0(input$subbrowsecolor, "_s"))
          
        }
      }
      
      
    })
  
  ##Select subject ordering variable 
  output$subbrowseorder <- renderUI({selectInput('subbrowseorder', 'Order Subjects By:', varnames3.2$df, selected = subbrowsevariables$order, selectize=TRUE)})
  
  ##Ordered list of subject ID
  subjectorder3.2 <- reactiveValues(l=NULL, m=NULL)
  
  observeEvent(input$go, {
    subjectorder3.2$l=as.character(stratify_vars$df_sub[["ID"]])
  })
  
  
  
  ##Ordering vars dataset, upon action button, stratify_vars3.2$df3 will update based on variable, ordered vector of subjectIDs will be created
  observeEvent(input$subbrowse1, priority = 1, ignoreInit = T, {
      stratify_vars3.2$df3 <-  
        if (!input$subbrowseorder %in% "ID"){
          stratify_vars$df_full %>% 
            group_by(ID) %>%
            summarise_at(input$subbrowseorder, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                         ifelse(is.numeric(x), mean(x, na.rm = T), "Error"))
            return(y)}) %>% 
            ungroup() %>%
            arrange_(input$subbrowseorder) %>%
            rename_at(vars(input$subbrowseorder), ~ paste0(input$subbrowseorder, "_o")) %>%
            mutate(ID_val = paste0(ID, " | ", input$subbrowseorder, "=", ordershow(.data[[paste0(input$subbrowseorder, "_o")]])))
        }
      
      else {
        stratify_vars$df_sub %>% 
          select(1) %>% 
          arrange_("ID") %>%
          mutate(ID_val = ID)
      }
      
      
      #mutate(ID_val = paste0(ID, " Order Var: ", ordershow(.data[[paste0(input$subbrowseorder, "_o")]])))
      #mutate(ID_val = ID)
      
      subjectorder3.2$l <- stratify_vars3.2$df3[["ID"]]
      subjectorder3.2$m <- stratify_vars3.2$df3[["ID_val"]]
      
      
    })
  
  ##Page Selector
  
  pagenum3.2 <- reactiveValues(l=1)
  
  output$subbrowsepage <- renderUI({selectInput('subbrowsepage', 'Go To Page:', c(1:ceiling(nrow(stratify_vars$df_sub)/4)), selectize=TRUE)})
  
  observeEvent(input$subbrowsepage, {pagenum3.2$l <- as.numeric(input$subbrowsepage)})
  
  observeEvent(input$subbrowseprev, {
    if (pagenum3.2$l > 1) {pagenum3.2$l <- pagenum3.2$l - 1}
  }) 
  
  observeEvent(input$subbrowsenext, {
    if (pagenum3.2$l < ceiling(nrow(stratify_vars$df_sub)/4)) {pagenum3.2$l <- pagenum3.2$l + 1}
  }) 
  
  ##display page
  
  output$pagenum3.2_display <- renderText(print(pagenum3.2$l))
  
  
  ##Pull increments of 16 subjects based on the value of page selector, the IDs are stored as subjectorder16
  subjectorder16_3.2 <- reactive({ 
    #when page number is max:
    if (pagenum3.2$l==ceiling(nrow(stratify_vars$df_sub)/4)) {subjectorder3.2$l[((as.numeric(pagenum3.2$l) - 1)*4 + 1) : length(subjectorder3.2$l)]}
    #otherwise: ie 1-16, 17-32, and so on
    else {subjectorder3.2$l[((pagenum3.2$l - 1)*4 + 1) : (pagenum3.2$l*4)]}
    
  }) 
  
  subjectorder16_3.2_val <-reactive({ 
    #when page number is max:
    if (pagenum3.2$l==ceiling(nrow(stratify_vars$df_sub)/4)) {subjectorder3.2$m[((as.numeric(pagenum3.2$l) - 1)*4 + 1) : length(subjectorder3.2$m)]}
    #otherwise: ie 1-16, 17-32, and so on
    else {subjectorder3.2$m[((pagenum3.2$l - 1)*4 + 1) : (pagenum3.2$l*4)]}
    
  }) 
  
  #reset page number on new plot
  
  observeEvent(input$subbrowse1, {pagenum3.2$l <- 1})
  
  
  #Random plot
  
  #make plot update after new variables are selected
  #subbrowserandomcheck <- reactiveValues(l=0, m=0)


  subbrowsevariables <- reactiveValues(var = "ID", color = "weekday", order = "ID")
  
  #Random defaults
  observeEvent(input$go, priority = -1, {
    
    subbrowsevariables$var <- names(dataset())[[sample(1:length(names(dataset())), 1)]]
    
  })
  
  subbrowserandbutton <- reactiveValues(r1=NULL, r2=NULL, r3=NULL)
  
  observeEvent(input$subbrowserandom, priority = 2, ignoreInit = T, {
    
    subbrowserandbutton$r1 <- sample(1:length(names(dataset())), 1)
    
    subbrowserandbutton$r2 <- sample(1:length(varnames3.2$df), 1)
    
    subbrowserandbutton$r3 <- sample(1:length(varnames3.2$df), 1)
    
    
    if("Main Var." %in% input$subbrowserand_choice) {subbrowsevariables$var <- names(dataset())[[subbrowserandbutton$r1]]}
    
    if("Color Var." %in% input$subbrowserand_choice) {subbrowsevariables$color <- varnames3.2$df[[subbrowserandbutton$r2]]}
    
    if ("Order Var." %in% input$subbrowserand_choice) {subbrowsevariables$order <- varnames3.2$df[[subbrowserandbutton$r3]]}
    
    
  })


  subbrowsedata <- reactiveValues(l=NULL)
  
  observeEvent( c(input$subbrowse1, pagenum3.2$l), ignoreInit = T, {
  
   subbrowsedata$l <- 
      
      if (input$subbrowseraw %in% "Raw" | input$subbrowsevar %in% "ID") {
        
        dataset_levels$df %>% 
          left_join(stratify_vars3.2$df2, by=c(df2_join_vars())) %>%
          left_join(stratify_vars3.2$df3, by="ID") %>%
          filter(ID %in% subjectorder16_3.2()) %>%
          mutate_at("ID_val", ~factor(., levels = subjectorder16_3.2_val()))
      }
      
      else if (input$subbrowseraw %in% "Subject Normalized") {
        
        dataset_levels$df %>% 
          group_by_("ID") %>%
          mutate_at(input$subbrowsevar, funs(normalize)) %>%
          ungroup() %>%
          left_join(stratify_vars3.2$df2, by=c(df2_join_vars())) %>%
          left_join(stratify_vars3.2$df3, by="ID") %>%
          filter(ID %in% subjectorder16_3.2()) %>%
          mutate_at("ID_val", ~factor(., levels = subjectorder16_3.2_val()))
        
      }
   
  
      
    })
   
  
  
  #dummy variable to control plot, since plot runs automatically when page is selected, will make it dependent on a reactive value that updates 
  #once the "create plot" button is clicked.
  
  subbrowseplot_dummy <- reactiveValues(l=0)
  
  observeEvent(input$subbrowse1, priority = 0, ignoreInit = T, {
      if (subbrowseplot_dummy$l==0) {subbrowseplot_dummy$l <- 1}
      else NULL
    })
  

  ##Reset plot when dataset changes
  
  observeEvent(input$go, {
    subbrowseplot_dummy$l <- 0
    
  })
  
  
 
  ##Instructions that appear before create plot button is clicked
  
  output$subbrowseplot_instr <-  renderText(
    if(subbrowseplot_dummy$l==0) {"Click the [Create/Update Plot] button to generate plot!"}
    else NULL 
  )
  
  
  ##Subject browser plot 
  
  
  output$subbrowseplot <- renderPlot({
    
    input$subbrowse1
    pagenum3.2$l
    
    isolate(
      
    
    if(subbrowseplot_dummy$l==0) NULL
    
    else{
      
      
        
        if(input$subbrowselevel %in% "Assessment")  {
          
          
          if (is.numeric(subbrowsedata$l[[varcolor3.2$l]]) | is.integer(subbrowsedata$l[[varcolor3.2$l]])) {
            
            ggplot(data = subbrowsedata$l, aes_string(x = "timeindex", y = input$subbrowsevar, group = "day")) +       
              geom_line(size = 1.2, color = "grey")+
              geom_point(size = 3.5, aes_string(color = varcolor3.2$l))+
              scale_color_distiller(palette = "Spectral") +
              facet_wrap(~ID_val, nrow = 4) + 
              theme_bw(base_size = 18) +
              scale_x_continuous(breaks = seq(1, max(dataset()$timeindex), by=max(dataset()$timepoint)))
            
          }
          
          else {
            
            ggplot(data = subbrowsedata$l, aes_string(x = "timeindex", y = input$subbrowsevar, group = "day")) +       
              geom_line(size = 1.2, color = "grey")+
              geom_point(size = 3.5, aes_string(color = varcolor3.2$l))+
              facet_wrap(~ID_val, nrow = 4) + 
              theme_bw(base_size = 18) +
              scale_x_continuous(breaks = seq(1, max(dataset()$timeindex), by=max(dataset()$timepoint)))
            
          }
          
        }
        
        else if (input$subbrowselevel %in% "Day")  {
          
          
          if (is.numeric(subbrowsedata$l[[varcolor3.2$l]]) | is.integer(subbrowsedata$l[[varcolor3.2$l]])) {
            
            ggplot(data = subbrowsedata$l, aes_string(x = "day", y = input$subbrowsevar, group = "ID")) +       
              geom_line(size = 1.2, color = "grey")+
              geom_point(size = 3.5, aes_string(color = varcolor3.2$l))+
              scale_color_distiller(palette = "Spectral") +
              facet_wrap(~ID_val, nrow = 4) + 
              theme_bw(base_size = 18) +
              scale_x_continuous(breaks = c(1:max(dataset()$day)))
            
          }
          
          else {
            
            ggplot(data = subbrowsedata$l, aes_string(x = "day", y = input$subbrowsevar, group = "ID")) +       
              geom_line(size = 1.2, color = "grey")+
              geom_point(size = 3.5, aes_string(color = varcolor3.2$l))+
              facet_wrap(~ID_val, nrow = 4) + 
              theme_bw(base_size = 18) +
              scale_x_continuous(breaks = c(1:max(dataset()$day)))
            
          }
          
        }
      
    }
    
    )
    
  })
  
  
  #Subject Dashboard Page 3: Subject Compare------------------------------------------------------------------------------------------------------------------------------
  
  ##Select variable 1
  output$subcomparevar1 <- renderUI({selectInput('subcomparevar1', 'Top Variable:', c(names(dataset())), selected = subcomparevariables$var1, selectize=TRUE)})
  
  
  ##select variable 2
  output$subcomparevar2 <- renderUI({selectInput('subcomparevar2', 'Bottom Variable:', c(names(dataset())), selected = subcomparevariables$var2, selectize=TRUE)})
  
  
  ##Color datasets:
  stratify_vars3.3 <- reactiveValues(df = NULL, df2 = NULL)
  
  ##Default datasets for color and order
  observeEvent(input$go, priority = -1, {
    
    stratify_vars3.3$df <- stratify_vars$df_full %>% select_("ID", "timeindex") %>% mutate(dummy_s = 1)
    stratify_vars3.3$df2 <- stratify_vars$df_full %>% select_("ID", "weekday") %>% rename(weekday_s = weekday)
    
    varnames3.3$df <- names(stratify_vars$df_full)
    
    
  })
  
  
  ##create main dataset with the appropriate level
  dataset_levels3.3 <- reactiveValues(df = NULL)
  
  observeEvent({input$subcompare1}, priority = 1, ignoreInit = T,  {
    
    if (input$subcomparelevel %in% "Day") {
      
      dataset_levels3.3$df <- dataset() %>% 
        select_("ID", "day", input$subcomparevar1, input$subcomparevar2) %>% mutate(dummy=1) %>%
        group_by(ID, day) %>%
        summarise_all(function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                ifelse(is.numeric(x), mean(x, na.rm = T), NA))
        return(y)}) %>% 
        ungroup()
      
      
    }
    
    else if (input$subcomparelevel %in% "Assessment") {
      
      dataset_levels3.3$df <- dataset() %>% select_("ID", "timeindex", "day", input$subcomparevar1, input$subcomparevar2) %>% mutate(dummy=1)
      
      
      
    }
  })
  
  
  ##Using varnames so updating stratify_vars3.3$df2 doesn't refresh subcomparecolor
  varnames3.3 <- reactiveValues(df=NULL)
  
  ##Level select for color variable, choices depend on the level of main dataset:
  
  output$subcomparecolorlevel <- renderUI({
    
    if (input$subcomparelevel %in% "Assessment") {
      radioButtons("subcomparecolorlevel", "Color Variable Level:", c("Day", "Assessment"), selected = "Assessment", inline = T)
    }
    
    else if (input$subcomparelevel %in% "Day") {
      radioButtons("subcomparecolorlevel", "Color Variable Level:", c("Day"), selected = "Day", inline = T)
    }
    
  })
  
  
  ##Key variable for each combination of raw/normalized and assessment/day/suject for coloring variable
  colorvarkey3.3 <- reactive({paste0(input$subcomparecolortype, "-", input$subcomparecolorlevel)})
  
  ##create the appropriate stratify_vars3.3$df 
  observeEvent({input$subcompare1}, priority = 1, ignoreInit = T, {
    
    if(!input$subcomparecolor %in% c("ID", "variable")) {
      
      if (colorvarkey3.3() %in% "Raw-Day") {
        stratify_vars3.3$df <- stratify_vars$df_full %>%
          group_by(ID, day) %>%
          summarise_at(input$subcomparecolor, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                        ifelse(is.numeric(x), mean(x, na.rm = T), NA))
          return(y)}) %>% 
          ungroup() 
        
      }
      
      else if (colorvarkey3.3() %in% "Raw-Assessment") {
        stratify_vars3.3$df <- stratify_vars$df_full %>% select_("ID", "timeindex", input$subcomparecolor)
      }
      
      
      else if (colorvarkey3.3() %in% "Subject Normalized-Day") {
        stratify_vars3.3$df <- stratify_vars$df_full %>%
          group_by(ID, day) %>%
          summarise_at(input$subcomparecolor, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                        ifelse(is.numeric(x), mean(x, na.rm = T), NA))
          return(y)}) %>% 
          ungroup() %>% group_by(ID) %>%
          mutate_at(input$subcomparecolor, funs(normalize)) %>%
          ungroup()
      }
      
      else if (colorvarkey3.3() %in% "Subject Normalized-Assessment") {
        stratify_vars3.3$df <- stratify_vars$df_full %>% select_("ID", "timeindex", input$subcomparecolor) %>%
          group_by(ID) %>%
          mutate_at(input$subcomparecolor, funs(normalize)) %>%
          ungroup()
      }
      
    }
    
    else {
      
      if (colorvarkey3.3() %in% c("Raw-Assessment", "Subject Normalized-Assessment")) {
        
        stratify_vars3.3$df <- stratify_vars$df_full %>% select_("ID", "timeindex") %>% mutate(dummy_s=1)}
      
      else if (colorvarkey3.3() %in% c("Raw-Day", "Subject Normalized-Day")) {
        
        stratify_vars3.3$df <- stratify_vars$df_full %>% mutate(dummy=1) %>% group_by(ID, day) %>% summarise(dummy_s=mean(dummy))}
      
    }
    })
  
  
  ##Select coloring variable (subject level, can bin into quantiles)
  output$subcomparecolor <- renderUI({selectInput('subcomparecolor', 'Color By:', c("variable", varnames3.3$df[!varnames3.3$df %in% "timeindex"]), selected=subcomparevariables$color, selectize=TRUE)})
  
  ###varcolor3.3$l is used so selecting color input doesn't automatically update plot
  varcolor3.3 <- reactiveValues(l="weekday")
  
  observeEvent({input$subcomparecolor}, ignoreInit = T, { if(input$subcomparecolor %in% c("ID", "variable")) {varcolor3.3$l <- input$subcomparecolor}
    else {varcolor3.3$l <- paste0(input$subcomparecolor, "_s")} 
  })
  
  #output$test1 <- renderText(names(subcomparedata$l))
  
  #output$test2 <- renderText(input$subcomparecolor)
  
  ##Color variable dataset join vars, depending on level and type
  df2_join_vars3.3 <- reactive({
    if (input$subcomparecolorlevel %in% "Day") {c("ID", "day")}
    else if (input$subcomparecolorlevel %in% "Assessment") {c("ID", "timeindex")}
  })
  
  ##Color vars dataset, upon action button, stratify_vars3.3$df2 will update based on variable and quantile selection
  
  observeEvent(input$subcompare1, priority = 1, ignoreInit = T, {
    
    stratify_vars3.3$df2 <-  
      
      if(input$subcomparecolor %in% c("ID", "variable")) {stratify_vars3.3$df}
    
    else {
      if(input$subcompareradio %in% "Auto"){
        stratify_vars3.3$df %>% select_(df2_join_vars3.3()[1], df2_join_vars3.3()[length(df2_join_vars3.3())], input$subcomparecolor) %>%
          mutate_at(input$subcomparecolor, function (x) { if (is.character(x) | is.factor(x)) {x}
            else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) > 20) {my_ntiles(x, input$subcomparentile)}
            else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) <= 20) {factor(x, ordered = T, exclude = c(NA, "NaN"))}
            else {x=NA}
          }) %>% 
          rename_at(vars(input$subcomparecolor), ~ paste0(input$subcomparecolor, "_s"))
      }
      
      else if (input$subcompareradio %in% "On"){
        stratify_vars3.3$df %>% select_(df2_join_vars3.3()[1], df2_join_vars3.3()[length(df2_join_vars3.3())], input$subcomparecolor) %>%
          mutate_at(input$subcomparecolor, ~my_ntiles(.x, input$subcomparentile)) %>%
          rename_at(vars(input$subcomparecolor), ~ paste0(input$subcomparecolor, "_s"))
        
      }
      
      else if (input$subcompareradio %in% "Off"){
        stratify_vars3.3$df %>% 
          select_(df2_join_vars3.3()[1], df2_join_vars3.3()[length(df2_join_vars3.3())], input$subcomparecolor) %>%
          rename_at(vars(input$subcomparecolor), ~ paste0(input$subcomparecolor, "_s"))
        
      }
    }
    
    
  })
  
  
  
  #subject selector
  output$subcomparesubject <- renderUI({selectInput('subcomparesubject', 'Select Subject:', c(unique(dataset()$ID)), selected = subcomparevariables$subject, selectize=TRUE)})
  
  
  
  
  #Random plot
  
  #make plot update after new variables are selected
  #subcomparerandomcheck <- reactiveValues(l=0, m=0)
  
  
  subcomparevariables <- reactiveValues(var1 = "ID", var2 = "ID", color = "variable", subject = NULL)
  
  #Random defaults
  observeEvent(input$go, priority = -1, {
    
    subcomparevariables$var1 <- names(dataset())[[sample(1:length(names(dataset())), 1)]]
    
    subcomparevariables$var2 <- names(dataset())[[sample(1:length(names(dataset())), 1)]]
    
    subcomparevariables$subject <- unique(dataset()$ID)[[sample(1:length(unique(dataset()$ID)), 1)]]
    
  })
  
  subcomparerandbutton <- reactiveValues(r1=NULL, r2=NULL, r3=NULL)
  
  observeEvent(input$subcomparerandom, priority = 2, ignoreInit = T, {
    
    subcomparerandbutton$r1 <- sample(1:length(names(dataset())), 1)
    
    subcomparerandbutton$r2 <- sample(1:length(names(dataset())), 1)
    
    subcomparerandbutton$r3 <- sample(1:length(varnames3.3$df), 1)
    
    subcomparerandbutton$r4 <- sample(1:length(unique(dataset()$ID)), 1)
    
    
    if("Top Var." %in% input$subcomparerand_choice) {subcomparevariables$var1 <- names(dataset())[[subcomparerandbutton$r1]]}
    
    if("Bottom Var." %in% input$subcomparerand_choice) {subcomparevariables$var2 <- names(dataset())[[subcomparerandbutton$r2]]}
    
    if ("Color Var." %in% input$subcomparerand_choice) {subcomparevariables$color <- varnames3.3$df[[subcomparerandbutton$r3]]}
    
    if ("Subject" %in% input$subcomparerand_choice) {subcomparevariables$order <- unique(dataset()$ID)[[subcomparerandbutton$r4]]}
    
  })
  
  
  subcomparedata <- reactiveValues(l=NULL)
  
  observeEvent(input$subcompare1, ignoreInit = T, {
    
    subcomparedata$l <- 
      
      if (input$subcompareraw %in% "Raw" | input$subcomparevar1 %in% "ID" |  input$subcomparevar2 %in% "ID") {
        
        if(input$subcomparevar1 == input$subcomparevar2) {
          
          dataset_levels3.3$df %>% 
            left_join(stratify_vars3.3$df2, by=c(df2_join_vars3.3())) %>%
            filter(ID %in% input$subcomparesubject) %>%
            gather(input$subcomparevar1, input$subcomparevar2, key = variable, value = value) 
        }
        
        else {
          
          dataset_levels3.3$df %>% 
            left_join(stratify_vars3.3$df2, by=c(df2_join_vars3.3())) %>%
            filter(ID %in% input$subcomparesubject) %>%
            gather(input$subcomparevar1, input$subcomparevar2, key = variable, value = value) %>%
            mutate(variable = factor(variable, levels = c(input$subcomparevar1, input$subcomparevar2)))
        }
      }
    
    else if (input$subcompareraw %in% "Subject Normalized") {
      
      if(input$subcomparevar1 == input$subcomparevar2) {
        
        dataset_levels3.3$df %>% 
          group_by_("ID") %>%
          mutate_at(c(input$subcomparevar1, input$subcomparevar2), funs(normalize)) %>%
          ungroup() %>%
          left_join(stratify_vars3.3$df2, by=c(df2_join_vars3.3())) %>%
          filter(ID %in% input$subcomparesubject) %>%
          gather(input$subcomparevar1, input$subcomparevar2, key = variable, value = value) 
        
      }
      
      else {
        
        dataset_levels3.3$df %>% 
          group_by_("ID") %>%
          mutate_at(c(input$subcomparevar1, input$subcomparevar2), funs(normalize)) %>%
          ungroup() %>%
          left_join(stratify_vars3.3$df2, by=c(df2_join_vars3.3())) %>%
          filter(ID %in% input$subcomparesubject) %>%
          gather(input$subcomparevar1, input$subcomparevar2, key = variable, value = value) %>%
          mutate(variable = factor(variable, levels = c(input$subcomparevar1, input$subcomparevar2)))
                 
      }
    }
  })
        
        
        
        #dummy variable to control plot, since plot runs automatically when page is selected, will make it dependent on a reactive value that updates 
        #once the "create plot" button is clicked.
        
        subcompareplot_dummy <- reactiveValues(l=0)
        
        observeEvent(input$subcompare1, priority = 0, ignoreInit = T, {
          if (subcompareplot_dummy$l==0) {subcompareplot_dummy$l <- 1}
          else NULL
        })
        
        
        ##Reset plot when dataset changes
        
        observeEvent(input$go, {
          subcompareplot_dummy$l <- 0
          
        })
        
        
        
        ##Instructions that appear before create plot button is clicked
        
        output$subcompareplot_instr <-  renderText(
          if(subcompareplot_dummy$l==0) {"Click the [Create/Update Plot] button to generate plot!"}
          else NULL 
        )
        
        
        ##Subject browser plot 
        
        
        output$subcompareplot <- renderPlotly({
          
          input$subcompare1
          
          
          isolate(
            
            
            if(subcompareplot_dummy$l==0) NULL
            
            else{
              
              
              
              if(input$subcomparelevel %in% "Assessment")  {
                
                
                if (is.numeric(subcomparedata$l[[varcolor3.3$l]]) | is.integer(subcomparedata$l[[varcolor3.3$l]])) {
                  
                  ggplotly(ggplot(data = subcomparedata$l, aes_string(x = "timeindex", y = "value", group = "day")) +       
                    geom_line(size = 1, color = "grey")+
                    geom_point(size = 2, aes_string(color = varcolor3.3$l))+
                    scale_color_distiller(palette = "Spectral") +
                    facet_wrap(~variable, nrow = 2, scales = "free_y") + 
                    theme_bw(base_size = 14) +
                    scale_x_continuous(breaks = seq(1, max(dataset()$timeindex), by=max(dataset()$timepoint)))+
                    ggtitle(paste0("Subject ", input$subcomparesubject))
                    )
                  
                }
                
                else {
                  
                  ggplotly(ggplot(data = subcomparedata$l, aes_string(x = "timeindex", y = "value", group = "day")) +       
                    geom_line(size = 1, color = "grey")+
                    geom_point(size = 2, aes_string(color = varcolor3.3$l))+
                    facet_wrap(~variable, nrow = 2, scales = "free_y") + 
                    theme_bw(base_size = 14) +
                    scale_x_continuous(breaks = seq(1, max(dataset()$timeindex), by=max(dataset()$timepoint)))+
                    ggtitle(paste0("Subject ", input$subcomparesubject))
                  )
                  
                }
                
              }
              
              else if (input$subcomparelevel %in% "Day")  {
                
                
                if (is.numeric(subcomparedata$l[[varcolor3.3$l]]) | is.integer(subcomparedata$l[[varcolor3.3$l]])) {
                  
                  ggplotly(ggplot(data = subcomparedata$l, aes_string(x = "day", y = "value", group = "ID")) +       
                    geom_line(size = 1, color = "grey")+
                    geom_point(size = 2, aes_string(color = varcolor3.3$l))+
                    scale_color_distiller(palette = "Spectral") +
                    facet_wrap(~variable, nrow = 2, scales = "free_y") + 
                    theme_bw(base_size = 14) +
                    scale_x_continuous(breaks = c(1:max(dataset()$day)))+
                    ggtitle(paste0("Subject ", input$subcomparesubject))
                  )
                  
                }
                
                else {
                  
                  ggplotly(ggplot(data = subcomparedata$l, aes_string(x = "day", y = "value", group = "ID")) +       
                    geom_line(size = 1, color = "grey")+
                    geom_point(size = 2, aes_string(color = varcolor3.3$l))+
                    facet_wrap(~variable, nrow = 2, scales = "free_y") + 
                    theme_bw(base_size = 14) +
                    scale_x_continuous(breaks = c(1:max(dataset()$day))) +
                    ggtitle(paste0("Subject ", input$subcomparesubject))
                  )
                  
                }
                
              }
              
            }
            
          )
          
        })
      

}


shinyApp(ui = ui, server = server)  


#next: add day level,fix df2 function for entire app, fix clicking buttons too fast,



