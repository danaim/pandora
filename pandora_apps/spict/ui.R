ui <- fluidPage(theme = shinytheme("superhero"),
                
                #Title developed as 
                #Create a page with a top level navigation bar
                navbarPage("Stock assessment - a4a - SPiCT",
                           tabPanel("Intro",
                                    
                                    h4( p(strong("General"))),
                                    p("Through this application the user can easily run two stock assessment models with no programming knowledge required.
                                      The two models are the stochastic surplus production model in continious time  (SPiCT) and the statistical catch-at-age stock assesment model developed as part of the Assessment For ALL (a4a). 
                                      For both models general information is provided about the dependencies, type of data needed and settings."),
                                    br(),
                                    
                                    h4(p(strong('Disclaimer'))),
                                    p('The application was created with R programming language [1], 
                                      is free to use and comes with absolutely no warranty.'),
                                    br(), 
                                    
                                    h4( p(strong("Creators"))),
                                    p ('Maria Kikeri, Danai Mantopoulou-Palouka, Vasiliki Sgardeli and Dimitrios Damalas. contact: shark@hcmr.gr'),
                                    br(),
                                    
                                    h4(p(strong('References')), align='bottom'),
                                    h6(p('[1] R Core Team (2020). R: A language and environment for statistical computing. R Foundation for
                                       Statistical Computing, Vienna, Austria. URL https://www.R-project.org/')),
                                    
                           ),
                           tabPanel("SPiCT",          
                                    #Tab panel with info
                                    tabsetPanel(
                                      tabPanel("Info",
                                                
                                                 h4("SPiCT"),
                                                 br(),
                                                 h4( p(strong("General"))),
                                                 p("This application implements the Stochastic Production Model in Continuous Time (SPiCT, Pedersen & Berg 2016) [1] and its time- 
                                                        variant productivity extentions (Mildenberger et al. 2020) [2]."),
                                                 br(),
                                                 
                                                 h4( p(strong("Dependencies"))),
                                                 p("Runs with spict_v1.3.0, R version > 4 and requires libraries: shiny, spict, TMB, ggplot2, ellipse,
                                                        gridExtra, dplyr, googleVis"),
                                                 br(),
                                                 
                                                 h4( p(strong("Input Data and Settings"))),
                                                 p("To run the assessment, the input data should be provided in a tab delimited text file including one time series of catch data and one or more 
                                                        time series of a biomass index. The input file has an even number of columns, which are: timeC: years of catch data, obsC: catch data,
                                                        timeI: years of first Index, obsI: first Index, timeI1: years of second Index, obsI1: second Index, etc.
                                                        For seasonal data, the time columns should be decimal, e.g. 2021,5 will be used to refer to June 2021."),     
                                                 br(),
                                               
                                                 h4( p(strong("Acknowledgments"))),
                                                 p('The spict code for the time-variant models was implemented with the help and guidance of Casper Willestofte Berg'),
                                                 br(), br(),
                                                 
                                                 h4( p(strong("References"))),
                                                 h6( p("[1] Pedersen, M.W., Berg, C.W. 2017. A stochastic surplus production model in continuous time. Fish and Fisheries, 18: 226-243."),
                                                     p("[2] Mildenberger, T.K., Berg, C.W., Pedersen, M.W., Kokkalis, A., Nielsen, J.R. 2020. Time-variant productivity in biomass dynamic models 
                                                        on seasonal and long-term scales, ICES Journal of Marine Science, 77(1): 174-187.")
                                                 )
                                              
                                               # )
                                               
                                      )
                                      
                                      
                                      ,
                                      #Tab panel with Input (Data - Model)
                                      tabPanel("Input",
                                               navbarPage('Input',
                                                          #tab panel inside Input
                                                          tabPanel('Data',
                                                                   sidebarLayout(
                                                                     sidebarPanel(
                                                                       #Input data
                                                                       fileInput("catchindex",label = "Data Catch - Index"),
                                                                       
                                                                       hr(),
                                                                       #Message with the number of indices (Options: One index / Many indices)
                                                                       fluidRow(column(12,style = "height:32px; background-color:; font-size:14px; font-weight:bold; color:#FF6600;",textOutput("col"))),
                                                                       
                                                                       #Message with the type of data (Options: Seasonal / No seasonal)
                                                                       fluidRow(column(12,style = "height:32px; background-color:; font-size:14px; font-weight:bold; color:#FF6600;",textOutput("seasonalno"))),
                                                                       
                                                                       #Plot options: Simple, advanced, shifted
                                                                       radioButtons("plotoption", "Plotting data",
                                                                                    choices=c("Simple" = "simple", 
                                                                                              "Advanced" = "advanced",
                                                                                              "Shifted" = "shifted")),
                                                                       conditionalPanel(
                                                                         
                                                                         condition = "input.plotoption == 'shifted'",
                                                                         
                                                                         conditionalPanel(
                                                                           
                                                                           condition =   "output.col=='One index'  ",
                                                                           
                                                                           
                                                                           sliderInput("shiftcatch", "Shift catch:",min = 0, max = 1,value= 0,step = 0.1),
                                                                           
                                                                           
                                                                           sliderInput("shiftindex", "Shift index 1:",min = 0, max = 1,value= 0,step = 0.1)),
                                                                         
                                                                         
                                                                         conditionalPanel(
                                                                           
                                                                           condition = "output.col=='Many indices' ",  
                                                                           
                                                                           
                                                                           sliderInput("shiftcatch1", "Shift catch:",min = 0, max = 1,value= 0,step = 0.1),
                                                                           
                                                                           #insert as many sliders as the number of indices
                                                                           uiOutput("sliders"))
                                                                         
                                                                       )
                                                                     )
                                                                     ,
                                                                     mainPanel(
                                                                       tabsetPanel(
                                                                         tabPanel("Plotting data", 
                                                                                  plotOutput("dataplot", height=600) %>% withSpinner(color="#e5ecec"),
                                                                                  #download plot 
                                                                                  downloadButton('downloadPlotData', 'Download Plots'),
                                                                                  br()
                                                                         )
                                                                       )
                                                                     )
                                                                   )
                                                          )
                                                          ,
                                                          tabPanel('Model',
                                                                   #fluidPage(
                                                                   #Select model
                                                                   
                                                                   
                                                                   selectInput("selectmodel", "Select model",
                                                                               c("Original model" = "original", 
                                                                                 "Regime Shift" = "regime",
                                                                                 "Gradual shift" = "gradual" )),
                                                                   
                                                                   
                                                                   #Model original SPiCT
                                                                   
                                                                   conditionalPanel(
                                                                     
                                                                     condition = "input.selectmodel=='original'",
                                                                     
                                                                     uiOutput("Scaefer"),
                                                                     
                                                                     uiOutput("checkboxab"),
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "output.seasonalno=='Seasonal data'",
                                                                       
                                                                       checkboxInput("checkboxseasontype", label = "Seasonal pattern",value=FALSE),
                                                                       
                                                                       
                                                                       conditionalPanel(
                                                                         condition = "(input.checkboxseasontype == 1) ",
                                                                         numericInput("seasontype", "Season pattern:", NULL, min = 0, max = 100000),
                                                                         bsPopover(id = "seasontype", 
                                                                                   title = "Seasonal pattern of fishing mortality, 1: fixed pattern with use of cyclic B-splines (default), 2: varying pattern with use of coupled SDEs",
                                                                                   placement="right",trigger = "hover")),
                                                                       
                                                                       checkboxInput("checkboxorder", label = "Order of the spline",value=FALSE),
                                                                       conditionalPanel(
                                                                         condition = "(input.checkboxorder == 1) ",
                                                                         numericInput("orderspline", "Order of the spline:", NULL, min = 0, max = 100000),
                                                                         bsPopover(id = "orderspline", 
                                                                                   title = "Order of the seasonal spline",
                                                                                   placement="right",trigger = "hover"))),
                                                                     
                                                                     checkboxInput("checkbox", label = "Use priors"),
                                                                     
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "input.checkbox == 1",
                                                                       
                                                                       pickerInput("priors", "Select priors", c("r" = "growth", 
                                                                                                                "MSY" = "MSY",
                                                                                                                "K" = "capacity",
                                                                                                                "B" = "biomass",
                                                                                                                "F" = "F",
                                                                                                                "BBmsy" = "BBmsy",
                                                                                                                "FFmsy" = "FFmsy",
                                                                                                                "bkfrac" = "bkfrac"), multiple = T)),
                                                                     
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "(input.priors.indexOf('growth') > -1 & input.checkbox == 1) ",
                                                                       
                                                                       uiOutput("priorgrowthm"),
                                                                       
                                                                       uiOutput("priorgrowthsd")),
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "(input.priors.indexOf('MSY') > -1 & input.checkbox == 1) ",
                                                                       
                                                                       uiOutput("priormeanm"),
                                                                       
                                                                       uiOutput("priormeansd")),
                                                                     
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "(input.priors.indexOf('capacity') > -1 & input.checkbox == 1) ",
                                                                       
                                                                       uiOutput("priorcapacitym"),
                                                                       
                                                                       uiOutput("priorcapacitysd")),
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "(input.priors.indexOf('biomass') > -1 & input.checkbox == 1) ",
                                                                       
                                                                       uiOutput("priorbiomassm"),
                                                                       
                                                                       uiOutput("priorbiomasssd"),
                                                                       
                                                                       uiOutput("slider1")),
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "(input.priors.indexOf('F') > -1 & input.checkbox == 1) ",
                                                                       
                                                                       uiOutput("priorfishm"),
                                                                       
                                                                       uiOutput("priorfishsd"),
                                                                       
                                                                       uiOutput("slider2")),
                                                                     
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "(input.priors.indexOf('BBmsy') > -1 & input.checkbox == 1) ",
                                                                       
                                                                       uiOutput("priorBBmsym"),
                                                                       
                                                                       uiOutput("priorBBmsysd"),
                                                                       uiOutput("slider3")),
                                                                     
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "(input.priors.indexOf('FFmsy') > -1 & input.checkbox == 1) ",
                                                                       
                                                                       uiOutput("priorFFmsym"),
                                                                       
                                                                       uiOutput("priorFFmsysd"),
                                                                       
                                                                       uiOutput("slider4")),
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "(input.priors.indexOf('bkfrac') > -1 & input.checkbox == 1) ",
                                                                       
                                                                       uiOutput("priorbkfracm"),
                                                                       
                                                                       uiOutput("priorbkfracsd"),
                                                                       
                                                                     )),
                                                                   
                                                                   
                                                                   #Model Regime Shift SPiCT 
                                                                   conditionalPanel(
                                                                     
                                                                     condition = "input.selectmodel=='regime'",
                                                                     
                                                                     uiOutput("slider10"),
                                                                     
                                                                     uiOutput("Scaefer1"),
                                                                     
                                                                     uiOutput("checkboxab1"),
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "output.seasonalno=='Seasonal data'",
                                                                       
                                                                       checkboxInput("checkboxseasontype1", label = "Seasonal pattern",value=FALSE),
                                                                       
                                                                       conditionalPanel(
                                                                         condition = "(input.checkboxseasontype1 == 1) ",
                                                                         numericInput("seasontype1", "Season pattern:", NULL, min = 0, max = 100000),
                                                                         bsPopover(id = "seasontype1", 
                                                                                   title = "Seasonal pattern of fishing mortality, 1: fixed pattern with use of cyclic B-splines (default), 2: varying pattern with use of coupled SDEs",
                                                                                   placement="right",trigger = "hover")),
                                                                       
                                                                       checkboxInput("checkboxorder1", label = "Order of the spline",value=FALSE),
                                                                       
                                                                       conditionalPanel(
                                                                         condition = "(input.checkboxorder1 == 1) ",
                                                                         numericInput("orderspline1", "Order of the spline:", NULL, min = 0, max = 100000),
                                                                         bsPopover(id = "orderspline1", title = "Order of the seasonal spline",placement="right",trigger = "hover")) 
                                                                       
                                                                       
                                                                     ),
                                                                     
                                                                     checkboxInput("checkbox1", label = "Use priors"),
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "input.checkbox1 == 1",
                                                                       
                                                                       pickerInput("priors1", "Select priors", c("r" = "growth", 
                                                                                                                 "MSY" = "MSY",
                                                                                                                 "K" = "capacity",
                                                                                                                 "B" = "biomass",
                                                                                                                 "F" = "F",
                                                                                                                 "BBmsy" = "BBmsy",
                                                                                                                 "FFmsy" = "FFmsy",
                                                                                                                 "bkfrac" = "bkfrac"), multiple = T)),
                                                                     
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "(input.priors1.indexOf('growth') > -1 & input.checkbox1 == 1) ",
                                                                       
                                                                       uiOutput("prior1growthm"),
                                                                       
                                                                       uiOutput("prior1growthsd")),
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "(input.priors1.indexOf('MSY') > -1 & input.checkbox1 == 1) ",
                                                                       
                                                                       uiOutput("prior1meanm"),
                                                                       
                                                                       uiOutput("prior1meansd")),
                                                                     
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "(input.priors1.indexOf('capacity') > -1 & input.checkbox1 == 1) ",
                                                                       
                                                                       uiOutput("prior1capacitym"),
                                                                       
                                                                       uiOutput("prior1capacitysd")),
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "(input.priors1.indexOf('biomass') > -1 & input.checkbox1 == 1) ",
                                                                       
                                                                       uiOutput("prior1biomassm"),
                                                                       
                                                                       uiOutput("prior1biomasssd"),
                                                                       
                                                                       uiOutput("slider11")),
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "(input.priors1.indexOf('') > -1 & input.checkbox1 == 1) ",
                                                                       
                                                                       uiOutput("prior1fishm"),
                                                                       
                                                                       uiOutput("prior1fishsd"),
                                                                       
                                                                       
                                                                       uiOutput("slider12")),
                                                                     
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "(input.priors1.indexOf('BBmsy') > -1 & input.checkbox1 == 1) ",
                                                                       
                                                                       uiOutput("prior1BBmsym"),
                                                                       
                                                                       uiOutput("prior1BBmsysd"),
                                                                       uiOutput("slider13")),
                                                                     
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "(input.priors1.indexOf('FFmsy') > -1 & input.checkbox1 == 1) ",
                                                                       
                                                                       uiOutput("prior1FFmsym"),
                                                                       
                                                                       uiOutput("prior1FFmsysd"),
                                                                       
                                                                       uiOutput("slider14")),
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "(input.priors1.indexOf('bkfrac') > -1 & input.checkbox1 == 1) ",
                                                                       
                                                                       uiOutput("prior1bkfracm"),
                                                                       
                                                                       uiOutput("prior1bkfracsd"),
                                                                       
                                                                     )),
                                                                   
                                                                   
                                                                   
                                                                   #Model Gradual shift model
                                                                   conditionalPanel(
                                                                     condition = "input.selectmodel == 'gradual' ",
                                                                     uiOutput("Scaefer2"),
                                                                     
                                                                     uiOutput("checkboxab2"),
                                                                     
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "output.seasonalno=='Seasonal data'",
                                                                       
                                                                       checkboxInput("checkboxseasontype2", label = "Seasonal pattern",value=FALSE),
                                                                       
                                                                       conditionalPanel(
                                                                         condition = "(input.checkboxseasontype2 == 1) ",
                                                                         numericInput("seasontype2", "Season pattern:", NULL, min = 0, max = 100000),
                                                                         bsPopover(id = "seasontype2", 
                                                                                   title = "Seasonal pattern of fishing mortality, 1: fixed pattern with use of cyclic B-splines (default), 2: varying pattern with use of coupled SDEs",
                                                                                   placement="right",trigger = "hover")),
                                                                       
                                                                       checkboxInput("checkboxorder2", label = "Order of the spline",value=FALSE),
                                                                       
                                                                       conditionalPanel(
                                                                         condition = "(input.checkboxorder2 == 1) ",
                                                                         numericInput("orderspline2", "Order of the spline:", NULL, min = 0, max = 100000),
                                                                         bsPopover(id = "orderspline2", title = "Order of the seasonal spline",placement="right",trigger = "hover"))),
                                                                     
                                                                     checkboxInput("checkbox2", label = "Use priors"),
                                                                     
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "input.checkbox2 == 1",
                                                                       
                                                                       pickerInput("priors2", "Select priors", c("r" = "growth", 
                                                                                                                 "MSY" = "MSY",
                                                                                                                 "K" = "capacity",
                                                                                                                 "B" = "biomass",
                                                                                                                 "F" = "F",
                                                                                                                 "BBmsy" = "BBmsy",
                                                                                                                 "FFmsy" = "FFmsy",
                                                                                                                 "bkfrac" = "bkfrac"), multiple = T)),
                                                                     
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "(input.priors2.indexOf('growth') > -1 & input.checkbox2 == 1) ",
                                                                       
                                                                       uiOutput("prior2growthm"),
                                                                       
                                                                       uiOutput("prior2growthsd")),
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "(input.priors2.indexOf('MSY') > -1 & input.checkbox2 == 1) ",
                                                                       
                                                                       uiOutput("prior2meanm"),
                                                                       
                                                                       uiOutput("prior2meansd")),
                                                                     
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "(input.priors2.indexOf('capacity') > -1 & input.checkbox2 == 1) ",
                                                                       
                                                                       uiOutput("prior2capacitym"),
                                                                       
                                                                       uiOutput("prior2capacitysd")),
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "(input.priors2.indexOf('biomass') > -1 & input.checkbox2 == 1) ",
                                                                       
                                                                       uiOutput("prior2biomassm"),
                                                                       
                                                                       uiOutput("prior2biomasssd"),
                                                                       
                                                                       uiOutput("slider17")),
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "(input.priors2.indexOf('') > -1 & input.checkbox2 == 1) ",
                                                                       
                                                                       uiOutput("prior2fishm"),
                                                                       
                                                                       uiOutput("prior2fishsd"),
                                                                       
                                                                       
                                                                       uiOutput("slider18")),
                                                                     
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "(input.priors2.indexOf('BBmsy') > -1 & input.checkbox2 == 1) ",
                                                                       
                                                                       uiOutput("prior2BBmsym"),
                                                                       
                                                                       uiOutput("prior2BBmsysd"),
                                                                       uiOutput("slider19")),
                                                                     
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "(input.priors2.indexOf('FFmsy') > -1 & input.checkbox2 == 1) ",
                                                                       
                                                                       uiOutput("prior2FFmsym"),
                                                                       
                                                                       uiOutput("prior2FFmsysd"),
                                                                       
                                                                       uiOutput("slider20")),
                                                                     
                                                                     conditionalPanel(
                                                                       
                                                                       condition = "(input.priors2.indexOf('bkfrac') > -1 & input.checkbox2 == 1) ",
                                                                       
                                                                       uiOutput("prior2bkfracm"),
                                                                       
                                                                       uiOutput("prior2bkfracsd")
                                                                       
                                                                     ))
                                                                   
                                                                   
                                                                   
                                                                   
                                                                   
                                                                   
                                                                   
                                                                   
                                                                   
                                                                   
                                                          )
                                                          
                                                          
                                                          
                                               )  
                                      )
                                      
                                      
                                      ,
                                      #Tab panel Output (Plot results - Residuals and diagnostics - Retrospective - Simulating data - Summary - Forecasting)
                                      tabPanel('Run assessment',
                                               navbarPage('Output',
                                                          
                                                          tabPanel('Summary plot',
                                                                   plotOutput("plotRun", height=600)%>% withSpinner(color="#e5ecec"),
                                                                   downloadButton('downloadPlotSPiCT', 'Download Plot')
                                                                   
                                                                   
                                                          )
                                                          ,
                                                          tabPanel('Single plots',
                                                                   sidebarLayout(
                                                                     sidebarPanel(
                                                                       radioButtons("plots", "Plots:",
                                                                                    c("Absolute biomass" = "bio",
                                                                                      "Relative biomass" = "rbio",
                                                                                      "Absolute fishing mortality" = "fmort",
                                                                                      "Relative fishing mortality" = "rfmort",
                                                                                      "Catch" = "catch",
                                                                                      "Production curve" = "curve",
                                                                                      "Kobe plot" = "kobe",
                                                                                      "Priors" = "priors"
                                                                                    ))
                                                                       
                                                                     ),
                                                                     
                                                                     mainPanel(
                                                                       tabPanel("Plot results", 
                                                                                plotOutput("plots1", height=600)%>% withSpinner(color="#e5ecec"),
                                                                                downloadButton('downloadPlot', 'Download Plots')
                                                                                
                                                                       )
                                                                     )
                                                                   )
                                                                   
                                                          )
                                                          
                                                          
                                                          
                                                          
                                                          ,
                                                          
                                                          tabPanel("Residuals and diagnostics", 
                                                                   plotOutput("residuals", height=600)%>% withSpinner(color="#e5ecec"),
                                                                   downloadButton('downloadPlotResiduals', 'Download Plot'),
                                                                   br(),
                                                                   h4( p(strong("Residuals"))),
                                                                   p("1. Log of the input data series."),
                                                                   
                                                                   p("2. OSA residuals with the p-value of a bias test. If the header is green the test is not significant."),
                                                                   
                                                                   p("3. Empirical autocorrelation of the residuals. A Ljung-Box simultaneous test of multiple lags with p-value shown in the header, and tests for individual lags shown by dashed horizontal lines in the plot"),
                                                                   
                                                                   p("4. Tests for normality of the residuals both as a QQ-plot and with a Shapiro test."),
                                                                   
                                                                   br()
                                                                   
                                                          ),
                                                          tabPanel("Retrospective",
                                                                   plotOutput("retro", height=600)%>% withSpinner(color="#e5ecec"),
                                                                   downloadButton('downloadPlotRetro', 'Download Plot'),
                                                                   
                                                                   br(),
                                                                   
                                                                   #h4( p(strong("Residuals"))),
                                                                   p("Retrospecitive plots are sometimes used to evaluate the robustness of the model fit to the introduction of new data, i.e. to check whether the fit changes substantially when new data becomes available"),
                                                                   
                                                                   br()
                                                                   
                                                          ),
                                                          tabPanel("Simulating data",
                                                                   plotOutput("simulation", height=600)%>% withSpinner(color="#e5ecec"),
                                                                   downloadButton('downloadPlotSimulating', 'Download Plot'),
                                                          ),
                                                          
                                                          tabPanel("Model parameters", tableOutput("tablesummary")%>%withSpinner(color="#e5ecec")),
                                                          
                                                          tabPanel("Forecasting", verbatimTextOutput("forecastsum")%>% withSpinner(color="#e5ecec")
                                                                   
                                                          )
                                               )
                                      )
                                    )  
                           )
                )
)