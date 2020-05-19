# Basic app Work in Progress
# Input stk 
# Text input for submodels
# Mortality vectors for sensitivity tests

library(shiny)
library(shinythemes)
library(FLCore)
library(FLa4a)
library(plotly)
library(reshape2)
library(ggplotFL); theme_set(theme_bw())
data("ple4")
data("ple4.index")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),
                
                # Application title
                titlePanel("Stock assessment"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    
                    fileInput("file1", label = "Stock object"),
                    fileInput("file2", label = "Index object"),
                    
                    radioButtons("menuType", "Choose method of Submodel input :",
                                c("Manually" = "manual",
                                  "Drop down menu" = "dropdown")
                                ),
                    
                    ### DROPDOWN MENU
                    conditionalPanel(
                      condition = "input.menuType == 'dropdown'",
                      
                      # Choose f model
                      selectInput("fmodelType", "Choose F model",
                                  c("Factor on year" = "factor", 
                                    "Spline on year" = "spline",
                                    "Factor on year and age" = "factor_age",
                                    "Splines on year and age" = "spline_age"
                                  )
                      ),
                      conditionalPanel(
                        condition = "input.fmodelType == 'spline' || input.fmodelType == 'spline_age'",
                        # knots in splines on year
                        sliderInput("fk", label = "knots in F model on year", value = 10,
                                    min = 3, 
                                    max = 30,
                                    step = 1)
                      ),
                      conditionalPanel(
                        condition = "input.fmodelType == 'spline_age'",
                        # knots in splines on age
                        sliderInput("fk_age", label = "knots in F model on age", value = 10,
                                    min = 3, max = 10, step = 1)
                      ),
                      
                      # Choose sr model
                      selectInput("srmodelType", "Choose SR model",
                                  c(Factor = "factor", Spline = "spline")
                      ),
                      conditionalPanel(condition = "input.srmodelType == 'spline'",
                                       # Fmodel
                                       sliderInput("srk", label = "knots in SR model", value = 10,
                                                   min = 3, max = 30, step = 1)
                      ),
                      
                      # Choose q model
                      selectInput("qmodelType", "Choose q model",
                                  c(Factor = "factor", Spline = "spline")
                      ),
                      conditionalPanel(condition = "input.qmodelType == 'spline'",
                                       # Fmodel
                                       sliderInput("qk", label = "knots in q model", value = 10,
                                                   min = 3, max = 10, step = 1)
                      )
                    ),
                    
                    conditionalPanel(
                      condition = "input.menuType == 'manual'",
                      textInput("fmodel", "fmodel :", "~factor(year)"),
                      textInput("qmodel", "qmodel (separate qmodels with &) :", "~factor(age)"),
                      textInput("srmodel", "srmodel :", "~factor(year)")
                    )
                    
                    
                  ),
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Assessment", 
                               plotOutput("assessment"),
                               br(),
                               textOutput("AICBIC")),
                      tabPanel("Diagnostics",
                               h3("Log Residuals"),
                               plotOutput("residuals"),
                               br(),
                               h3("Fit VS Original values"),
                               plotOutput("fitVSstk")),
                      tabPanel("Submodels", 
                               actionButton(inputId="plot1","Plot F model"),
                               # Choose index to plot
                               selectInput("indexName", "Choose index to plot :"," "),
                               actionButton(inputId="plot2","Plot q model"),
                               plotlyOutput("plotF"),
                               plotlyOutput("plotQ"))
                    )
                  )
                )
)

# Server
# Define server logic

server <- function(input, output, session) {
  data("ple4")
  data("ple4.index")
  
  ### Create stk object from input data:
  stk <- reactive({
    if ( is.null(input$file1)) return(NULL)
    inFile <- input$file1
    file <- inFile$datapath
    # load the file into new environment and get it from there
    e = new.env()
    name <- load(file, envir = e)
    return(e[[name]])
  })
  
  ### Create index object from input data:
  idx <- reactive({
    if ( is.null(input$file2)) return(NULL)
    inFile <- input$file2
    file <- inFile$datapath
    # load the file into new environment and get it from there
    e = new.env()
    name <- load(file, envir = e)
    return(e[[name]])
  })
  
  ### Built models for DROPDOWN MENU
  # Built F model
  fmod <- reactive({
    if (input$fmodelType == 'spline'){
      return(as.formula(paste0("~s(year, k = ", input$fk, ")")))
    } 
    else if (input$fmodelType == 'factor') {
      return(as.formula("~factor(year)"))
    }
    else if (input$fmodelType == 'spline_age') {
      return(as.formula(paste0("~s(year, k = ", input$fk, ")+s(age, k = ", input$fk_age,")")))
    } else {
      return(as.formula("~factor(year)+factor(age)"))
    }
  })
  
  # Built q model
  qmod <- reactive({
    if (input$qmodelType == 'spline'){
      return(list(as.formula(paste0("~s(age, k = ", input$qk, ")"))))
    } else {
      return(list(as.formula("~factor(age)")))
    }
  })
  
  # Built SR model
  srmod <- reactive({
    if (input$srmodelType == 'spline'){
      return(as.formula(paste0("~s(year, k = ", input$srk, ")")))
    } else {
      return(as.formula("~factor(year)"))
    }
  })
  
  ### Built models for MANUAL
  
  fmod_2 <- reactive({
    return(as.formula(input$fmodel))
  })
  qmod_2 <- reactive({
    y <- unlist(strsplit(input$qmodel,"&"))
    ls <- list()
    for(i in 1:length(y)){ls[[i]] <- y[i]}
    ls <- lapply(ls, formula)
    return(ls)
  })
  srmod_2 <- reactive({
    return(as.formula(input$srmodel))
  })
  
  ############ Basic assessment function ##############
  # sca reactive function
  fit <- reactive({
    if (input$menuType == 'dropdown'){
      return(sca(stk(),idx(), fmodel = fmod(), qmodel = qmod(), srmodel = srmod()))
    } else {
      return(sca(stk(),idx(), fmodel = fmod_2(), qmodel = qmod_2(), srmodel = srmod_2()))
    }
    
  })
  
  #####################################################
  
  # Plot the assessment results with fit()
  output$assessment <- renderPlot({
    stk.a4a <- stk() + fit()
    plot(stk.a4a)
  })
  
  # Print AIC and BIC
  output$AICBIC <- renderText({
    paste0("AIC : ", AIC(fit()),", BIC : ", BIC(fit()))
  })
  
  ### DIAGNOSTICS TAB
  # Plot the residuals
  output$residuals <- renderPlot({
    res <- residuals(fit(), stk(), idx())
    plot(res)
  })
  
  output$fitVSstk <- renderPlot({
    plot(fit(), stk())
  })
  
  observeEvent(input$plot1,{
    output$plotF <- renderPlotly({
      stk.a4a <- stk() + fit()
      tmp = as.data.frame(harvest(stk.a4a))
      tmp = tmp[,c(1,2,7)]
      df <- acast(tmp, age~year, value.var="data")
      fig <- plot_ly(z = ~ df)
      fig <- fig %>% add_surface()
      fig <- fig %>% layout(scene = list(xaxis = list(title = 'year'),
                                         yaxis = list(title = 'age'),
                                         zaxis = list(title = 'f')))
      
      fig
    })
  })
  
  idxNames = reactive({
    names(idx())
  })
  observe({
    updateSelectInput(session, "indexName",
                      choices = idxNames()
    )})
  
  observeEvent(input$plot2,{
    output$plotQ <- renderPlotly({
      fit <- fit()
      idx <- idx()
      sfrac <- mean(range(idx[[input$indexName]])[c("startf", "endf")])
      Z <- (m(stk()) + harvest(fit))*sfrac # check M * sfrac
      lst <- dimnames(fit@index[[input$indexName]])
      lst$x <- stock.n(fit)*exp(-Z)
      stkn <- do.call("trim", lst)
      tmp = as.data.frame(index(fit)[[input$indexName]]/stkn)
      tmp = tmp[,c(1,2,7)]
      df <- acast(tmp, age~year, value.var="data")
      
      fig <- plot_ly(z = ~ df)
      fig <- fig %>% add_surface()
      fig <- fig %>% layout(scene = list(xaxis = list(title = 'year'),
                                         yaxis = list(title = 'age'),
                                         zaxis = list(title = 'Catchability')))
      
      fig
    })
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)