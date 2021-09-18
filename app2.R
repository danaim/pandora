library(shiny)
library(shinythemes)
library(FLCore)
library(FLa4a)
library(plotly)
library(reshape2)
library(ggplotFL); theme_set(theme_bw())

## Only run examples in interactive R sessions

  
  ui <- fluidPage(
    
    # Input files
    fileInput("file1", label = "Stock object"),
    fileInput("file2", label = "Index object"),
    
    textInput("fmodel", "fmodel :",value = ""),
    textInput("qmodel", "qmodel (separate qmodels with &) :",value = "~factor(replace(age,age>3,3))"),
    textInput("srmodel", "srmodel :",value = ""),
    textOutput("debug"),
    plotOutput("assessment"),
    actionButton("run","Run Assessment")
    
  )
  server <- function(input, output) {
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
    
    # output$debug <- renderText({
    #   paste0("fmodel: ", as.character(fmod_2()), "  qmodel: ", as.character(qmod_2()))
    # })
    
    # output$assessment <- renderPlot({
    #   stk.a4a <- stk() + fit()
    #   plot(stk.a4a)
    # })
    # 
    # fit <- reactive({
    #   return(sca(stk(),idx(), fmodel = fmod_2(), qmodel = qmod_2(), srmodel = srmod_2()))
    # })
    
    # observeEvent(input$run,{
    #   fit <- reactive({
    #     return(sca(stk(),idx(), fmodel = fmod_2(), qmodel = qmod_2(), srmodel = srmod_2()))
    #   })
    #   
    #   output$assessment <- renderPlot({
    #     stk.a4a <- stk() + fit()
    #     plot(FLStocks(fitted = stk.a4a, catch = stk()))
    #   })
    # })
    # 
    
    fit <- eventReactive(input$run,{
      return(sca(stk(),idx(), fmodel = fmod_2(), qmodel = qmod_2(), srmodel = srmod_2()))
    })
    output$assessment <- renderPlot({
      stk.a4a <- stk() + fit()
      plot(FLStocks(fitted = stk.a4a, catch = stk()))
    })
    
  }
  shinyApp(ui, server)

