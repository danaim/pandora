# Basic app Work in Progress
# Mortality vectors for sensitivity tests

library(shiny)
library(shinythemes)
library(FLCore)
library(FLa4a)
library(FLBRP)
library(plotly)
library(reshape2)
library(ggplotFL); theme_set(theme_bw())
library(reshape2)
source('retro.R')
source('cohorts_consistency.R')


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("darkly"),
                #####
                # HTML
                tags$head(
                  tags$style(HTML('#run{border: 0.5px solid white;}'))
                ),
                #####
                # Application title
                titlePanel("a4a Stock assessment"),
                
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
                                  c("Factor on year and age" = "factor_age",
                                    "Splines on year and age" = "spline_age",
                                    "Factor on year spline on age" = "factor", 
                                    "Spline on year factor on age" = "spline"
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
                        condition = "input.fmodelType == 'spline_age' || input.fmodelType == 'factor'",
                        # knots in splines on age
                        sliderInput("fk_age", label = "knots in F model on age", value = 10,
                                    min = 3, max = 10, step = 1)
                      ),
                      
                      # Choose sr model
                      selectInput("srmodelType", "Choose SR model",
                                  c("Factor" = "factor", 
                                    "Spline" = "spline",
                                    "Geometric mean" = "geomean",
                                    "Ricker" = "ricker",
                                    "Beverton - Holt" = "bevholt",
                                    "Hockey stick" = "hockey")
                      ),
                      conditionalPanel(condition = "input.srmodelType == 'spline'",
                                       # Fmodel
                                       sliderInput("srk", label = "knots in SR model", value = 10,
                                                   min = 3, max = 30, step = 1)
                      ),
                      conditionalPanel(condition = "!(input.srmodelType == 'spline' || input.srmodelType == 'factor' )",
                                       # Fmodel
                                       sliderInput("cv", label = "CV", value = 0.2,
                                                   min = 0.1, max = 1, step = 0.1)
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
                    ### MANUAL MENU
                    conditionalPanel(
                      condition = "input.menuType == 'manual'",
                      textInput("fmodel", "fmodel :",value = ""),
                      textInput("qmodel", "qmodel (separate qmodels with &) :",value = ""),
                      textInput("srmodel", "srmodel :",value = "")
                    ),
                    actionButton("run","Run Assessment")
                    
                    
                  ),
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Inspect Input", 
                               br(),br(),
                               plotOutput("catches", height = 300, width = 800),
                               br(),br(),
                               selectInput("inspect_input", "Choose plot :",
                                           c("Catch at age" = "catch_age", 
                                             "Index at age" = "index_age",
                                             "Catch cohort consistency" = "cohort_catch",
                                             "Index cohort consistency" = "cohort_index"
                                           )
                               ),
                               br(),br(),
                               plotOutput("inspectPlot", height = 600, width = 800)),
                      tabPanel("Assessment", 
                               plotOutput("assessment", height = 600, width = 800),
                               br(),br(),
                               textOutput("AICBIC"),
                               br(),br(),
                               plotOutput("assessment_sim", height = 600, width = 800)),
                      tabPanel("Diagnostics",
                               br(),br(),
                               selectInput("diagnostics", "Choose diagnostics plot :",
                                           c("Aggregated catch diagnostics" = "catch", 
                                             "Standardized log residuals" = "log_residuals",
                                             "Bubble plot of log residuals" = "bubbles",
                                             "Fitted and observed catch" = "fitVScatch",
                                             "Fitted and observed index" = "fitVSindex"
                                           )
                               ),
                               br(),br(),
                               plotOutput("residuals", height = 600, width = 800)),
                      tabPanel("Submodels", 
                               actionButton(inputId="plot1","Plot F model"),
                               # Choose index to plot
                               selectInput("indexName", "Choose index to plot :"," "),
                               actionButton(inputId="plot2","Plot q model"),
                               plotlyOutput("plotF", height = 600, width = 600),
                               plotlyOutput("plotQ", height = 600, width = 600)),
                      tabPanel("Retrospective", 
                               plotOutput("retro"),
                               sliderInput("yrsback", label = "Years of retrospective", value = 3,
                                           min = 2, 
                                           max = 5,
                                           step = 1)),
                      tabPanel("Reference points", 
                               tableOutput("refpoints_tab"),
                               br(),br(),
                               plotOutput("refpoints_plot",  height = 600, width = 800)

                               )
                    )
                  )
                )
)

# Server
# Define server logic

server <- function(session, input, output) {

  
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
      return(as.formula(paste0("~s(year, k = ", input$fk, ")+factor(age)")))
    } 
    else if (input$fmodelType == 'factor') {
      return(as.formula("~factor(year)+s(age, k = ",input$fk_age, ")"))
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
    } 
    else if(input$srmodelType == 'factor'){
      return(as.formula("~factor(year)"))
    }
    else if(input$srmodelType == 'geomean'){
      return(as.formula(paste0("~geomean(CV = ", input$cv,")")))
    }
    else if(input$srmodelType == 'ricker'){
      return(as.formula(paste0("~ricker(CV = ", input$cv,")")))
    }
    else if(input$srmodelType == 'bevholt'){
      return(as.formula(paste0("~bevholt(CV = ", input$cv,")")))
    }
    else if(input$srmodelType == 'hockey'){
      return(as.formula(paste0("~hockey(CV = ", input$cv,")")))
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
  fit <- eventReactive(input$run,{
    if (input$menuType == 'dropdown'){
      return(sca(stk(),idx(), fmodel = fmod(), qmodel = qmod(), srmodel = srmod()))
    } else {
      return(sca(stk(),idx(), fmodel = fmod_2(), qmodel = qmod_2(), srmodel = srmod_2()))
    }
    
  })
  
  #####################################################
  # Plot input
  output$catches <- renderPlot({
    req(stk())
    df2 <- data.frame(year = range(stk())["minyear"]:range(stk())['maxyear'],
                      catch = as.numeric(catch(stk())),
                      landings = as.numeric(landings(stk())),
                      discards = as.numeric(discards(stk()))
    )
    df2 <- melt(df2,id.vars = 'year')
    ggplot(data = df2, aes(x = year, y = value, color = variable)) + geom_line()
  })
  
  output$inspectPlot <- renderPlot({
    if(input$inspect_input == "catch_age"){
      req(stk())
      df<-as.data.frame(catch.n(stk()))
      df<-na.omit(df)
      plot=ggplot(df,aes(x = age, y = data,color= as.factor(year)))+ geom_line(size = 1)+ggtitle("Catch at age")
      return(plot)
    }
    else if(input$inspect_input == "cohort_catch"){
      plotInternalConsistency(FLIndex(index = catch.n(stk())))
    }
    else if(input$inspect_input == "index_age"){
      req(idx())
      df<-as.data.frame(index(idx()[[1]]))
      df<-na.omit(df)
      plot=ggplot(df, aes(x = age, y = data,color= as.factor(year)))+geom_line(size = 1)+ggtitle("Index at age")
      return(plot)
    }
    else if(input$inspect_input == "cohort_index"){
      req(idx())
      plotInternalConsistency(idx()[[1]])
    }
  })

  # Plot the assessment results with fit()
  output$assessment <- renderPlot({
    stk.a4a <- stk() + fit()
    plot(stk.a4a)
  })
  
  # Print AIC and BIC
  output$AICBIC <- renderText({
    paste0("AIC : ", round(AIC(fit()),2),", BIC : ", round(BIC(fit()),2))
  })
  
  # Plot the assessment results with simulations
  output$assessment_sim <- renderPlot({
    stk.a4a.sim <- FLStocks(fit = stk() +simulate(fit(), 1000), catch = stk())
    plot(stk.a4a.sim)
  })
  
  ### DIAGNOSTICS TAB
  # Plot the residuals
  output$residuals <- renderPlot({
    req(fit())
    res_catch <- computeCatchDiagnostics(fit(),stk())
    res <- residuals(fit(), stk(), idx())
    if(input$diagnostics == 'catch'){
      plot(res_catch)
    }
    else if(input$diagnostics == 'log_residuals'){
      plot(res)
    }
    else if(input$diagnostics == 'bubbles'){
      bubbles(res)
    }
    else if(input$diagnostics == 'fitVScatch'){
      plot(fit(),stk())
    }
    else if(input$diagnostics == 'fitVSindex'){
      plot(fit(),idx())
    }
    
  })
  
  
  observeEvent(input$plot1,{
    output$plotF <- renderPlotly({
      stk.a4a <- stk() + fit()
      tmp = as.data.frame(harvest(stk.a4a))
      tmp = tmp[,c(1,2,7)]
      fig <- plot_ly(x = ~tmp$year, y = ~tmp$age, z = tmp$data, 
                     type = 'mesh3d',
                     colors = colorRamp(c("darkblue","green","yellow")),
                     intensity = ~tmp$data)
      fig <- fig %>% layout(scene = list(xaxis = list(title = 'year'),
                                  yaxis = list(title = 'age'),
                                  zaxis = list(title = 'harvest')))
      fig %>%
        layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
               paper_bgcolor = "rgba(0, 0, 0, 0)",
               fig_bgcolor   = "rgba(0, 0, 0, 0)",
               font = list(color = '#FFFFFF'))
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
      fig <- plot_ly(x = ~tmp$year, y = ~tmp$age, z = tmp$data, 
                     type = 'mesh3d',
                     colors = colorRamp(c("darkblue","green","yellow")),
                     intensity = ~tmp$data)
      fig <- fig %>% layout(scene = list(xaxis = list(title = 'year'),
                                         yaxis = list(title = 'age'),
                                         zaxis = list(title = 'Catchability')))
      
      fig %>%
        layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
               paper_bgcolor = "rgba(0, 0, 0, 0)",
               fig_bgcolor   = "rgba(0, 0, 0, 0)",
               font = list(color = '#FFFFFF'))
    })
  })
  
  output$retro <- renderPlot({
    plot(retro(stk(), idx(), fit(), retro = input$yrsback))
  })
  

  
  refpoints <- reactive({
    stk_df <- as.data.frame(fbar(stk()+simulate(fit(),500)), drop=TRUE)
    rp_<- as.data.frame(brp(FLBRP(stk()+simulate(fit(),500)))@refpts["f0.1"])
    
    rp_sub <- rp_[which(rp_$quant=="harvest"),]
    final <- merge(stk_df,rp_sub,by=c("iter"),all=T)
    final2 <- final[which(final$year==as.numeric(stk()@range['maxyear'])),]
    colnames(final2) <- c("iter","year","fbar","refp","quant","f0.1")
    final2$FcurrF0.1 <- final2$fbar/final2$f0.1
    
    final3 <- final2[,c('iter','fbar','quant','f0.1','FcurrF0.1')]
    x <- reshape2::melt(final3)
    return(x)
  })
  
  refpts_table <- reactive({
    req(stk(),fit())
    ref_pts <- brp(FLBRP(stk()+fit()))
    x <- data.frame(F01 = as.numeric(ref_pts@refpts['f0.1','harvest']),
                    Fcurrent = as.numeric(fbar(stk()+fit())[,as.character(range(stk())['maxyear'])]),
                    Fcurrent_F01 = as.numeric(fbar(stk()+fit())[,as.character(range(stk())['maxyear'])])/ as.numeric(ref_pts@refpts['f0.1','harvest'])
                    )
    x
  })
  
  output$refpoints_tab <- renderTable({
    refpts_table()
  })

  output$refpoints_plot <- renderPlot({
    p=ggplot(data = refpoints(), aes(x = value,color = variable,fill = variable)) +
      geom_density(alpha = 0.6)+
      facet_wrap(~variable, scales = 'free', ncol = 1)
    return(p)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)