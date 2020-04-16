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


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),
                
                # Application title
                titlePanel("Stock assessment"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
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
                                min = 3, max = 30, step = 1)
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
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Assessment", plotOutput("assessment")),
                      tabPanel("Diagnostics",
                               h3("Log Residuals"),
                               plotOutput("residuals"),
                               br(),
                               h3("Fit VS Original values"),
                               plotOutput("fitVSstk")),
                      tabPanel("Submodels", 
                               actionButton(inputId="plot1","Plot F model"),
                               actionButton(inputId="plot2","Plot q model"),
                               plotlyOutput("plotF"),
                               plotlyOutput("plotQ"))
                    )
                  )
                )
)

# Define server logic

server <- function(input, output, session) {
  data("ple4")
  data("ple4.index")
  
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
  
  # Plot the assessment results
  output$assessment <- renderPlot({
    ple4.a4a <- ple4 + sca(ple4,ple4.index, fmodel = fmod(), qmodel = qmod(), srmodel = srmod())
    plot(ple4.a4a)
  })
  
  ### DIAGNOSTICS TAB
  # Plot the residuals
  output$residuals <- renderPlot({
    fit <- sca(ple4,ple4.index, fmodel = fmod(), qmodel = qmod(), srmodel = srmod())
    res <- residuals(fit, ple4, ple4.index)
    plot(res)
  })
  
  output$fitVSstk <- renderPlot({
    fit <- sca(ple4,ple4.index, fmodel = fmod(), qmodel = qmod(), srmodel = srmod())
    plot(fit, ple4)
  })
  
  observeEvent(input$plot1,{
    output$plotF <- renderPlotly({
      ple4.a4a <- ple4 + sca(ple4,ple4.index, fmodel = fmod(), qmodel = qmod(), srmodel = srmod())
      tmp = as.data.frame(harvest(ple4.a4a))
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
  
  observeEvent(input$plot2,{
    output$plotQ <- renderPlotly({
      fit <- sca(ple4,ple4.index, fmodel = fmod(), 
                 qmodel = qmod(), srmodel = srmod())
      sfrac <- mean(range(ple4.index)[c("startf", "endf")])
      Z <- (m(ple4) + harvest(fit))*sfrac # check M * sfrac
      lst <- dimnames(fit@index[[1]])
      lst$x <- stock.n(fit)*exp(-Z)
      stkn <- do.call("trim", lst)
      tmp = as.data.frame(index(fit)[[1]]/stkn)
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