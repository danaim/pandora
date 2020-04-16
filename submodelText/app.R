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
    titlePanel("Submodel text input for a4a"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("fmodel", "fmodel :", "~factor(year)"),
            textInput("qmodel", "qmodel :", "~factor(age)"),
            textInput("srmodel", "srmodel :", "~factor(year)")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Assessment",
                         actionButton(inputId = "run", "Run assessment"),
                         plotOutput("assessment")),
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

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    fmod <- reactive({
        return(as.formula(input$fmodel))
    })
    qmod <- reactive({
        return(list(as.formula(input$qmodel)))
    })
    srmod <- reactive({
        return(as.formula(input$srmodel))
    })

    # Plot the assessment results
    observeEvent(input$run,{
        output$assessment <- renderPlot({
            ple4.a4a <- ple4 + sca(ple4,ple4.index, fmodel = fmod(), qmodel = qmod(), srmodel = srmod())
            plot(ple4.a4a)
        })
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
