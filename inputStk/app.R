library(shiny)
library(FLCore)
library(FLa4a)
library(ggplotFL); theme_set(theme_bw())

# Define two datasets and store them to disk

# Define UI
ui <- shinyUI(fluidPage(
  titlePanel(".RData File Upload Test"),
  mainPanel(
    fileInput("file", label = "Stock object"),
    fileInput("file2", label = "Index object"),
    actionButton(inputId="plot1","Plot Stock Object"),
    actionButton(inputId="plot2","Plot Cohort Consistency in Index"),
    plotOutput("plot_stk"))
  
)
)

# Define server logic
server <- shinyServer(function(input, output) {
  
  observeEvent(input$plot1,{
    if ( is.null(input$file)) return(NULL)
    inFile <- input$file
    file <- inFile$datapath
    # load the file into new environment and get it from there
    e = new.env()
    name <- load(file, envir = e)
    data <- e[[name]]
    
    # Plot the data
    output$plot_stk <- renderPlot({
      plot(data)
    })
  })
  
  observeEvent(input$plot2,{
    if ( is.null(input$file2)) return(NULL)
    inFile <- input$file2
    file2 <- inFile$datapath
    # load the file into new environment and get it from there
    e = new.env()
    name <- load(file2, envir = e)
    data <- e[[name]]
    
    # Plot the data
    output$plot_stk <- renderPlot({
      plot(data, type = 'internal')
    })
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)