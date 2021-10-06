#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "darkly"),
    # App title ----
    titlePanel("Create a stock object"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            # Landings input
            h5("General inputs for stock object:"),
            # Input: Choose dataset ----
            textInput("minyear", "Starting year:", value = 2000),
            textInput("maxyear", "Ending year:", value = 2020),
            textInput("minage", "Minimun age:", value = 0),
            textInput("maxage", "Maximum age:", value = 7),
            # Button
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            h3("Build a stock object: "),
            br(),
            h4("Download a template csv for each of the following needed values, fill in with your own data and then upload them again in the next section"),
            br(),
            "Landings:",
            br(),
            downloadButton("landings", "Download Landings template"),
            tableOutput("landingsTable"),
            br(),
            "Landings at age:",
            br(),
            downloadButton("landingsAtAge", "Download Landings at age template"),
            tableOutput("landingsAtAgeTable"),
            br(),
            "Landings individual weight at age:",
            br(),
            downloadButton("landingsWeight", "Download Landings individual weight template"),
            tableOutput("landingsWeightTable"),
            br(),br(),
            br(),
            ### ================================================================================== ###
            ### ================================================================================== ###
            "Discards:",
            br(),
            downloadButton("discards", "Download Discards template"),
            tableOutput("discardsTable"),
            br(),
            "Discards at age:",
            br(),
            downloadButton("discardsAtAge", "Download Discards at age template"),
            tableOutput("discardsAtAgeTable"),
            br(),
            "Discards individual weight at age:",
            br(),
            downloadButton("discardsWeight", "Download Discards individual weight template"),
            tableOutput("discardsWeightTable")
            
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    ### ================================================================================== ###
    ### ================================================================================== ###
    ## Build landings template
    # Construct landings csv
    landingsTemplate <- reactive({
        data.frame(year = input$minyear:input$maxyear, landings = "", units = "tonnes")
    })
    # Construct landings at age csv
    landingsAtAgeTemplate <- reactive({
        as.data.frame(expand.grid(year = input$minyear:input$maxyear, age = input$minage:input$maxage,landings = "", units = "1000"))
    })
    # Construct landings at age csv
    landingsWeightTemplate <- reactive({
        as.data.frame(expand.grid(year = input$minyear:input$maxyear, age = input$minage:input$maxage,weight = "", units = "kg"))
    })
    
    ### CSV outputs
    output$landingsTable <- renderTable({
        head(landingsTemplate())
    })
    output$landingsAtAgeTable <- renderTable({
        head(landingsAtAgeTemplate())
    })
    output$landingsWeightTable <- renderTable({
        head(landingsWeightTemplate())
    })
    

    # Downloadable csv of landings
    output$landings <- downloadHandler(
        filename = function() {
            paste("landings.csv", sep = "")
        },
        content = function(file) {
            write.csv(landingsTemplate(), file, row.names = FALSE)
        }
    )
    
    # Downloadable csv of landings at age
    output$landingsAtAge <- downloadHandler(
        filename = function() {
            paste("landingsAtAge.csv", sep = "")
        },
        content = function(file) {
            write.csv(landingsAtAgeTemplate(), file, row.names = FALSE)
        }
    )
    
    # Downloadable csv of landings at age
    output$landingsWeight <- downloadHandler(
        filename = function() {
            paste("landingsWeight.csv", sep = "")
        },
        content = function(file) {
            write.csv(landingsWeightTemplate(), file, row.names = FALSE)
        }
    )
    ### ================================================================================== ###
    ### ================================================================================== ###
    ## Build discards template
    # Construct discards csv
    discardsTemplate <- reactive({
        data.frame(year = input$minyear:input$maxyear, discards = "", units = "tonnes")
    })
    # Construct discards at age csv
    discardsAtAgeTemplate <- reactive({
        as.data.frame(expand.grid(year = input$minyear:input$maxyear, age = input$minage:input$maxage,discards = "", units = "1000"))
    })
    # Construct discards at age csv
    discardsWeightTemplate <- reactive({
        as.data.frame(expand.grid(year = input$minyear:input$maxyear, age = input$minage:input$maxage,weight = "", units = "kg"))
    })
    
    ### CSV outputs
    output$discardsTable <- renderTable({
        head(discardsTemplate())
    })
    output$discardsAtAgeTable <- renderTable({
        head(discardsAtAgeTemplate())
    })
    output$discardsWeightTable <- renderTable({
        head(discardsWeightTemplate())
    })
    
    
    # Downloadable csv of discards
    output$discards <- downloadHandler(
        filename = function() {
            paste("discards.csv", sep = "")
        },
        content = function(file) {
            write.csv(discardsTemplate(), file, row.names = FALSE)
        }
    )
    
    # Downloadable csv of discards at age
    output$discardsAtAge <- downloadHandler(
        filename = function() {
            paste("discardsAtAge.csv", sep = "")
        },
        content = function(file) {
            write.csv(discardsAtAgeTemplate(), file, row.names = FALSE)
        }
    )
    
    # Downloadable csv of discards at age
    output$discardsWeight <- downloadHandler(
        filename = function() {
            paste("discardsWeight.csv", sep = "")
        },
        content = function(file) {
            write.csv(discardsWeightTemplate(), file, row.names = FALSE)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
