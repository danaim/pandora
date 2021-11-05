server <- function( input, output, session) {
  
  
  #SPiCT code
  file_name <- reactive({
    inFile  <- input$catchindex
    if (is.null(inFile))
      return(NULL) else return (tools::file_path_sans_ext(inFile$name))
  })     
  
  file_col <- reactive({
    inFile <- input$catchindex
    if (is.null(inFile)){
      return(NULL)
    }
    else{
      tmp <- read.delim(inFile$datapath,header=TRUE)
      return ((ncol(tmp)))
    }
  }) 
  
  
  data <- reactive({
    inFile <- input$catchindex
    req(input$catchindex)
    if (is.null(inFile))
      return(NULL)
    d <- read.delim(inFile$datapath,header=TRUE)
    print(ncol(d))
    inp <-NULL
    inp <- list(timeC = d$timeC, obsC = d$obsC)
    i=1
    if (i <= (ncol(d)/2-1)){
      for (j in seq(1,  ncol(d)-2, by=2)){
        inp$timeI[[i]] <- d[,j+2]
        inp$obsI[[i]]  <- d[,j+3]
        i=i+1
      }}
    return(inp)
    print(inp)
  })
  
  
  output$sliders <- renderUI({
    pvars  <- file_col()/2-1
    myList <-list()
    lapply(seq(pvars), function(i) {
      sliderInput(inputId = paste0("Shiftindex", i),label = paste0("Shiftindex", i), min = 0, max = 1,value= 0,step = 0.1)
    })
  })
  
  
  #Shift index   
  data1 <- reactive({
    if (input$plotoption == 'advanced') {
      d <- data() 
      return(d)}
    else if (input$plotoption == 'simple'){
      d <- data() 
      return(d)}
    else if (input$plotoption == 'shifted'){
      if (file_col()==4){
        inpshift <- data()
        inpshift$timeC <- inpshift$timeC + input$shiftcatch
        inpshift$timeI[[1]] <- inpshift$timeI[[1]] + input$shiftindex
        return(inpshift) 
      }
      if (file_col()>4){
        i=1
        inpshift <- data()
        inpshift$timeC <- inpshift$timeC + input$shiftcatch1
        for ( i in 1: (file_col()/2-1)){
          inpshift$timeI[[i]] <- inpshift$timeI[[i]] + input[[paste0("Shiftindex", i)]]
          i=i+1}
        print(inpshift)
        return(inpshift)}
    }
  })
  
  output$text <- renderText({
    pvars         <- file_col()/2-1   
    slider_inputs <- paste0("Shiftindex", seq(pvars))   
    values        <- purrr::map_dbl(slider_inputs, function(x) input[[x]])   
    paste(values, collapse = ", ") 
  })
  
  output$myFileColrow <- renderText({ file_col() })
  output$indices <- renderText({ 
    if (((file_col())>0)==TRUE){return(((file_col())/2-1))}
  })
  
  output$seasonalno <- renderText({ 
    d <- data()
    if (is.null(data())){return("Not found")}
    if (is.integer(d$timeC[[1]])==FALSE){return("Seasonal data")}
    if (is.integer(d$timeC[[1]])==TRUE){return("No seasonal data")}
  })
  
  
  output$col <- renderText({ 
    if(is.null(file_col())==TRUE){return("")}
    if(((file_col())>4)==TRUE){return("Many indices")}
    if(((file_col())<=4)==TRUE){return("One index")}
    else {return("Not found")}
  })
  
  
  #Plot of data: simple and advanced and shifted data
  plotting <- reactive ({
    if (is.null(input$plotoption)){return(null)}
    else if (input$plotoption == 'advanced') {plot1 = plotspict.ci(data1())}
    else if (input$plotoption == 'simple'){plot2 = plotspict.data(data1())}
    else if (input$plotoption == 'shifted'){
      inpshift <- data1()
      plotspict.data(inpshift)}
  }) 
  
  #downloadHandler to save plots of data in png file
  output$downloadPlotData <- downloadHandler(
    filename = function(){  paste(input$plotoption, ".png", sep="")},
    content = function(file) {
      png(file)
      if(input$plotoption == 'advanced') {print(plotspict.ci(data1()))}
      else if (input$plotoption == 'simple'){print(plotspict.data(data1()))}
      else if (input$plotoption == 'shifted'){print(plotspict.data(data1()))}
      dev.off()
    })
  
  
######renderUI for model original####
  output$Scaefer <- renderUI({
    checkboxInput("Schaefer", label = "Fix the production curve to Schaefer model", value=FALSE)
  })
  
  output$checkboxab <- renderUI({
    checkboxInput("checkboxab", label = "Disable the default alpha and beta priors and use priors on sdi, sdc, sdb",value=FALSE)
  })
  
  #conditionalInput for original model
  output$priorgrowthm <- renderUI({
    numericInput("mg", "Mean-r:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "mg", title = "Prior for the intrinsic growth rate (mean)",placement="right",trigger = "hover")
  })
  
  output$priorgrowthsd <- renderUI({
    numericInput("sdg", "Sd:-r", NULL , min = 0, max = 1000000)
    #bsPopover(id = "sdg", title = "Prior for the intrinsic growth rate (standard deviation)",placement="right",trigger = "hover")
  })
  
  output$priormeanm <- renderUI({
    numericInput("mm", "Mean-MSY:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "mm", title = "Prior for the MSY (mean)",placement="right",trigger = "hover")
  })
  
  output$priormeansd <- renderUI({
    numericInput("sdm", "Sd-MSY:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "sdm", title = "Prior for the MSY (standard deviation)",placement="right",trigger = "hover")
  })
  
  output$priorcapacitym <- renderUI({
    numericInput("mc", "Mean-Capacity:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "mc", title = "Prior for the carrying capacity (mean)",placement="right",trigger = "hover")
  })
  
  output$priorcapacitysd <- renderUI({
    numericInput("sdc", "Sd-Capacity:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "sdc", title = "Prior for the carrying capacity (standard deviation)",placement="right",trigger = "hover")
  })
  
  output$priorbiomassm<- renderUI({
    numericInput("mb", "Mean-Biomass:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "mb", title = "Prior for the biomass (mean)",placement="right",trigger = "hover")
  })

  output$priorbiomasssd<- renderUI({
    numericInput("sdb", "Sd-Biomass:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "sdb", title = "Prior for the biomass (standard deviation)",placement="right",trigger = "hover")
  })
  
  output$priorfishm<- renderUI({
    numericInput("mf", "Mean-F:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "mf", title = "Prior for the fishing mortality (mean)",placement="right",trigger = "hover")
  })
  
  output$priorfishsd<- renderUI({
    numericInput("sdf", "Sd-F:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "sdf", title = "Prior for the fishing mortality (standard deviation)",placement="right",trigger = "hover")
  })
  
  
  output$priorBBmsym<- renderUI({
    numericInput("mbb", "Mean-BBmsy:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "mbb", title = "Prior for the relative biomass (B/Bmsy) (mean)",placement="right",trigger = "hover")
  })
  
  output$priorBBmsysd<- renderUI({
    numericInput("sdbb", "Sd-BBmsy:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "sdbb", title = "Prior for the relative biomass (B/Bmsy) (Standard deviation)",placement="right",trigger = "hover")
  })
  
  output$priorFFmsym<- renderUI({
    numericInput("mff", "Mean-FFmsy:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "mff", title = "Prior for the relative fishing mortality (F/Fmsy) (mean)",placement="right",trigger = "hover")
  })
  
  output$priorFFmsysd<- renderUI({
    numericInput("sdff", "Sd-FFmsy:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "sdff", title = "Prior for the relative fishing mortality (F/Fmsy) (standard deviation)",placement="right",trigger = "hover")
  })
  
  output$priorbkfracm<- renderUI({
    numericInput("mbk", "Mean-bkfrac:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "mbk", title = "Prior for the initial biomass fraction (B/K) (mean)",placement="right",trigger = "hover")
  })
  
  output$priorbkfracsd<- renderUI({
    numericInput("sdbk", "Sd-bkfrac:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "sdbk", title = "Prior for the initial biomass fraction (B/K) (standard deviation)",placement="right",trigger = "hover")
  })
  
  
  ####renderUI for model regime####
  output$Scaefer1 <- renderUI({
    checkboxInput("Schaefer1", label = "Fix the production curve to Schaefer model", value=FALSE)
  })
  
  output$checkboxab1 <- renderUI({
    checkboxInput("checkboxab1", label = "Disable the default alpha and beta priors and use priors on sdi, sdc, sdb",value=FALSE)
  })
  
  output$prior1growthm <- renderUI({
    numericInput("mg1", "Mean-r:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "mg1", title = "Prior for the intrinsic growth rate (mean)",placement="right",trigger = "hover")
  })
  
  output$prior1growthsd <- renderUI({
    numericInput("sdg1", "Sd:-r", NULL , min = 0, max = 1000000)
    #bsPopover(id = "sdg1", title = "Prior for the intrinsic growth rate (standard deviation)",placement="right",trigger = "hover"))
  })
  
  output$prior1meanm <- renderUI({
    numericInput("mm1", "Mean-MSY:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "mm1", title = "Prior for the MSY (mean)",placement="right",trigger = "hover")
  })
  
  output$prior1meansd <- renderUI({
    numericInput("sdm1", "Sd-MSY:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "sdm1", title = "Prior for the MSY (standard deviation)",placement="right",trigger = "hover")
  })
  
  output$prior1capacitym <- renderUI({
    numericInput("mc1", "Mean-Capacity:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "mc1", title = "Prior for the carrying capacity (mean)",placement="right",trigger = "hover")
  })
  
  output$prior1capacitysd <- renderUI({
    numericInput("sdc1", "Sd-Capacity:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "sdc1", title = "Prior for the carrying capacity (standard deviation)",placement="right",trigger = "hover"))
  })
  
  output$prior1biomassm<- renderUI({
    numericInput("mb1", "Mean-Biomass:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "mb1", title = "Prior for the biomass (mean)",placement="right",trigger = "hover")
  })
  
  output$prior1biomasssd<- renderUI({
    numericInput("sdb1", "Sd-Biomass:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "sdb1", title = "Prior for the biomass (standard deviation)",placement="right",trigger = "hover")) 
  })
  
  output$prior1fishm<- renderUI({
    numericInput("mf1", "Mean-Fish:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "mf1", title = "Prior for the fishing mortality (mean)",placement="right",trigger = "hover"),
  })

  output$prior1fishsd<- renderUI({
    numericInput("sdf1", "Sd-Fish:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "sdf1", title = "Prior for the fishing mortality (standard deviation)",placement="right",trigger = "hover"),
  })
  
  output$prior1BBmsym<- renderUI({
    numericInput("mbb1", "Mean-BBmsy:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "mbb1", title = "Prior for the relative biomass (B/Bmsy) (mean)",placement="right",trigger = "hover")
  })
  
  output$prior1BBmsysd<- renderUI({
    numericInput("sdbb1", "Sd-BBmsy:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "sdbb1", title = "Prior for the relative biomass (B/Bmsy) (standard deviation)",placement="right",trigger = "hover")
  })
  
  output$prior1FFmsym<- renderUI({
    numericInput("mff1", "Mean-FFmsy:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "mff1", title = "Prior for the relative fishing mortality (F/Fmsy) (mean)",placement="right",trigger = "hover")
  })
  
  output$prior1FFmsysd<- renderUI({
    numericInput("sdff1", "Sd-FFmsy:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "sdff1", title = "Prior for the relative fishing mortality (F/Fmsy) (standard deviation)",placement="right",trigger = "hover")
  })
  
  output$prior1bkfracm<- renderUI({
    numericInput("mbk1", "Mean-bkfrac:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "mbk1", title = "Prior for the initial biomass fraction (B/K) (mean)",placement="right",trigger = "hover"),
  })
  
  output$prior1bkfracsd<- renderUI({
    numericInput("sdbk1", "Sd-bkfrac:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "sdbk1", title = "Prior for the initial biomass fraction (B/K) (standard deviation)",placement="right",trigger = "hover"))
  })
  
  ###renderUI for gradual model#### 
  output$Scaefer2 <- renderUI({
    checkboxInput("Schaefer2", label = "Fix the production curve to Schaefer model", value=FALSE)
  })
  
  output$checkboxab2 <- renderUI({
    checkboxInput("checkboxab2", label = "Disable the default alpha and beta priors and use priors on sdi, sdc, sdb",value=FALSE)
  })
  
  output$prior2growthm <- renderUI({
    numericInput("mg2", "Mean-r:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "mg2", title = "Prior for the intrinsic growth rate (mean)",placement="right",trigger = "hover")
  })
  
 output$prior2growthsd <- renderUI({
    numericInput("sdg2", "Sd:-r", NULL , min = 0, max = 1000000)
    #bsPopover(id = "sdg2", title = "Prior for the intrinsic growth rate (standard deviation)",placement="right",trigger = "hover"))
  })
  
  
 output$prior2meanm <- renderUI({
    numericInput("mm2", "Mean-MSY:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "mm2", title = "Prior for the MSY (mean)",placement="right",trigger = "hover")
  })
  
  output$prior2meansd <- renderUI({
    numericInput("sdm2", "Sd-MSY:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "sdm2", title = "Prior for the MSY (standard deviation)",placement="right",trigger = "hover")
  })
  
  output$prior2capacitym <- renderUI({
    numericInput("mc2", "Mean-Capacity:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "mc2", title = "Prior for the carrying capacity (mean)",placement="right",trigger = "hover")
  })
  
  output$prior2capacitysd <- renderUI({
    numericInput("sdc2", "Sd-Capacity:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "sdc2", title = "Prior for the carrying capacity (standard deviation)",placement="right",trigger = "hover"))
  })
  
  output$prior2biomassm<- renderUI({
    numericInput("mb2", "Mean-Biomass:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "mb2", title = "Prior for the biomass (mean)",placement="right",trigger = "hover")
  })
  
  output$prior2biomasssd<- renderUI({
    numericInput("sdb2", "Sd-Biomass:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "sdb2", title = "Prior for the biomass (standard deviation)",placement="right",trigger = "hover")) 
  })
  
  output$prior2fishm<- renderUI({
    numericInput("mf2", "Mean-Fish:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "mf2", title = "Prior for the fishing mortality (mean)",placement="right",trigger = "hover"),
  })

  output$prior2fishsd<- renderUI({
    numericInput("sdf2", "Sd-Fish:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "sdf2", title = "Prior for the fishing mortality (standard deviation)",placement="right",trigger = "hover"),
  })
  
  output$prior2BBmsym<- renderUI({
    numericInput("mbb2", "Mean-BBmsy:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "mbb2", title = "Prior for the relative biomass (B/Bmsy) (mean)",placement="right",trigger = "hover")
  })
  
  output$prior2BBmsysd<- renderUI({
    numericInput("sdbb2", "Sd-BBmsy:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "sdbb1", title = "Prior for the relative biomass (B/Bmsy) (standard deviation)",placement="right",trigger = "hover")
  })
  
  output$prior2FFmsym<- renderUI({
    numericInput("mff2", "Mean-FFmsy:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "mff1", title = "Prior for the relative fishing mortality (F/Fmsy) (mean)",placement="right",trigger = "hover")
  })
  
  output$prior2FFmsysd<- renderUI({
    numericInput("sdff2", "Sd-FFmsy:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "sdff2", title = "Prior for the relative fishing mortality (F/Fmsy) (standard deviation)",placement="right",trigger = "hover")
  })
  
  output$prior2bkfracm<- renderUI({
    numericInput("mbk2", "Mean-bkfrac:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "mbk2", title = "Initial biomass fraction (B/K) (mean)",placement="right",trigger = "hover")
  })
  
  output$prior2bkfracsd<- renderUI({
    numericInput("sdbk2", "Sd-bkfrac:", NULL, min = 0, max = 1000000)
    #bsPopover(id = "sdbk2", title = "Initial biomass fraction (B/K) (standard deviation)",placement="right",trigger = "hover"))
  })
  
  ###renderUI for model seasonal####
  
  
  ####Fit the model####
  model1 <- reactive ({
    
    
    if (input$selectmodel == 'original'){
      
      inp <- data1()
      x   <- input$priors
      
      if ('growth' %in% x)  {inp$priors$logr      <- c(log(input$mg^2/sqrt(input$mg^2 + input$sdg^2)), sqrt(log(input$sdg^2/input$mg^2 + 1)), 1)}
      if ('MSY' %in% x)    {inp$priors$logm      <- c(log(input$mm^2/sqrt(input$mm^2 + input$sdm^2)), sqrt(log(input$sdm^2/input$mm^2 + 1)), 1)}
      if ('capacity' %in% x){inp$priors$logK      <- c(log(input$mc^2/sqrt(input$mc^2 + input$sdc^2)), sqrt(log(input$sdc^2/input$mc^2 + 1)), 1)}
      if ('biomass'%in% x)  {inp$priors$logB      <- c(log(input$mb^2/sqrt(input$mb^2 + input$sdb^2)), sqrt(log(input$sdb^2/input$mb^2 + 1)), 1, input$slider1)}
      if ('bkfrac'%in% x)   {inp$priors$logbkfrac <- c(log(input$mbk^2/sqrt(input$mbk^2 + input$sdbk^2)), sqrt(log(input$sdbk^2/input$mbk^2 + 1)), 1)}
      if ('BBmsy'%in% x)    {inp$priors$logBBmsy  <- c(log(input$mbb^2/sqrt(input$mbb^2 + input$sdbb^2)), sqrt(log(input$sdbb^2/input$mbb^2 + 1)),  1, input$slider3)}
      if ('FFmsy'%in% x)    {inp$priors$logFFmsy  <- c(log(input$mff^2/sqrt(input$mff^2 + input$sdff^2)), sqrt(log(input$sdff^2/input$mff^2 + 1)), 1, input$slider4)}
      if ('F'%in% x)        {inp$priors$logF      <- c(log(input$mf^2/sqrt(input$mf^2 + input$sdf^2)), sqrt(log(input$sdf^2/input$mf^2 + 1)), 1, input$slider2)}
      
      if(input$checkboxab){
        inp$priors$logalpha <- c(0,0,0)
        inp$priors$logbeta  <- c(0,0,0)
        if (((file_col())<=4)==TRUE){
          inp$priors$logsdi   <- list( c(log(0.4),0.5,1) )
        } else if (((file_col())>4)==TRUE) {
          inp$priors$logsdi   <- list( c(log(0.4),0.5,1), c( log(0.4),0.5,1) )
        } 
        inp$priors$logsdc   <- c( log(0.1),0.5,1)
        inp$priors$logsdb   <- c(log(0.1),0.5,1)
      }
      
      if(input$Schaefer){
        inp$phases$logn <- -1
        inp$ini$logn    <- log(2)
        
      }
      
      if(input$checkboxseasontype == TRUE){
        inp$seasontype<-input$seasontype
      }
      
      if(input$checkboxorder == TRUE){
        inp$splineorder  <- input$orderspline
      }
      
      inp <- check.inp(inp)
      p   <- fit.spict(inp)
      return(p)
    }
    
    else if (input$selectmodel == 'regime'){
      
      inp <- data1()
      x   <- input$priors1
      
      if ('growth' %in% x)  {inp$priors$logr      <- c(log(input$mg1^2/sqrt(input$mg1^2 + input$sdg1^2)), sqrt(log(input$sdg1^2/input$mg1^2 + 1)), 1)}
      if ('MSY' %in% x)    {inp$priors$logm      <- c(log(input$mm1^2/sqrt(input$mm1^2 + input$sdm1^2)), sqrt(log(input$sdm1^2/input$mm1^2 + 1)), 1)}
      if ('capacity' %in% x){inp$priors$logK      <- c(log(input$mc1^2/sqrt(input$mc1^2 + input$sdc1^2)), sqrt(log(input$sdc1^2/input$mc1^2 + 1)), 1)}
      if ('biomass'%in% x)  {inp$priors$logB      <- c(log(input$mb1^2/sqrt(input$mb1^2 + input$sdb1^2)), sqrt(log(input$sdb1^2/input$mb1^2 + 1)), 1, input$slider11)}
      if ('bkfrac'%in% x)   {inp$priors$logbkfrac <- c(log(input$mbk1^2/sqrt(input$mbk1^2 + input$sdbk1^2)), sqrt(log(input$sdbk1^2/input$mbk1^2 + 1)), 1)}
      if ('BBmsy'%in% x)    {inp$priors$logBBmsy  <- c(log(input$mbb1^2/sqrt(input$mbb1^2 + input$sdbb1^2)), sqrt(log(input$sdbb1^2/input$mbb1^2 + 1)),  1, input$slider13)}
      if ('FFmsy'%in% x)    {inp$priors$logFFmsy  <- c(log(input$mff1^2/sqrt(input$mff1^2 + input$sdff1^2)), sqrt(log(input$sdff1^2/input$mff1^2 + 1)), 1, input$slider14)}
      if ('F'%in% x)        {inp$priors$logF      <- c(log(input$mf1^2/sqrt(input$mf1^2 + input$sdf1^2)), sqrt(log(input$sdf1^2/input$mf1^2 + 1)), 1, input$slider12)}
      
      
      if(input$checkboxab1 == TRUE){
        inp$priors$logalpha <- c(0,0,0)
        inp$priors$logbeta  <- c(0,0,0)
        if (((file_col())<=4)==TRUE){
          inp$priors$logsdi   <- list( c(log(0.4),0.5,1) )
        } else if (((file_col())>4)==TRUE) {
          inp$priors$logsdi   <- list( c(log(0.4),0.5,1), c( log(0.4),0.5,1) )
        } 
        inp$priors$logsdc   <- c( log(0.1),0.5,1)
        inp$priors$logsdb   <- c(log(0.1),0.5,1)
      }
      
      if(input$Schaefer1 == TRUE){
        inp$phases$logn <- -1
        inp$ini$logn    <- log(2)
      }
      
      if(input$checkboxseasontype1 == TRUE){
        inp$seasontype <- input$seasontype1
      }

      if(input$checkboxorder1 == TRUE){
        inp$splineorder  <- input$orderspline1
      }
      
      inp           <- check.inp(inp)
      lengthMSY     <- length(inp$MSYregime)
      dteuler       <- inp$dteuler
      shiftyear     <- input$shift - floor(inp$timeC[1]) 
      breakpoint    <- length(seq(0, shiftyear, dteuler))
      inp$MSYregime <- as.factor(c(rep(1, breakpoint), rep(2,lengthMSY - breakpoint)))
      
      p <- fit.spict(inp)
      return(p)
    }
    
    
    else if (input$selectmodel == 'gradual'){
      
      inp <- data1()
      inp$timevaryinggrowth <- T
      x   <- input$priors2
  
      if ('growth' %in% x)  {inp$priors$logr      <- c(log(input$mg2^2/sqrt(input$mg2^2 + input$sdg2^2)), sqrt(log(input$sdg2^2/input$mg2^2 + 1)), 1)}
      if ('MSY' %in% x)    {inp$priors$logm      <- c(log(input$mm2^2/sqrt(input$mm2^2 + input$sdm2^2)), sqrt(log(input$sdm2^2/input$mm2^2 + 1)), 1)}
      if ('capacity' %in% x){inp$priors$logK      <- c(log(input$mc2^2/sqrt(input$mc2^2 + input$sdc2^2)), sqrt(log(input$sdc2^2/input$mc2^2 + 1)), 1)}
      if ('biomass'%in% x)  {inp$priors$logB      <- c(log(input$mb2^2/sqrt(input$mb2^2 + input$sdb2^2)), sqrt(log(input$sdb2^2/input$mb2^2 + 1)), 1, input$slider17)}
      if ('bkfrac'%in% x)   {inp$priors$logbkfrac <- c(log(input$mbk2^2/sqrt(input$mbk2^2 + input$sdbk2^2)), sqrt(log(input$sdbk2^2/input$mbk2^2 + 1)), 1)}
      if ('BBmsy'%in% x)    {inp$priors$logBBmsy  <- c(log(input$mbb2^2/sqrt(input$mbb2^2 + input$sdbb2^2)), sqrt(log(input$sdbb2^2/input$mbb2^2 + 1)),  1, input$slider19)}
      if ('FFmsy'%in% x)    {inp$priors$logFFmsy  <- c(log(input$mff2^2/sqrt(input$mff2^2 + input$sdff2^2)), sqrt(log(input$sdff2^2/input$mff2^2 + 1)), 1, input$slider20)}
      if ('F'%in% x)        {inp$priors$logF      <- c(log(input$mf2^2/sqrt(input$mf2^2 + input$sdf2^2)), sqrt(log(input$sdf2^2/input$mf2^2 + 1)), 1, input$slider18)}
      
      inp$ini$logpsi    <- -3
      inp$ini$logsdm    <- -3
      inp$priors$logsdm <- c(log(0.05),1,1)
      inp$priors$logpsi <- c(log(0.05),1,1)
      
      if(input$checkboxab2 == TRUE){
        inp$priors$logalpha <- c(0,0,0)
        inp$priors$logbeta  <- c(0,0,0)
        if (((file_col())<=4)==TRUE){
          inp$priors$logsdi   <- list( c(log(0.4),0.5,1) )
        } else if (((file_col())>4)==TRUE) {
          inp$priors$logsdi   <- list( c(log(0.4),0.5,1), c( log(0.4),0.5,1) )
        } 
        inp$priors$logsdc   <- c( log(0.1),0.5,1)
        inp$priors$logsdb   <- c(log(0.1),0.5,1)
      }
      
      if(input$Schaefer2 == TRUE){
        inp$phases$logn <- -1
        inp$ini$logn    <- log(2)
      }
      
      if(input$checkboxseasontype2 == TRUE){
        inp$seasontype<-input$seasontype2
      }
      
      if(input$checkboxorder2 == TRUE){
        inp$splineorder  <- input$orderspline2
      }
  
      inp <- check.inp(inp)
      p   <- fit.spict(inp)
      
      return(p)
    }
  
  }) 

######Sliders####
  model2 <- reactive ({
    res <- calc.osa.resid(model1())
  }) 
  
  output$slider1 = renderUI({
    minZ <- min(data1()$timeC)
    maxZ <- max(data1()$timeC)
    sliderInput("slider1", h6("Year to which the prior should be applied:"), min = minZ, max = maxZ, value = minZ, step = 1)
  })
  
  output$slider2 = renderUI({
    minZ <- min(data1()$timeC)
    maxZ <- max(data1()$timeC)
    sliderInput("slider2", h6("Year to which the prior should be applied:"), min = minZ, max = maxZ, value = minZ, step = 1)
  })
  
  output$slider3 = renderUI({
    minZ <- min(data1()$timeC)
    maxZ <- max(data1()$timeC)
    sliderInput("slider3", h6("Year to which the prior should be applied:"), min = minZ, max = maxZ, value = minZ, step = 1)
  })
  
  output$slider4 = renderUI({
    minZ <- min(data1()$timeC)
    maxZ <- max(data1()$timeC)
    sliderInput("slider4", h6("Year to which the prior should be applied:"), min = minZ, max = maxZ, value = minZ, step = 1)
  })
  
  output$slider5 = renderUI({
    maxZ <- max(data1()$timeC)
    numericInput("start", "Specify the start of the forecast interval:", NULL, min = maxZ , max = maxZ + 20)
  })
  
  output$slider6 = renderUI({
    numericInput("length", "Specify the length of the forecast interval:", NULL, min = 1 , max = 100)
  })
  
  
  output$slider7 = renderUI({
    numericInput("scenario", "Fishing scenario: specify a factor to multiply the current fishing mortality by ",NULL, min = 0 , max = 10) 
  })
  
  output$slider8 = renderUI({
    maxZ <- max(data1()$timeC)
    numericInput("manage", "Year that management should start ",NULL, min = maxZ , max = maxZ + 20)
  })
  
  output$slider9 = renderUI({
    maxZ <- max(data1()$timeC)
    numericInput("point", "Time point of the reported forecast of biomass and fishing mortality ",NULL, min = maxZ , max = maxZ + 20)
  })
  
  output$slider10 = renderUI({
    minZ <- min(data1()$timeC)
    maxZ <- max(data1()$timeC)
    numericInput("shift", "Specify the year of shift:", NULL, min = minZ , max = maxZ )
  })
  
  output$slider11 = renderUI({
    minZ <- min(data1()$timeC)
    maxZ <- max(data1()$timeC)
    sliderInput("slider11", h6("Year to which the prior should be applied:"), min = minZ, max = maxZ, value = minZ, step = 1)
  })
  
  output$slider12 = renderUI({
    minZ <- min(data1()$timeC)
    maxZ <- max(data1()$timeC)
    sliderInput("slider12", h6("Year to which the prior should be applied:"), min = minZ, max = maxZ, value = minZ, step = 1)
  })
  
  output$slider13 = renderUI({
    minZ <- min(data1()$timeC)
    maxZ <- max(data1()$timeC)
    sliderInput("slider13", h6("Year to which the prior should be applied:"), min = minZ, max = maxZ, value = minZ, step = 1)
  })
  
  output$slider14 = renderUI({
    minZ <- min(data1()$timeC)
    maxZ <- max(data1()$timeC)
    sliderInput("slider14", h6("Year to which the prior should be applied:"), min = minZ, max = maxZ, value = minZ, step = 1)
  })
  
  output$slider15 = renderUI({
    minZ <- min(data1()$timeC)
    maxZ <- max(data1()$timeC)
    numericInput("gshift",  "Specify the year of shift:", NULL, min = minZ , max = maxZ )
  })
  
  output$slider17 = renderUI({
    minZ <- min(data1()$timeC)
    maxZ <- max(data1()$timeC)
    sliderInput("slider17", h6("Year to which the prior should be applied:"), min = minZ, max = maxZ, value = minZ, step = 1)
  })
  
  output$slider18 = renderUI({
    minZ <- min(data1()$timeC)
    maxZ <- max(data1()$timeC)
    sliderInput("slider18", h6("Year to which the prior should be applied:"), min = minZ, max = maxZ, value = minZ, step = 1)
  })
  
  output$slider19 = renderUI({
    minZ <- min(data1()$timeC)
    maxZ <- max(data1()$timeC)
    sliderInput("slider19", h6("Year to which the prior should be applied:"), min = minZ, max = maxZ, value = minZ, step = 1)
  })
  
  output$slider20 = renderUI({
    minZ <- min(data1()$timeC)
    maxZ <- max(data1()$timeC)
    sliderInput("slider20", h6("Year to which the prior should be applied:"), min = minZ, max = maxZ, value = minZ, step = 1)
  })
  
  
########## Results#######################
  
  # Plot data simple - advanced
  output$dataplot <- renderPlot({
    return(plotting())
  })
  
  output$plotOfResults <- renderPlot({
    return(plot(model1()))
  })
  
  
  # Print Summary of results
  # Convergence of the model fit, which has code 0 if the fit was successful
  
  output$Summaryofresults <- renderText({
    capture.output(summary(model1()))
  })
  
  
  output$plotRun <- renderPlot({
    plot(model1())
  })
  
  # downloadHandler to save plots in a pdf file
  output$downloadPlotSPiCT <- downloadHandler(
    filename = function(){paste(input$plots, ".png", sep="")},
    content = function(file) {
    png(file)
    print(plot(model1()))
    dev.off()
  })
  
  output$plots1 <- renderPlot({
    if (input$plots == 'bio'){
      plotspict.biomass(model1())
    } 
    else if (input$plots == 'rbio') {
      plotspict.bbmsy(model1())
    }
    else if (input$plots == 'fmort') {
      plotspict.f( model1(), main='', qlegend=FALSE, rel.axes=FALSE, rel.ci=FALSE)
    }
    else if (input$plots == 'rfmort') {
      plotspict.ffmsy(model1(),main='', qlegend=FALSE)
    }
    else if (input$plots == 'catch') {
      plotspict.catch(model1())
    }
    else if (input$plots == 'curve') {
      plotspict.production(model1())
    }
    else if (input$plots == 'kobe'){
      plotspict.fb(model1())
    }
    else {
      plotspict.priors(model1(), do.plot=NULL)
    }
    
  })
  
  
  
  # downloadHandler to save plots in a pdf file
  output$downloadPlot <- downloadHandler(
    filename = function(){paste(input$plots, ".png", sep="")},
    content = function(file) {
      png(file)
      
      if(input$plots == 'bio'){print(plotspict.biomass(model1()))}
      else if(input$plots == 'rbio'){print(plotspict.bbmsy(model1()))}
      else if (input$plots == 'fmort'){print(plotspict.f( model1(), main='', qlegend=FALSE, rel.axes=FALSE, rel.ci=FALSE))}
      else if (input$plots == 'rfmort') {print(plotspict.ffmsy(model1(),main='', qlegend=FALSE))}
      else if (input$plots == 'catch') {print(plotspict.catch(model1()))}
      else if (input$plots == 'curve') {print(plotspict.production(model1()))}
      else if (input$plots == 'kobe'){ print(plotspict.fb(model1()))}
      else if (input$plots == 'priors'){print(plotspict.priors(model1(), do.plot=NULL))}
      dev.off()
  })
  
  output$residuals <- renderPlot({
    plotspict.diagnostic(model2())
  })
  
  
  # downloadHandler to save residuals in png file
  output$downloadPlotResiduals <- downloadHandler(
    filename = function(){"Residuals.png"},
    content = function(file) {
    png(file)
    print(plotspict.diagnostic(model2()))
    dev.off()
  })
  
  #Retrospecitive plots are sometimes used to evaluate the robustness of the model fit to the introduction of newdata
  output$retro <- renderPlot({
    res        <- retro(model1(), nretroyear = 6)
    plotspict.retro(res)
    
  })
  
  
  
  #downloadHandler to save retrospective results in png file
  output$downloadPlotRetro <- downloadHandler(
    filename = function(){"Retrospective.png"},
    content = function(file) {
      png(file)
      print(plotspict.retro(retro(model1(), nretroyear = 6)))
      dev.off()
    }   
  )
  
  output$simulation<- renderPlot({
    inp <- check.inp(data1())
    sim <- sim.spict(inp)
    plotspict.data(sim)
    
  })
  
  
  #downloadHandler to save retrospective results in png file
  output$downloadPlotSimulating <- downloadHandler(
    filename = function(){"Simulating.png"},
    content = function(file) {
    png(file)
    print( plotspict.data(sim.spict(check.inp(data1()))))
    dev.off()
  })
  
  output$tablesummary <- renderTable({
    maxZ <- max(data1()$timeC)
    m       <- get.par('logm', model1(), exp=F)
    n       <- get.par('logn', model1(), exp=F)
    q       <- get.par('logq', model1(), exp=F)
    Bmsy    <- get.par('logBmsy', model1(), exp=F)         #get a parameter value
    Fmsy    <- get.par('logFmsy', model1(), exp=F)         
    K       <- get.par('logK', model1(), exp=F)  
    r       <- get.par('logr', model1(), exp=F)   
    MSY     <- get.par('logMSY', model1(), exp=F)
    B       <- get.par('logB', model1(), exp=F)
    Fm      <- get.par('logF', model1(), exp=F)
    FFmsy   <- get.par('logFFmsy', model1(), exp=F)
    BBmsy   <- get.par('logBBmsy', model1(), exp=F)
    Fmsyll  <- Fmsy[1]
    Fmsyul  <- Fmsy[3]
    Bmsyll  <- Bmsy[1]
    Bmsyul  <- Bmsy[3]
    FFmsyll <- FFmsy[which(row.names(FFmsy)==maxZ), 1]
    FFmsyul <- FFmsy[which(row.names(FFmsy)==maxZ), 3]
    BBmsyll <- BBmsy[which(row.names(BBmsy)==maxZ), 1]
    BBmsyul <- BBmsy[which(row.names(BBmsy)==maxZ), 3]
    
    df <- data.frame(K=format(round(exp(K[,2]), 0), nsmall = 0),
                     r=format(round(exp(r[,2]), 3), nsmall = 3),
                     MSY=format(round(exp(MSY[,2]), 0), nsmall = 0),
                     Bmsy=format(round(exp(Bmsy[,2]), 0), nsmall = 0),
                     Fmsy=format(round(exp(Fmsy[,2]), 2), nsmall = 2),
                     BBmsy=format(round(exp(BBmsy[which(row.names(BBmsy)==maxZ),2]), 2), nsmall = 2),
                     FFmsy=format(round(exp(FFmsy[which(row.names(FFmsy)==maxZ),2]), 2), nsmall = 2)
    )
    print(df)
  })
  
  output$forecastsum <-  renderPrint({
    
    inp  <- data1()
    res  <- fit.spict(inp)
    res1 <- manage(res)
    df1  <- mansummary(res1)
    head(df1)
    p1   <- tableGrob(head(df1))
    grid.arrange(p1)
  })
  
  
  
  
}
