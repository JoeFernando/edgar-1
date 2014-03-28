source('useful functions.R')
shinyServer(function(input, output){
  
  store <- reactiveValues(filingstable = NULL)
  
  output$ciktable <- renderTable({
    result <- cik.lookup(input$toSearch)
    if(!identical(NULL, result)){
      result
    }
  })
  
  output$filingstable <- renderTable({
    result <- get.filing.locs(cik = as.numeric(input$cik), type = input$formtype)
    if(!identical(NULL, result)){
      store$filingstable <- result
      result[,c("Filings", "Filing Date", "File/Film Number")]
    }
  })
  
  output$formselect <- renderUI({
    display <- paste(store$filingstable$Filings, "--", store$filingstable$`Filing Date`)
    selectInput("formlink","Choose Form", display)
  })
})