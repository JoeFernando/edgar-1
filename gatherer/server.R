source('useful functions.R')
shinyServer(function(input, output){
  
  
  output$ciktable <- renderTable({
    result <- cik.lookup(input$toSearch)
    if(!identical(NULL, result)){
      result
    }
  })
  
  output$filingstable <- renderTable({
    result <- get.filing.locs(cik = as.numeric(input$cik), type = input$formtype)
    if(!identical(NULL, result)){
      result[,c("Filings", "Filing Date", "File/Film Number")]
    }
  })
})