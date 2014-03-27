source('useful functions.R')
shinyServer(function(input, output){
  output$ciktable <- renderTable({
    result <- cik.lookup(input$toSearch)
    if(!identical(NULL, result)){
      result
    }
  })
})