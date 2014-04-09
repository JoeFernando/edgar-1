source('useful functions.R')

## ---- server ----

shinyServer(function(input, output){
  
  store <- reactiveValues(filingstable = NULL, xbrl = NULL)
  
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
  
  output$elementselect <- renderUI({
    if(!is.null(input$formlink)){
      if(input$formlink != " -- "){
        splits <- strsplit(input$formlink, split = " -- ")[[1]]
        link <- store$filingstable$links[which(store$filingstable$`Filings` == splits[1] & store$filingstable$`Filing Date` == splits[2])]
        print(link)
        store$xbrl <- getXbrlData(xbrl.inst.doc.loc(get.files(link)))
        display <-unique(get.var.type(store$xbrl$fct$elementId))
        selectInput("elementtype", "Choose Element Type", display)
      }
    }
  })
})

