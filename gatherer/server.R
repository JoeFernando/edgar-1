source('useful functions.R')

## ---- server ----

shinyServer(function(input, output){
  
  companies.data <- reactiveValues(ciks = NULL, n = 0, xbrlData = list(), tickers = NULL, queries = NULL)
  
  selected.vars <- reactiveValues(this = NULL)
  
  ods <- reactiveValues(data = NULL, type = NULL, symbol = NULL, n = 0, last.grab = NULL)
  
  output$ods <- renderTable({
    if(input$addODS == 0) return(NULL)
    
    isolate({
      ods$n <- ods$n + 1
      ods$type[ods$n] <- input$odsType
      ods$symbol[ods$n] <- input$symbol
      if(input$odsType == "Symbols"){
        ods$data[[ods$n]] <- fix.getSymbols(getSymbols(input$symbol,
                                        src = "yahoo",
                                        from = input$odsStartDate,
                                        to = input$odsEndDate,
                                        auto.assign = FALSE))
        ods$last.grab <- ods$data[[ods$n]]
      }
      if(input$odsType == "Metals"){
        ods$data[[ods$n]] <- fix.getSymbols(getMetals(input$symbol,
                                                      from = input$odsStartDate,
                                                      to = input$odsEndDate,
                                                      auto.assign = FALSE))
        ods$last.grab <- ods$data[ods$n]
      }
      
      
    })
#     data.frame(ods$last.grab)
    data.frame(type = ods$type, symbol = ods$symbol)
  })
  
  output$ciktable <- renderTable({
    result <- cik.lookup(input$toSearch)
    if(!identical(NULL, result)){
      result
    }
  })
  co.table.data <- reactive({
    input$pullCik
    
    cur.l <- length(companies.data$ciks)
    companies.data$ciks[cur.l + 1] = input$cikToPull  
    print(companies.data$ciks)
  })
  
  
  output$companiestable <- renderTable({
    if(input$pullCik == 0) return(NULL)
    
    isolate({
    if(!(input$cikToPull %in% companies.data$ciks)){
      companies.data$n = companies.data$n + 1
      companies.data$ciks[companies.data$n] = (input$cikToPull)
      companies.data$xbrlData[[companies.data$n]] = pull.10K(cik = input$cikToPull, from = input$startDate, to = input$endDate)
      companies.data$tickers[companies.data$n] = unique(companies.data$xbrlData[[companies.data$n]]$ticker)[1]
      companies.data$queries[companies.data$n] = '.'
    }
    })
    data.frame(CIK = companies.data$ciks, ticker = companies.data$tickers)
    
  })
  
  output$selectedVariables <- renderTable({
    if(input$query == 0) return(NULL)
    input$tickerSelect
      
    isolate({
      index <- which(companies.data$tickers == input$tickerSelect)
      selected.vars$this <- data.frame(grep(input$varQuery,companies.data$xbrlData[[index]]$elementId, value = TRUE))
      names(selected.vars$this) <- companies.data$tickers[index]
      return(data.frame(selected.vars$this))
    })
  })
  
  output$symbol <- renderTable({
    if(input$addSymbols == 0) return(NULL)
    
    isolate({

      new <- fix.getSymbol(getSymbols(input$symbol, auto.assign = FALSE))
      total <- merge(new, symbol.data$symbols, all = TRUE)
      symbol.data$symbols <- total
    })
    
    symbol.data$symbols
    })
  
  output$varQuerySelect <- renderUI(
    selectInput(inputId = "tickerSelect",
                label = "Ticker to Select Query",
                choices = companies.data$tickers)
    )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      xbrl.data <- xbrl.cn(companies.data$xbrlData, variables = input$varQuery)$data
      ods.data <- merge.list(ods$data)
      if(is.null(xbrl.data)){
        if(!is.null(ods.data)){
          data <- ods.data
        }
      }else{
        if(is.null(ods.data)){
          data <- xbrl.data
        }else{
          data <- merge(xbrl.data, ods.data, all = TRUE)
        }
      }
      write.csv(data, file, row.names = FALSE)
    }
  )

  output$downloadKeyframe <- downloadHandler(
    filename = function() {
      paste('keyframe-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      data <- xbrl.cn(companies.data$xbrlData, variables = input$varQuery)
      write.csv(data$keyframe, file, row.names = FALSE)
    }
    )

})

