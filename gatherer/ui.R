## ---- ui ----

shinyUI(pageWithSidebar(
  headerPanel("Financial Data Gatherer"),
  
  sidebarPanel(
    wellPanel(
      textInput(inputId = "toSearch",
                label = "CIK Lookup",
                value = "Google")
      ),
    
    wellPanel(
      textInput(inputId = "cik",
                   label = "Filings Search",
                   value = "0001288776"),
      
      textInput(inputId = "formtype",
                label = "Form Type",
                value = "10-K")
      ),
    
    wellPanel(
      uiOutput("formselect")
      ),
    
    wellPanel(
      uiOutput("elementselect")
      )
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("CIK Lookup", tableOutput(outputId = "ciktable")),
      tabPanel("Filings Lookup", tableOutput(outputId = "filingstable"))
      )
    )
  ))