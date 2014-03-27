shinyUI(pageWithSidebar(
  headerPanel("Financial Data Gatherer"),
  
  sidebarPanel(textInput(inputId = "toSearch",
            label = "CIK Lookup",
            value = "Enter search term...")),
  
  mainPanel(tableOutput(outputId = "ciktable"))
  
  ))