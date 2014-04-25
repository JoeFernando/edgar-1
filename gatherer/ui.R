## ---- ui ----

shinyUI(pageWithSidebar(
  headerPanel("Financial Data Gatherer"),
  
  sidebarPanel(
    textInput(inputId = "filename",
              label = "Output Filename",
              value = "output.csv"),
    downloadButton('downloadData', 'Download Data'),
    downloadButton('downloadKeyframe', 'Download Keyframe'),
    conditionalPanel( condition = "input.main == 'Quarterly Reports'",
      wellPanel(
        textInput(inputId = "toSearch",
                  label = "CIK Lookup",
                  value = "Google"),
        textInput(inputId = "cikToPull",
                  label = "Get Data For:",
                  value = '0001288776'),
        wellPanel(
          dateInput(inputId = 'startDate',
                    label = "From",
                    value = "1990-01-01",
                    format = "yyyy-mm-dd"
          ),
          dateInput(inputId = 'endDate',
                    label = "To",
                    value = today(),
                    format = "yyyy-mm-dd")
        ),
        actionButton(inputId = "pullCik",
                     label = "Add Company")
      ),
      
      conditionalPanel(condition = "input.companies != 'CIK Lookup'",
                       wellPanel(
                         uiOutput('varQuerySelect'),
                         textInput(inputId = 'varQuery',
                                   label = "REGEX Variable Query",
                                   value = '.'),
                         actionButton(inputId = "query",
                                      label = "Query")
                         ))
    ),
    conditionalPanel(condition = "input.main = 'Other Data Sources'",
                     wellPanel(
                       textInput(inputId = "symbol",
                                 label = "Symbol",
                                 value = "GOOG"),
                       radioButtons(inputId = "odsType",
                                    label = "Data Type",
                                    choices = c("Symbols", "Metals")),
                       dateInput(inputId = 'odsStartDate',
                                 label = "From",
                                 value = today() - days(499),
                                 format = "yyyy-mm-dd"
                       ),
                       dateInput(inputId = 'odsEndDate',
                                 label = "To",
                                 value = today(),
                                 format = "yyyy-mm-dd"),
                       actionButton(inputId = "addODS",
                                    label = "Add Data")
                       ))
  ),
  
  mainPanel(
    tabsetPanel( id = 'main',
      tabPanel("Quarterly Reports",
               tabsetPanel( id = "companies", type = "pills",
                 tabPanel("CIK Lookup", tableOutput(outputId = "ciktable")),
                 tabPanel("Companies in Data", tableOutput(outputId = "companiestable")),
                 tabPanel("Selected Variables", tableOutput(outputId = "selectedVariables"))
                )
              ),
      tabPanel("Other Data Sources", tableOutput(outputId = "ods"))
      )
    )
  ))

