dashboardPage(
  title="Fantasy Test",
  dashboardHeader(title="Fantasy Test"
  ),
  dashboardSidebar(
    sidebarMenu(
     fluidRow(
       
       br(),
      div(
        actionButton("button", "Start", width='70%')
        , checkboxInput("showSalary", "Show Selection Salary", FALSE)
        , checkboxInput("showInjury", "Show Selection Injury", FALSE)
        , align="center")
      #checkboxInput("scrapeBox", "Some values", FALSE),
      #downloadButton('downloadData', 'Download')
    ))),
  dashboardBody(
    singleton( tags$head(tags$script(src = "message_handler.js"))),
    fluidPage(
      column(12, 
             DT::dataTableOutput('table'),
             #uiOutput('tableSelect')
             DT::dataTableOutput('tableSelect')
      )
    )
  )
)
