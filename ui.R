dashboardPage(
  dashboardHeader(title="Fantasy Test"
  ),
  dashboardSidebar(
    sidebarMenu(
      actionButton("button", "Start"),
      checkboxInput("scrapeBox", "Some valus", FALSE)
      downloadButton('downloadData', 'Download')
      )),
    dashboardBody(
      column(12, tableOutput('table')
      )
    )
  )
)