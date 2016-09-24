dashboardPage(
  dashboardHeader(title="Fantasy Test"
  ),
  dashboardSidebar(
    sidebarMenu(
      actionButton("button", "Start"),
      checkboxInput("scrapeBox", "Some valus", FALSE)
        
      )),
    dashboardBody(
      column(12, tableOutput('table')
      )
    )
  )
)