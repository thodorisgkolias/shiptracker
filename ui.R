# UI
ui <- dashboardPage(
  title = 'Ship Tracker',
  header = NULL,
  sidebar = dashboardSidebar(disable = TRUE),
  body = dashboardBody(
    fluidRow(
      column(width = 8,
             p("Select Vessel Type:"),
             ShipTypeUI(id = 'ship')
      ),
      column(width = 8,
             p("Select Vessel:"),
             ShipNameUI(id = 'ship')
      )
    ),
    fluidRow(
      leafletOutput("route", height = "600px")
      
    ),
    fluidRow(
      
      valueBoxOutput(outputId = 'totalDist', width = 5),
      valueBoxOutput(outputId = 'parked', width = 5),
      valueBoxOutput(outputId = 'nextdest', width = 5)
      
    )
  )
)
