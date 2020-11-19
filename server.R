# Server


server <- shinyServer(
  function(input, output, session) {
    
    # Call modules
    shiptype <- callModule(ShipTypeServer, id = 'ship')
    shipname <- callModule(ShipNameServer, id = 'ship')
    
    
    # ship route
    ShipData <- reactive({
      req(shiptype())
      req(shipname())
      
      validate(need(!is.null(shipname()),message = ''))
      
      
      data %>%
        filter(ship_type == shiptype(),
               SHIPNAME == shipname()) %>%
        mutate(LON_Start = lag(LON),
               LAT_Start = lag(LAT)) %>%
        mutate(Dist = distHaversine(cbind(LON, LAT), cbind(LON_Start, LAT_Start)),
               Dist = ifelse(is.na(Dist), # one obs
                             0,
                             Dist))
    })
    
    
    # map
    output$route <- renderLeaflet({
      req(shiptype())
      req(shipname())
      validate(need(nrow(ShipData() > 0), message = ''))
      
      ShipRoute <- ShipData() %>%
        filter(Dist == max(Dist, na.rm = TRUE)) %>%
        filter(DATETIME == ymd_hms(max(DATETIME, na.rm = TRUE))) %>%
        distinct()
      
      Lon = rbind(ShipRoute$LON_Start, ShipRoute$LON)
      Lat = rbind(ShipRoute$LAT_Start, ShipRoute$LAT)
      
      leaflet() %>%
        addTiles() %>%
        addCircleMarkers(lng = Lon, 
                         lat = Lat) %>%
        addPolylines(lng = Lon,
                     lat = Lat) %>%
        addPopups(lng = Lon[2],
                  lat = Lat[2],
                  popup = paste0(ShipRoute$SHIPNAME, ' has traveled ', round(ShipRoute$Dist,0), 'm'))
    })
    
    # Value boxes
    output$totalDist <- renderValueBox({
      total_dist <- ShipData() %>%
        group_by(SHIPNAME) %>%
        summarise(Dist = sum(Dist, na.rm = TRUE))
      
      valueBox(subtitle = 'Total distance traveled',
               value = round(total_dist$Dist,0),
               icon = icon('map'),
               color = 'blue'
      )
    })
    
    output$parked <- renderValueBox({
      parked_status <- ShipData() %>%
        group_by(SHIPNAME) %>%
        filter(DATETIME == ymd_hms(max(DATETIME, na.rm = TRUE))) %>%
        distinct() %>%
        mutate(Status = ifelse(is_parked == 1,
                               'Parked',
                               'Moving'))
      
      
      
      
      valueBox(subtitle = 'Current status',
               value = parked_status$Status,
               icon = icon('ship'),
               color = 'green'
      )
    })
    
    output$nextdest <- renderValueBox({
      destination <- ShipData() %>%
        group_by(SHIPNAME) %>%
        filter(DATETIME == ymd_hms(max(DATETIME, na.rm = TRUE))) %>%
        distinct()
      
      
      
      valueBox(subtitle = 'Next destination',
               value = destination$DESTINATION,
               icon = icon('anchor'),
               color = 'red'
      )
    })
    
  }
)
