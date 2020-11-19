# Global
library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(tidyverse)
library(leaflet)
library(geosphere)
library(lubridate)

# Load data
data <- readRDS('data/ships.rds')
shiptypes <- unique(data$ship_type)


# Modules
ShipTypeUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns('shiptype'))
}

ShipTypeServer <- function(input, output, session) {
  output$shiptype <- renderUI({
    dropdown_input(
      input_id = session$ns('shiptype'),
      choices = shiptypes,
      value = shiptypes[1]
    )
  })
  
  return(reactive({input$shiptype}))
  
}


ShipNameUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns('shipname'))
}

ShipNameServer <- function(input, output, session) {
  output$shipname <- renderUI({
    dropdown_input(
      input_id = session$ns('shipname'),
      choices = ''
    )
  })
  
  observeEvent(input$shiptype,{
    
    shipnames <- data %>%
      filter(ship_type == input$shiptype) %>%
      distinct(SHIPNAME)
    
    update_dropdown_input(session,
                          input_id = 'shipname',
                          choices = shipnames$SHIPNAME
    )
    
  })
  
  return(reactive({input$shipname}))
}

