
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(data.table)
library(leaflet)
library(tigris)
library(stringr)
library(RColorBrewer)

library(readr)

pal<-colorNumeric(palette="YlGnBu",
                  domain = 0:3000
)  

popup <- paste0("CT: ", il_combo_merged$census_tract, 
                "<br>", "AVG TPU: ", il_combo_merged$tax_avg,
                "<br>","min: ", il_combo_merged$min, 
                "<br>", "mid: ", il_combo_merged$middle, 
                "<br>", "max: ", il_combo_merged$max,
                "<br>", "rent: $", il_combo_merged$median_gross_rent)

#create a map
map <-renderLeaflet({
  leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = il_combo_merged, 
                fillColor = ~pal(median_gross_rent), 
                color = "#b2aeae", # you need to use hex colors
                opacity = 0,
                fillOpacity = 0.6, 
                weight = 1, 
                smoothFactor = 0.2,
                popup = popup) %>%
    addLegend(pal = pal, 
              values = 0:3000, 
              position = "bottomright", 
              title = "Median Annual Tax Per Unit",
              layerId="colorLegend",
              labFormat = labelFormat(prefix = "$")) 
})

pop_pal <- colorNumeric(
  palette = "RdYlGn",
  domain = -7000:7000
)

pop_popup <- paste0("zip: ", il_pop_change$zip, "<br>", "Net Migration: ", round(il_pop_change$change,2),"<br>","Avg Margin of Error: ", round(((il_pop_change$MoE_2011 + il_pop_change$MoE_2015)/2),2))

pop_map <- renderLeaflet({
  leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = il_pop_change, 
                fillColor = ~pop_pal(change), 
                color = "#b2aeae", # you need to use hex colors
                opacity = 0,
                fillOpacity = 0.55, 
                weight = 1, 
                smoothFactor = 0.2,
                popup = pop_popup) %>%
    addLegend(pal = pop_pal, 
              values = -7000:7000, 
              position = "bottomright", 
              title = "2011-2015 Net Migration",
              layerId="colorLegend",
              labFormat = labelFormat(prefix = ""))
})

shinyServer(function(input, output) {
  
  output$map <- map
  output$pop_map <- pop_map
  
  observe({
    optionBy <- input$option
    choiceBy <- input$choice
    dt <- il_combo_merged
    
    if (optionBy == "atl") {
      #setView(map, 84.3880,33.7490, 13)
      lng = -84.3880
      lat = 33.7490
      dt <- ga_combo_merged
    } else if (optionBy == "nyc") {
      #setView(map, 74.0060,40.7128, 13)
      lng = -74.0060
      lat = 40.7128
      dt <- ny_combo_merged
    } else if (optionBy == "chicago") {
      #setView(map, 87.6298,41.8781, 13)
      lng = -87.6298
      lat = 41.8781
      dt <- il_combo_merged
    } else if (optionBy == "dnv") {
      #setView(map, 104.9903,39.7392, 13)
      lng = -104.9903
      lat = 39.7392
      dt <- co_combo_merged
    } else if (optionBy == "sfbay") {
      #setView(map, 122.4194,37.7749, 13)
      lng = -122.4194
      lat = 37.7749
      dt <- sf_combo_merged
    } else if (optionBy == "la") {
      lng = -122.4194
      lat = 37.7749
      dt <- la_combo_merged
    } else if (optionBy == "seattle") {
      lng = -122.3321
      lat = 47.6062
      dt <- wa_combo_merged
    } else if (optionBy == "tx") {
      lng = -96.7970
      lat = 32.7767
      dt <- tx_combo_merged
    } else {
      lng = -93.2650
      lat = 44.9778
      dt <- mn_combo_merged
    }

    if (choiceBy == "tpu"){
      top <- 8000
      polymetric <- dt$middle
      pal<-colorNumeric(palette="YlOrRd",
                        domain = 0:top
      )
      pre_fix <- "$"
      b_title <- "Median Tax Per Unit"
    } else {
      top <- 3000
      polymetric <- dt$median_gross_rent
      pal<-colorNumeric(palette="YlGnBu",
                        domain = 0:top
      )  
      pre_fix <- " "
      b_title <- "Median Rent"
    } 
    popup <- paste0("Census Tract: ", dt$census_tract, 
                    "<br>", "AVG TPU: $", round(dt$tax_avg,2),
                    "<br>","min: $", dt$min, 
                    "<br>", "mid: $", dt$middle, 
                    "<br>", "max: $", dt$max,
                    "<br>", "rent: $", dt$median_gross_rent, 
                    "<br>", "tax/rent ratio: ", dt$ratio)
    
    leafletProxy("map", data = dt) %>%
      clearShapes() %>%
      addPolygons(data = dt, 
                  fillColor = ~pal(polymetric), 
                  color = "#b2aeae", # you need to use hex colors
                  opacity = 0,
                  fillOpacity = 0.6, 
                  weight = 1, 
                  smoothFactor = 0.2,
                  popup = popup) %>%
      addLegend(pal = pal, 
                values = 0:top, 
                position = "bottomright", 
                title = b_title,
                layerId="colorLegend",
                labFormat = labelFormat(prefix = pre_fix)) %>%
      setView(lng,lat,zoom = 10)
    
  })
  
  observe({
    optionBy <- input$age
    
    if (optionBy == "pop"){
      dt <- il_pop_change
      top <- 7000
      bottom <- -7000
      polymetric <- il_pop_change$change
      pop_pal <- colorNumeric(
        palette = "RdYlGn",
        domain = bottom:top
      )
      
      pop_popup <- paste0("zip: ", il_pop_change$zip, "<br>", "Net Migration: ", round(il_pop_change$change,2),"<br>","Avg Margin of Error: ", round(((il_pop_change$MoE_2011 + il_pop_change$MoE_2015)/2),2))
      b_title <- "Chicagoland Population Change"
    } else {
      dt <- il_pop_age_change
      top <- 3000
      bottom <- -3000
      polymetric <- il_pop_age_change$change
      pop_pal <- colorNumeric(
        palette = "RdYlGn",
        domain = bottom:top
      )
      pop_popup <- paste0("zip: ", il_pop_age_change$zip, "<br>", "Net Migration: ", round(il_pop_age_change$change,2))
      b_title <- "Net Migration: 25-44 Year Olds"
    }
    
    leafletProxy("pop_map", data = dt) %>%
      clearShapes() %>%
      addPolygons(data = dt, 
                  fillColor = ~pop_pal(polymetric), 
                  color = "#b2aeae", # you need to use hex colors
                  opacity = 0,
                  fillOpacity = 0.6, 
                  weight = 1, 
                  smoothFactor = 0.2,
                  popup = pop_popup) %>%
      addLegend(pal = pop_pal, 
                values = bottom:top, 
                position = "bottomright", 
                title = b_title,
                layerId="colorLegend",
                labFormat = labelFormat(prefix = "")) 
  })
  
})
