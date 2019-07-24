
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)

cities <- c(
  "Chicago" = "chicago",
  "New York" = "nyc",
  "Dallas" = "tx",
  "Atlanta" = "atl",
  "Denver" = "dnv",
  "Minneapolis" = "mn",
  "San Francisco" = "sfbay",
  "Los Angeles" = "la",
  "Seattle" = "seattle"
)

modes <- c(
  "Median Rent Per Unit" = "rent",
  "Median Tax Per Unit" = "tpu"
)

vars <- c(
  "Total Population Change" = "pop",
  "25 - 44 Year Old Shift" = "rent"
)

shinyUI(navbarPage("KIG Analytics", id="nav",
                   
                   tabPanel("Taxes and Rents",
                            div(class="outer",
                                tags$head(
                                  includeCSS("style.css")
                                ),
                                
                                leafletOutput("map", width = "100%", height = "100%"),
                                
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 330, height = "auto",
                                              
                                              h2("City"),
                                              selectInput("option", "Cities", cities),
                                              h2("Mode"),
                                              selectInput("choice", "Metric", modes)
                                )
                            )
                   ),
                   tabPanel("Chicagoland Population Change",
                            div(class="outer",
                                tags$head(
                                  includeCSS("style.css")
                                ),
                                
                                leafletOutput("pop_map", width = "100%", height = "100%"),
                                
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 330, height = "auto",
                                              
                                              h2("Chicagoland Population Change 2011-2015"),
                                              selectInput("age", "Ages", vars)
                                )
                            )
                   )
                   
))