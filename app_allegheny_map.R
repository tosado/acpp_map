# install.packages("shiny")
# install.packages("leaflet")
# install.packages("tidyverse")
# install.packages("rgdal")
# install.packages("maptools")
# install.packages("sp")

library(leaflet)
library(tidyverse)
library(shiny)



data_police<-read.csv("/Users/tosanadoki/Desktop/PIA 2096 R Data Visualization/contract data for r visualization - Sheet1.csv")
muni_sf<-rgdal::readOGR("/Users/tosanadoki/Desktop/PIA 2096 R Data Visualization/Allegheny_County_Municipal_Boundaries-shp/LandRecords_LANDRECORDS_OWNER_Municipalities.shp")

muni_sf<- spTransform(muni_sf, CRS("+proj=longlat +datum=WGS84"))


m<-merge(muni_sf, data_police, by.x = "LABEL", by.y = "Location.Name")
names(m)

data_police<- data_police %>%
  mutate(popup = paste0("<b>", data_police$Location.Name,"<b>",
                        "<br/><i>", data_police$Link.to.police.department, "</i>"
  )
  ) 
#select(color, popup)


num_regions<-unique(m$REGION)

num_COG<-unique(m$COG)
num_SD<-unique(m$SCHOOLD)
color_pal_COG<-colorFactor(topo.colors(length(num_COG)), m$COG)
color_pal_REGION<-colorFactor(topo.colors(length(num_regions)), m$REGION)
color_pal_SD<-colorFactor(topo.colors(length(num_SD)), m$REGION)

ui<-fluidPage(
  #leafletOutput("mymap"),
  p(),
  titlePanel("Allegheny County Municipal Map: Police Contracts"),
  sidebarLayout(
    sidebarPanel(
      helpText("A tool for finding trends"),
      selectInput("colorcode", label = "Choose from these options",
                  choices = c("Region" =1, "COG" = 2,
                              "School District"=3),
                  selected = "COG")
    ), mainPanel(leafletOutput("mymap"),
                 textOutput("chosen"))
  )
)

# server <- function(input, output, session) {
#   
#   points <- eventReactive(input$recalc, {
#     cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
#   }, ignoreNULL = FALSE)
#   
#   output$mymap <- renderLeaflet({
#     leaflet() %>%
#       addProviderTiles(providers$Stamen.TonerLite,
#                        options = providerTileOptions(noWrap = TRUE)
#       ) %>%
#       addMarkers(data = points())
#   })
# }

server <- function(input, output) {
  
  coded<-reactive({
    req(input$colorcode)
    #test<-renderText(input$colorcode)
    #test<-req(input$colorcode %in% names(data_police))
    paste0(input$colorcode)
    
  })
  
  
  
  output$chosen<-renderPrint({
    paste0("Showing map by ", coded())
  })
  
  output$mymap <- renderLeaflet({
    leaflet()%>% addTiles()%>%
      addPolygons(data = m, label= m$LABEL, color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor ="orange",
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE))

  })
  
  
  
  
}
# Run app ----
shinyApp(ui , server )

