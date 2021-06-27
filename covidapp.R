library(shiny)
library(tidyverse)
library(leaflet)
library(htmlwidgets)

all_modzcta <- readRDS("all_modzcta.RDS")

ui <- fluidPage(
  titlePanel("COVID-19 Trends by ZCTA"),
  sidebarLayout(
    sidebarPanel(
      tags$a(href = "https://github.com/nychealth/coronavirus-data","Data Repository", target = "_blank"),
      selectInput("date",
                  "Select a date (week ending in):",
                  choices = unique(all_modzcta$week_ending)
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Case Rate", leafletOutput("cases")),
        tabPanel("Test Rate", leafletOutput("tests")),
        tabPanel("Percent Positive", leafletOutput("pctpos"))
      )
    )
  )
)


server <- function(input, output){
  week_zcta <- reactive({
    w <- all_modzcta %>% filter(week_ending == input$date)
    return(w)
  })
  
  output$cases <- renderLeaflet({
    pal <- colorBin(palette = "YlGn", 9, domain = all_modzcta$caserate)
    
    labels <- sprintf(
      "<strong>%s</strong><br/%g> cases per 100,000 people",
      week_zcta()$MODZCTA, week_zcta()$caserate) %>%
      lapply(htmltools::HTML)
      
    week_zcta() %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(-73.9, 40.7, zoom = 10) %>%
      addPolygons(label = labels,
                  stroke = FALSE,
                  smoothFactor = 0.5,
                  apacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~pal(week_zcta()$caserate),
                  highlightOptions = hilightOptions(weight = 5,
                                                    fillOpacity = 1,
                                                    color = "black",
                                                    apacity = 1,
                                                    bringToFront = TRUE)) %>%
    addLegend("bottomright", 
              pal = pal,
              values = ~caserate,
              title = "Cases Per 100,000",
              apacity = 0.8)
  })
  
  output$tests <- renderLeaflet({
    pal <- colorBin(palette = "PuBu", 9, domain = all_modzcta$testrate)
    
    labels <- sprintf(
      "<strong>%s</strong><br/%g> cases per 100,000 people",
      week_zcta()$MODZCTA, week_zcta()$testrate) %>%
      lapply(htmltools::HTML)
    
    week_zcta() %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(-73.9, 40.7, zoom = 10) %>%
      addPolygons(label = labels,
                  stroke = FALSE,
                  smoothFactor = 0.5,
                  apacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~pal(week_zcta()$testrate),
                  highlightOptions = hilightOptions(weight = 5,
                                                    fillOpacity = 1,
                                                    color = "black",
                                                    apacity = 1,
                                                    bringToFront = TRUE)) %>%
    addLegend("bottomright", 
              pal = pal,
              values = ~testrate,
              title = "Test Per 100,000",
              apacity = 0.8)
  })
  
  output$pctpos <- renderLeaflet({
    pal <- colorBin(palette = "OrRd", 9, domain = all_modzcta$pctpos)
    
    labels <- sprintf(
      "<strong>%s</strong><br/%g> cases per 100,000 people",
      week_zcta()$MODZCTA, week_zcta()$pctpos) %>%
      lapply(htmltools::HTML)
    
    week_zcta() %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(-73.9, 40.7, zoom = 10) %>%
      addPolygons(label = labels,
                  stroke = FALSE,
                  smoothFactor = 0.5,
                  apacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~pal(week_zcta()$pctpos),
                  highlightOptions = hilightOptions(weight = 5,
                                                    fillOpacity = 1,
                                                    color = "black",
                                                    apacity = 1,
                                                    bringToFront = TRUE)) %>%
    addLegend("bottomright", 
              pal = pal,
              values = ~pctpos,
              title = "Test Positive Per 100,000",
              apacity = 0.8)
  })
}

shinyApp(ui, server)