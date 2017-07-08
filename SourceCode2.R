library(shiny)
library(miniUI)
library(leaflet)
library(ggplot2)

#Here is some text

ui <- miniPage(
  gadgetTitleBar("Combined Care Model"),
  miniTabstripPanel(
    
    miniTabPanel("Search", icon = icon("search"),
                 miniContentPanel(
                   DT::dataTableOutput("table")
                 )),
    
    miniTabPanel("Risk Info", icon = icon("info-circle"),
                 miniContentPanel(
                   sliderInput("year", "Year", 1978, 2010, c(2000, 2010), sep = "")
                 )
    ),
    miniTabPanel("Property Rating", icon = icon("balance-scale"),
                 miniContentPanel(
                   sliderInput("year", "Year", 1978, 2010, c(2000, 2010), sep = "")
                 )
    ),
    miniTabPanel("Liability Rating", icon = icon("balance-scale"),
                 miniContentPanel(
                   sliderInput("year", "Year", 1978, 2010, c(2000, 2010), sep = "")
                 )
    ),
    miniTabPanel("Quote", icon = icon("shopping-cart"),
                 miniContentPanel(
                   sliderInput("year", "Year", 1978, 2010, c(2000, 2010), sep = "")
                 )
    )
    )
  )


server <- function(input, output, session) {
  output$cars <- renderPlot({
    require(ggplot2)
    ggplot(cars, aes(speed, dist)) + geom_point()
  })
  
  output$map <- renderLeaflet({
    force(input$resetMap)
    
    leaflet(quakes, height = "100%") %>% addTiles() %>%
      addMarkers(lng = ~long, lat = ~lat)
  })
  
  output$table <- DT::renderDataTable({
    diamonds
  })
  
  observeEvent(input$done, {
    stopApp(TRUE)
  })
}

runGadget(shinyApp(ui, server), viewer = paneViewer())