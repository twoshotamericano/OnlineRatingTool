library(shiny)
library(miniUI)
library(leaflet)
library(ggplot2)
library(readr)

data<-read_csv("https://raw.githubusercontent.com/twoshotamericano/OnlineRatingTool/master/Data.csv")
#Here is some text

ui <- miniPage(
  gadgetTitleBar("Combined Care Model"),
  miniTabstripPanel(
    
    miniTabPanel("Search", icon = icon("search"),
                 miniContentPanel(padding=0,
                   DT::dataTableOutput("table",height="100%")
                 )),
    
    miniTabPanel("Risk Info", icon = icon("info-circle"),
                    fillRow(
                     fillCol(
                       textInput("Acc_insured", "Name of Insured", value = "Type here..",width="100%"),
                       dateInput("Acc_incept_date", "Inception Date", value = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en"),
                       dateInput("Acc_quote_date", "Quote Date", value = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en")
                     ),
                     fillCol(
                       selectInput("Acc_Risk_Type", "Risk Type", list("Fostering","Care Home","Secure Facility"), selected = NULL, multiple = FALSE, selectize = TRUE,width="50%"),
                       dateInput("Acc_expiry_date", "Expiry Date", value = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en"),
                       selectInput("Acc_underwriter", "Underwriter", list("Rich","Mark"), selected = NULL, multiple = FALSE, selectize = TRUE,height="100%")
                   )
                   )
                  
                  ,
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
    data[,c(1:5,12)]
  })
  
  observeEvent(input$done, {
    stopApp(TRUE)
  })
}

runGadget(shinyApp(ui, server), viewer = paneViewer())