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
                                  DT::dataTableOutput("table",
                                                      height="100%")
                 )),
    
    miniTabPanel("Risk Info", icon = icon("info-circle"),
                 miniContentPanel(scrollable = TRUE,
                     h3("Risk Information"),
                     textInput("Acc_insured", "Name of Insured", value = "Type here..",width="100%"),
                     selectInput("Acc_Risk_Type", "Risk Type", list("Fostering","Care Home","Secure Facility"), selected = NULL, multiple = FALSE, selectize = TRUE,width="100%"),
                     dateInput("Acc_incept_date", "Inception Date", value = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en",width="100%"),
                     dateInput("Acc_expiry_date", "Expiry Date", value = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en",width="100%"),
                     dateInput("Acc_quote_date", "Quote Date", value = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en",width="100%"),
                     selectInput("Acc_underwriter", "Underwriter", list("Rich","Mark"), selected = NULL, multiple = FALSE, selectize = TRUE,width="100%")
                   ))
                 
                 ,
                 miniTabPanel("Property Rating", icon = icon("balance-scale"),scrollable=TRUE,
                             miniContentPanel(scrollable = TRUE,
                                  h3("Exposure"),
                                  sliderInput("Prop_Rating_Buildings_Count", "No Properties", min=0, max=15, value=c(1), step=1,width="100%"),
                                  h3("SI Values $000"),
                                  sliderInput("Prop_Rating_SI_Buildings", "Buildings", min=0, max=20000, value=c(0.25,1000), step=1, pre="$",sep = ",",width="100%"),
                                  sliderInput("Prop_Rating_SI_FF", "Fixtures", min=0, max=20000, value=c(0.25,1000), step=1, pre="$",sep = ",",width="100%"),
                                  sliderInput("Prop_Rating_SI_Possessions", "Possessions", min=0, max=20000, value=c(0.25,1000), step=1, pre="$",sep = ",",width="100%"),
                                  sliderInput("Prop_Rating_SI_All_Risks", "All Risks", min=0, max=20000, value=c(0.25,1000), step=1, pre="$",sep = ",",width="100%"),
                                  sliderInput("Prop_Rating_SI_BI", "BI", min=0, max=20000, value=c(0.25,1000), step=1, pre="$",sep = ",",width="100%"),
                                  
                                  h3("Underwriting Factors"),
                                  selectInput("Prop_Rating_Construction_Adj", "Construction Type", 
                                              choices = list("Combustible" = 1, "Composite" = 2, "Other" = 3), 
                                              selected = 1,
                                              width="100%"),
                                  selectInput("Prop_Rating_Year_Adj", "Listed", 
                                              choices = list("Yes" = 1, "No" = 2), 
                                              selected = 1,
                                              width="100%"),
                                  selectInput("Prop_Rating_FireProtect_Adj", "Fire System", 
                                              choices = list("Yes" = 1, "No" = 2), 
                                              selected = 1,
                                              width="100%"),
                                  selectInput("Prop_Rating_RM_Adj", label ="Risk Mgmt", 
                                              choices = list("Good" = 1, "OK" = 2,"Bad"=3), 
                                              selected = 1,
                                              width="100%")
                                ))
                                
                                
                              
                 ,
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
    
    
    output$table <- DT::renderDataTable({
      data[,c(1:5,12)]
    })
    
    observeEvent(input$done, {
      stopApp(TRUE)
    })
  }
  
  runGadget(shinyApp(ui, server), viewer = paneViewer())