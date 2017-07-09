library(shiny)
library(miniUI)
library(leaflet)
library(ggplot2)
library(readr)

#Read Data
data<-read_csv("https://raw.githubusercontent.com/twoshotamericano/OnlineRatingTool/master/Data.csv")
parameters<-read.csv("https://raw.githubusercontent.com/twoshotamericano/OnlineRatingTool/master/Parameters.csv",
                     stringsAsFactors = FALSE)
rates<-read.csv("https://raw.githubusercontent.com/twoshotamericano/OnlineRatingTool/master/Rates.csv")

#Create Lists for DropDowns

#Risk Type
List_Acc_Risk_Type<-parameters[parameters$Field.Name=="Acc_Risk_Type",c(2)]
names(List_Acc_Risk_Type)<-parameters[parameters$Field.Name=="Acc_Risk_Type",c(3)]

#Construction Types
List_Construction_Adj<-parameters[parameters$Field.Name=="Prop_Rating_Construction_Adj",c(2)]
names(List_Construction_Adj)<-parameters[parameters$Field.Name=="Prop_Rating_Construction_Adj",c(3)]

#Listed Adjustment
List_Year_Adj<-parameters[parameters$Field.Name=="Prop_Rating_Year_Adj",c(2)]
names(List_Year_Adj)<-parameters[parameters$Field.Name=="Prop_Rating_Year_Adj",c(3)]

#Fire Protection Adjustment
List_FireProtect_Adj<-parameters[parameters$Field.Name=="Prop_Rating_FireProtect_Adj",c(2)]
names(List_FireProtect_Adj)<-parameters[parameters$Field.Name=="Prop_Rating_FireProtect_Adj",c(3)]

#Risk Mgmt Adjustment
List_RM_Adj<-parameters[parameters$Field.Name=="Prop_Rating_RM_Adj",c(2)]
names(List_RM_Adj)<-parameters[parameters$Field.Name=="Prop_Rating_RM_Adj",c(3)]


#Here is some text

ui <- miniPage(
  gadgetTitleBar("Care Model"),
  
  miniTabstripPanel(
    
    miniTabPanel("Search", icon = icon("search"),
                 miniContentPanel(padding=0,
                                  DT::dataTableOutput("table",
                                                      height="100%")
                 )),
    
    miniTabPanel("Risk Info", icon = icon("info-circle"),
                 miniContentPanel(scrollable = TRUE,
                     h4("Risk Information"),
                     textInput("Acc_insured", "Name of Insured", value = "Type here..",width="100%"),
                     selectInput("Acc_Risk_Type", "Risk Type", 
                                 List_Acc_Risk_Type, selected = NULL, multiple = FALSE, selectize = TRUE,width="100%"),
                     dateInput("Acc_incept_date", "Inception Date", value = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en",width="100%"),
                     dateInput("Acc_expiry_date", "Expiry Date", value = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en",width="100%"),
                     dateInput("Acc_quote_date", "Quote Date", value = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en",width="100%"),
                     selectInput("Acc_underwriter", "Underwriter", list("Rich","Mark"), selected = NULL, multiple = FALSE, selectize = TRUE,width="100%")
                   ))
                 
                 ,
                 miniTabPanel("Property Rating", icon = icon("balance-scale"),scrollable=TRUE,
                             miniContentPanel(scrollable = TRUE,
                                  h4("Exposure"),
                                  sliderInput("Prop_Rating_Buildings_Count", "No Properties", min=0, max=15, value=c(1), step=1,width="100%"),
                                  h4("SI Values $000"),
                                  sliderInput("Prop_Rating_SI_Buildings", "Buildings", min=0, max=20000, value=c(0.25,1000), step=1, pre="$",sep = ",",width="100%"),
                                  sliderInput("Prop_Rating_SI_FF", "Fixtures", min=0, max=20000, value=c(0.25,1000), step=1, pre="$",sep = ",",width="100%"),
                                  sliderInput("Prop_Rating_SI_Possessions", "Possessions", min=0, max=20000, value=c(0.25,1000), step=1, pre="$",sep = ",",width="100%"),
                                  sliderInput("Prop_Rating_SI_All_Risks", "All Risks", min=0, max=20000, value=c(0.25,1000), step=1, pre="$",sep = ",",width="100%"),
                                  sliderInput("Prop_Rating_SI_BI", "BI", min=0, max=20000, value=c(0.25,1000), step=1, pre="$",sep = ",",width="100%"),
                                  
                                  h4("Underwriting Factors"),
                                  selectInput("Prop_Rating_Construction_Adj", "Construction Type", 
                                              choices = List_Construction_Adj, 
                                              selected = 1,
                                              width="100%"),
                                  selectInput("Prop_Rating_Year_Adj", "Listed", 
                                              choices = List_Year_Adj, 
                                              selected = 1,
                                              width="100%"),
                                  selectInput("Prop_Rating_FireProtect_Adj", "Fire System", 
                                              choices = List_FireProtect_Adj, 
                                              selected = 1,
                                              width="100%"),
                                  selectInput("Prop_Rating_RM_Adj", label ="Risk Mgmt", 
                                              choices = List_RM_Adj, 
                                              selected = 1,
                                              width="100%")
                                ),
                                  miniButtonBlock(
                                      actionButton("Action", "Calculate Quote")
                                ))
                                
                                
                              
                 ,
                 
                 miniTabPanel("Quote", icon = icon("shopping-cart"),
                              miniContentPanel(
                                textInput("quote_name", "Assured Name", "Initial value")
                                
                              )
                 )
    )
  )
  
  
  server <- function(input, output, session) {
    
    
    quote<-eventReactive(input$Action, {
                        input$n
                                      })
    
    output$table <- DT::renderDataTable({
      data[,c(1,5,12)]
    })
    
    observeEvent(input$done, {
      stopApp(TRUE)
    })
  }
  
  runGadget(shinyApp(ui, server), viewer = paneViewer())