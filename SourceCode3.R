library(shiny)
library(miniUI)
library(leaflet)
library(ggplot2)
library(readr)
library(DT)

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

#Create Output Table
QuoteNames<-c("Buildings","Fixtures","Possessions","All Risks","BI","Total")


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
                                 List_Acc_Risk_Type, selected = 1, multiple = FALSE, selectize = TRUE,width="100%"),
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
                                h4("Your Quote is"),
                                
                                tableOutput("Quote")
                                
                              )
                 )
    )
  )
  
  
  server <- function(input, output, session) {
    
    
    RatingAdj<-eventReactive(input$Action, {
      
      #Construction Adjustment
      name1<-parameters[parameters$Field.Name=="Prop_Rating_Construction_Adj" & 
                      parameters$Value==input$Prop_Rating_Construction_Adj,3]
      
      adj1<-rates[rates$Values==name1 &
                       rates$Field.Name=="Prop_Rating_Construction_Adj",c(3)]
      
      
      #Prop_Rating_Year_Adj
      name2<-parameters[parameters$Field.Name=="Prop_Rating_Year_Adj" & 
                          parameters$Value==input$Prop_Rating_Year_Adj,3]
      adj2<-rates[rates$Values==name2 &
                    rates$Field.Name=="Prop_Rating_Year_Adj",c(3)]
      
      #Prop_Rating_FireProtect_Adj
      name3<-parameters[parameters$Field.Name=="Prop_Rating_FireProtect_Adj" & 
                          parameters$Value==input$Prop_Rating_FireProtect_Adj,3]
      
      adj3<-rates[rates$Values==name3 &
                    rates$Field.Name=="Prop_Rating_FireProtect_Adj",c(3)]
      
      #Prop_Rating_RM_Adj
      name4<-parameters[parameters$Field.Name=="Prop_Rating_RM_Adj" & 
                          parameters$Value==input$Prop_Rating_RM_Adj,3]
      
      adj4<-rates[rates$Values==name4 &
                    rates$Field.Name=="Prop_Rating_RM_Adj",c(3)]
      
      #Risk Type Adjustment
      name5<-parameters[parameters$Field.Name=="Acc_Risk_Type" & 
                          parameters$Value==input$Acc_Risk_Type,3]
      
      adj5<-rates[rates$Values==name5 &
                    rates$Field.Name=="Acc_Risk_Type",c(3)]
      
      #output Adjustment
      adj<-adj1*adj2*adj3*adj4*adj5
      #print(adj)
      
      
      #Buildings
      Premium_Building<-input$Prop_Rating_Buildings_Count*
        input$Prop_Rating_SI_Buildings[2]*
        rates[rates$Field.Name=="Prop_Rating_SI_Buildings",3]*1000
      #print(Premium_Building)
      
      #FF
      Premium_FF<-input$Prop_Rating_Buildings_Count*
        input$Prop_Rating_SI_FF[2]*
        rates[rates$Field.Name=="Prop_Rating_SI_FF",3]*1000
      #print(Premium_FF)
      
      #Possessions
      Premium_Possessions<-input$Prop_Rating_Buildings_Count*
        input$Prop_Rating_SI_Possessions[2]*
        rates[rates$Field.Name=="Prop_Rating_SI_Possessions",3]*1000
      #print(Premium_Possessions)
      
      #All Risks
      Premium_All_Risks<-input$Prop_Rating_Buildings_Count*
        input$Prop_Rating_SI_All_Risks[2]*
        rates[rates$Field.Name=="Prop_Rating_SI_All_Risks",3]*1000
      #print(Premium_All_Risks)
      
      #BI
      Premium_BI<-input$Prop_Rating_Buildings_Count*
        input$Prop_Rating_SI_BI[2]*
        rates[rates$Field.Name=="Prop_Rating_BI",3]*1000
      #print(Premium_BI)
      
      #Total
      Premium_Total<-Premium_Building+Premium_Possessions+Premium_FF+Premium_All_Risks+Premium_BI
      
      
      #QuoteSummary
      QuoteSummary<-data.frame("Type"=rep("A",6),"Quote"=rep(0,6),"SI"=rep(0,6),"Ded"=rep(0,6))
      
      QuoteSummary$Type<-QuoteNames
      QuoteSummary$Quote<-c(Premium_Building,Premium_FF,Premium_Possessions,Premium_All_Risks,Premium_BI,Premium_Total)*adj
      QuoteSummary$SI<-c(input$Prop_Rating_SI_Buildings[2],input$Prop_Rating_SI_FF[2],input$Prop_Rating_SI_Possessions[2],
        input$Prop_Rating_SI_All_Risks[2],input$Prop_Rating_SI_BI[2],"")
      QuoteSummary$Ded<-c(input$Prop_Rating_SI_Buildings[1]*1000,input$Prop_Rating_SI_FF[1]*1000,input$Prop_Rating_SI_Possessions[1]*1000,
        input$Prop_Rating_SI_All_Risks[1]*1000,input$Prop_Rating_SI_BI[1]*1000,"")
      #print(Quote)
      #QuoteSummary<-data.frame(Quote=rep(0,6),SI=rep(0,6),Ded=rep(0,6))
      #Quote
      #row.names(QuoteSummary)<-QuoteNames
      
      QuoteSummary
      
      })
    
    
    output$Quote<-renderTable({RatingAdj()})
    
    output$table <- DT::renderDataTable({
      data[,c(1,5)]
    })
    
    observeEvent(input$done, {
      stopApp(TRUE)
    })
  }
  
  runGadget(shinyApp(ui, server), viewer = paneViewer())