library(shiny)
library(shinythemes)



ui<-fluidPage(

  titlePanel("Care Combined Model"),

  sidebarLayout(

    sidebarPanel(
      "Sidebar"
    ),

    mainPanel(
      h3("This is the main panel"),
      textInput(inputId="text1",
                label="Description",
                value="Enter..")

    )
  )

)

server<-function(input,output,session){}

shinyApp(ui=ui,server=server)
