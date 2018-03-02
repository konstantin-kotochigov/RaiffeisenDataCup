library(shiny)

# Define UI for miles per gallon application
shinyUI(fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
  
  textInput("cust_id","Enter Customer number (1-19997)","1"),
  actionButton("prev_button","Prev"),
  actionButton("next_button","Next"),
  checkboxInput("check4","Home (Red)",value=TRUE),
  checkboxInput("check5","Work (Blue)",value=TRUE),
  checkboxInput("check_all", "Show ALL categories", value=TRUE),
  h4("Select categories"),
   checkboxInput("check_6011","ATMs (6011)",value=FALSE),
   checkboxInput("check_5411","Supermarkets (5411)",value=FALSE),
   checkboxInput("check_5814","Fastfood cafes (5814)",value=FALSE),
   checkboxInput("check_5912","Drugstores (5912)",value=FALSE),
   checkboxInput("check_5812","Restaurants (5812)",value=FALSE),
   checkboxInput("check_5541","Service Stations (5541)",value=FALSE),
   checkboxInput("check_5499","Food stores (5499)",value=FALSE),
   checkboxInput("check_4111","Transport (4111)",value=FALSE),
   checkboxInput("check_5691","Clothes (5691)",value=FALSE),
   checkboxInput("check_5977","Cosmetics (5977)",value=FALSE),
   checkboxInput("check_5921","Liqour (5921)",value=FALSE),
   checkboxInput("check_5331","Variety Stores (5331)",value=FALSE),
   checkboxInput("check_5999","Specialty Retail (5999)",value=FALSE),
   checkboxInput("check_5261","Garden (5261)",value=FALSE),
   checkboxInput("check_5661","Shoes (5661)",value=FALSE),
   actionButton("clear_button","Deselect all"),
  actionButton("fill_button","Select all")),
  
  
  mainPanel(
    verbatimTextOutput("info"),
    plotOutput("plot", click="plot_click")
    
  )
  )
  
))
