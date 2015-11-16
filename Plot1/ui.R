library(shiny)

shinyUI(fluidPage(
  titlePanel(strong(("Net Transactions across Cities"))),
  
  sidebarLayout(
    sidebarPanel(
      helpText("A look into the total financial transactions
               running through each location in each particular month"),
    
      fluidRow(
        
        column(6,
               selectInput("select", label = strong("Select the Month"), 
                           choices = list("April" = "April", "May" = "May",
                                          "June" = "June","July" = "July",
                                          "August" = "August","September"="September",
                                          "Total"="Total"),selected = "April")),
               br()),
               
      fluidRow(
        column(6,
               checkboxInput("stack", "Stacked representation", value = FALSE)))
    ),
    mainPanel(
      plotOutput("plot",height=600,width=600)
    )
)))