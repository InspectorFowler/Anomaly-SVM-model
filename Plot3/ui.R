library(shiny)

shinyUI(fluidPage(
  titlePanel(strong(("Net Transactions across Various Metrics"))),
  
  sidebarLayout(
    sidebarPanel(
      helpText("A look into the total financial transactions
               monthwise for each location separated on accounts and
               categories"),
      
      fluidRow(
        
        column(6,
               selectInput("selectloc", 
                           label = strong("Select Location"), 
                           choices = list("Atlantis" = "Atlantis",
                                          "Camelot" = "Camelot",
                                          "Olympus" = "Olympus",
                                          "Total"="Total"),
                           selected = "Atlantis")),
        br()),
      
      fluidRow(
        column(7,
               selectInput("selectAcc", 
                           label = strong("Select Account Description"), 
                           choices = list("Bonus" = "Bonus",
                                          "Chargebacks and Fraud" = "Chargebacks and Fraud",
                                          "City Driven Adv. & Marketing" = "City Driven Adv. & Marketing",
                                          "Commission Driver Payments" = "Commission Driver Payments",
                                          "Contractors & Outside Services" = "Contractors & Outside Services",
                                          "Driver Background Check"="Driver Background Check",
                                          "Facilities, Offices and Equipment"="Facilities, Offices and Equipment",
                                          "Fully Loaded Headcount"="Fully Loaded Headcount",
                                          "Gross Bookings"="Gross Bookings",
                                          "Incentives"="Incentives",
                                          "Legal and Regulatory"="Legal and Regulatory",
                                          "Marketing Credits"="Marketing Credits",
                                          "Payment Card Fees"="Payment Card Fees",
                                          "Promos"="Promos",
                                          "Returns"="Returns",
                                          "Ride Insurance"="Ride Insurance",
                                          "Travel & Entertainment"="Travel & Entertainment"),
                           selected = "Bonus")),
        br()),
      
      fluidRow(
        column(7,
               selectInput("selectCat", 
                           label = strong("Select Category"), 
                           choices = list("Accrual" = "Accrual",
                                          "Adjustment" = "Adjustment",
                                          "Purchase Invoices" = "Purchase Invoices",
                                          "Allocation" = "Allocation",
                                          "Uber Riders Invoices" = "Uber Riders Invoices",
                                          "Uber Riders Receipts" = "Uber Riders Receipts",
                                          "Uber Driver Payments" = "Uber Driver Payments",
                                          "Global Intercompany"="Global Intercompany"),
                           selected = "Adjustment")),
        br()),
      
      helpText(p(span("NOTE :",style="color:red"), em("Accounts and Categories cannot be 
                                                      subsetted to Total counts")))
      
      ),
    mainPanel(
      plotOutput("plot",height=600,width=700)
    )
    )))