library(shiny)
library(shinythemes)
library(leaflet)

ui <- fluidPage(theme = shinytheme("readable"),
  title = "Newark 4311 Data Analysis",
  
  headerPanel("Newark 4311 Data Analysis"),
  mainPanel(
    
    tags$p( class="alert alert-dismissible alert-secondary h4",
            "Welcome to the City of Newark's Open Data Portal. Explore, Transform and Share Newark's Data.",
            tags$br(),

            "Our mission is to promote the openness, transparency and accountability of government by providing high-value government data in standards compliant, machine readable format. This will serve as the basis for the creation of useful civic applications by third party developers.",
            tags$br(),
            "Newark City data is now open, freely and easily accessible to all. City data covers general, not personal, information. Other public, private, academic and voluntary organizations are invited to open their data and use this platform to share it. The aim is to let individuals, Small Business and Corporates use the data to bring practical improvements and economic growth to the community."
      
    ),
    
    tabsetPanel(
      id = 'dataset',
      tabPanel("Till 2015", 
               tags$div(class = "h4 text-center" ,
                        tags$br(),
                        tags$br(),
                        " Dataset of Newark Historic Dataset till 2015"),
               DT::dataTableOutput("mytable1"),
               tags$div(class = "h4 text-center" ,
                        tags$br(),
                        tags$br(),
                        "Clusters of the provided locations of service requests"),
               leafletOutput("mapsoutput"),
               tags$div(class = "h4 text-center" ,
                        tags$br(),
                        tags$br(),
                        "Trend of service calls"),
               plotOutput("trendofcalls2015"),
               tags$div(class = "h4 text-center" ,
                        tags$br(),
                        tags$br(),
                        "Location based street analysis"),
               DT::dataTableOutput("mytable5")
               ) ,
      tabPanel("2018-2019", 
               DT::dataTableOutput("mytable2"),
               
               DT::dataTableOutput("mytable1"),
               plotOutput("complaint.typename"),
               DT::dataTableOutput("mytable1"),
               plotOutput("trendofcalls2018"),
               DT::dataTableOutput("mytable1"),
               plotOutput("departments2018"),
               DT::dataTableOutput("mytable4"),
          
               plotOutput("monthlyServiceReq2018")
               
      ),
      tabPanel("2013 - 2019 [Overal Combined]",
               DT::dataTableOutput("mytable3"),
               plotOutput("overall4311compliants"),
               plotOutput("trendofcalls"),
               DT::dataTableOutput("mytable6")
               )
    )
  )
)
