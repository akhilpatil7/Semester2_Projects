library(shiny)
library(ggplot2)
library(httr)
library(jsonlite)
library(lubridate)
library(tidyverse)
library(stringr)
library(leaflet)
library(shinythemes)
library(rpart)
library(DT)
library(forecast)
library(rpart.plot)
library(caret)
library(leaflet.extras)


#Parsing Newark Historic data
urls.hist = c("http://data.ci.newark.nj.us/mk/api/3/action/datastore_search?resource_id=1cebcf44-3b2c-4f60-9d5d-817cb35bf9df&limit=40000",
              "http://data.ci.newark.nj.us/mk/api/3/action/datastore_search_sql?sql=SELECT%20*%20from%20%221cebcf44-3b2c-4f60-9d5d-817cb35bf9df%22%20WHERE%20_id%20%3E%2031889",
              "http://data.ci.newark.nj.us/mk/api/3/action/datastore_search_sql?sql=SELECT%20*%20from%20%221cebcf44-3b2c-4f60-9d5d-817cb35bf9df%22%20WHERE%20_id%20%3E%2063889",
              "http://data.ci.newark.nj.us/mk/api/3/action/datastore_search_sql?sql=SELECT%20*%20from%20%221cebcf44-3b2c-4f60-9d5d-817cb35bf9df%22%20WHERE%20_id%20%3E%2095888")
xhist = data.frame()

for(i in urls.hist ){
  raw.result <- GET(url = i)
  this.raw.content <- rawToChar(raw.result$content)
  this.content <- fromJSON(this.raw.content)
  json_file <- lapply(this.content, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
  })
  
  xhistoric = this.content$result$records
  xhistoric = xhistoric[,c("Type Name","Type Id","Street Address" , "Longitude","Request Id","Request Date", "Latitude" , "_id" ,"ID")]
  xhist = rbind(xhist,xhistoric)
}

url = "http://data.ci.newark.nj.us/mk/api/3/action/datastore_search?resource_id=bc8e29eb-3c53-418c-a42a-4dbbaae668c2&limit=60000"

raw.result <- GET(url = url)
this.raw.content <- rawToChar(raw.result$content)
this.content <- fromJSON(this.raw.content)

json_file <- lapply(this.content, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

x2015 = this.content$result$records


urls.2018 = c("http://data.ci.newark.nj.us/mk/api/3/action/datastore_search?resource_id=b923c1ad-7246-400b-a593-785f67677b94&limit=68408",
              "http://data.ci.newark.nj.us/mk/api/3/action/datastore_search_sql?sql=SELECT%20*%20from%20%22b923c1ad-7246-400b-a593-785f67677b94%22%20WHERE%20_id%20%3E%2031993",
              "http://data.ci.newark.nj.us/mk/api/3/action/datastore_search_sql?sql=SELECT%20*%20from%20%22b923c1ad-7246-400b-a593-785f67677b94%22%20WHERE%20_id%20%3E%2066495")
x2018 = data.frame()

for(i in urls.2018 ){
  raw.result <- GET(url = i)
  this.raw.content <- rawToChar(raw.result$content)
  this.content <- fromJSON(this.raw.content)
  json_file <- lapply(this.content, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
  })
  
  xtemp = this.content$result$records
  xtemp = xtemp[,c("Complaint","DateCreated","Typename","Location","Lot", "ComplaintID","Department" ,"_id" ,"Block")]
  x2018 = rbind(x2018,xtemp)
}


Geocoded2018 <- read.csv("https://raw.githubusercontent.com/akhilpatil7/Datasets/master/Geocoded2018.csv")


x2015$`Street Address` = paste(x2015$`Street Number`,x2015$`Street Name`)

x2015$`Street Name` = NULL
x2015$`Street Number` = NULL

x2015$`Street Address`[x2015$`Street Address` == " "] = NA
xhist[xhist == " "] = NA


xhist$`Request Date` = as.POSIXct(xhist$`Request Date`)

x2015$`Request Date` = "2015"
x2015$`Request Date` = as.Date(x2015$`Request Date`,format = "%Y")


x2018$DateCreated = as.POSIXct(x2018$DateCreated, format = "%b %d %Y")

till.2015 = rbind(xhist,x2015)
names(till.2015)[1:3]<-c("Complaint","ComplaintID","Location")
names(till.2015)[6]<-c("DateCreated")
names(xhist)[6]= "DateCreated"


overall4311 = rbind(till.2015[,c(1,3,6)], x2018[,c(1,4,2)])
overall4311trendofcalls= as.data.frame( c(xhist$DateCreated,x2018$DateCreated))
names(overall4311trendofcalls) = "DateCreated"

fillColor = "#FFA07A"
fillColor2 = "#F1C40F"

till.2015$Longitude = as.numeric(till.2015$Longitude)
till.2015$Latitude =  as.numeric(till.2015$Latitude)
center_lon = median(till.2015$Longitude,na.rm = TRUE)
center_lat = median(till.2015$Latitude,na.rm = TRUE)

locationAnalysis2018 = na.omit(x2018) %>%
  group_by(Location) %>%
  summarise(Count = length(Location)) %>%
  arrange(desc(Count), .by_group = TRUE)



locationAnalysis2015 =   na.omit(till.2015) %>%
  group_by(Location) %>%
  summarise(Count = length(Location)) %>%
  arrange(desc(Count), .by_group = TRUE)

locationAnalysisOverall = na.omit(overall4311) %>%
  group_by(Location) %>%
  summarise(Count = length(Location)) %>%
  arrange(desc(Count), .by_group = TRUE)

# Trend of Calls Overall 

Newark4311TrendData = overall4311trendofcalls %>%
  mutate(year = format(DateCreated, format="%Y") ) %>%
  mutate(month = format(DateCreated, format="%m")) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(month)) %>%
  group_by(year,month) %>%
  summarise(Count = n()) %>%
  arrange(year,month)

tsNewark4311TrendData = ts(Newark4311TrendData)

fit.overall <- auto.arima(tsNewark4311TrendData[,3])

preds = forecast(fit.overall, h = 5)


# Trend of calls till 2015
Newark4311TrendData2015 = xhist %>%
  mutate(year = format(DateCreated, format="%Y") ) %>%
  mutate(month = format(DateCreated, format="%m")) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(month)) %>%
  group_by(year,month) %>%
  summarise(Count = n()) %>%
  arrange(year,month)

tsNewark4311TrendData2015 = ts(Newark4311TrendData2015)

fit.2015 <- auto.arima(tsNewark4311TrendData2015[,3])

preds.2015 = forecast(fit.2015, h = 5)


# plotting the circles on Latitude and longitudes

Newark4311SampleAll = till.2015 %>%
  filter(!is.na(Latitude) ) %>%
  filter(!is.na(Longitude)) 





############################# SERVER ###################

server <- function(input, output,session) {
  
  
  output$mytable7 <- DT::renderDataTable({
    DT::datatable(till.2015, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
  output$mytable8 <- DT::renderDataTable({
    DT::datatable(x2018, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
    
  })
  
  output$mytable3 <- DT::renderDataTable({
    DT::datatable(overall4311, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
  
  
  ################# 2018 Dataset ##########################
  
  output$complaint.typename = renderPlot({
    
    x2018 %>%
      group_by(Typename) %>%
      summarise(Count = n()) %>%
      ungroup() %>%
      mutate(complaint_description = reorder(Typename,Count)) %>%
      arrange(desc(Count)) %>%
      head(10) %>%
      
      ggplot(aes(x = reorder(Typename,Count),y = Count )) +
      geom_bar(stat='identity',colour="white", fill = fillColor2) +
      geom_text(aes(x = complaint_description, y = 1, label = paste0("(",Count,")",sep="")),
                hjust=0, vjust=.5, size = 4, colour = 'black',
                fontface = 'bold') +
      labs(x = 'Complaint Descriptors', 
           y = 'Count', 
           title = 'Highest Complaint Typename 2018') +
      coord_flip() + 
      theme_bw()
  })
  
  ## trend of calls
  
  output$trendofcalls2018 = renderPlot({
    
    x2018 %>%
      mutate(year = format(DateCreated, format="%Y") )%>%
      mutate(month = format(DateCreated, format="%m")) %>%
      filter(!is.na(year)) %>%
      filter(!is.na(month)) %>%
      group_by(year,month) %>%
      summarise(Count = n()) %>%
      arrange(year,month) %>%
      mutate(YearMonth = make_date(year=year,month=month) ) %>%
      
      
      ggplot(aes(x=YearMonth,y=Count)) +
      geom_line(size=1, color="red")+
      geom_point(size=3, color="red") +
      labs(x = 'Time', y = 'Count',title = 'Trend of 4311 Calls') +
      theme_bw()
    
  })
  
  output$departments2018 = renderPlot({
    x2018 %>%
      group_by(Typename,Department) %>%
      summarise(Count = n()) %>%
      ungroup() %>%
      mutate(complaint_description = reorder(Department,Count)) %>%
      arrange(desc(Count), .by_group = TRUE) %>%
      head(50) %>%
      
      ggplot(aes(x = reorder(Department,Count),y = Count)) +
      geom_bar(stat='identity',colour="white", fill = fillColor) +
      labs(x = 'Agencies', 
           y = 'Complaint Count', 
           title = 'Number of Comlpaints With Agency') +
      coord_flip() + 
      theme_bw()
    
  })
  
  
  
  output$mytable4 <- DT::renderDataTable({
    DT::datatable(locationAnalysis2018, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
  
  #monthly service requests
  
  output$monthlyServiceReq2018 = renderPlot({
    x2018 %>%
      mutate(month = format(DateCreated, format="%m")) %>%
      filter(!is.na(month)) %>%
      group_by(month) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) %>%
      ungroup() %>%
      mutate(month = reorder(month,Count)) %>%
      
      ggplot(aes(x = month,y = Count)) +
      geom_bar(stat='identity',colour="white", fill = fillColor2) +
      geom_text(aes(x = month, y = 1, label = paste0("(",Count,")",sep="")),
                hjust=0, vjust=.5, size = 4, colour = 'black',
                fontface = 'bold') +
      labs(x = 'Month', 
           y = 'Count', 
           title = 'Months with service requests counts') +
      coord_flip() + 
      theme_bw()
  })
  
  ## cluster map output
  
output$mapsoutput2018 = renderLeaflet({
    
  filtered = reactive({  
      Geocoded2018 %>%
        filter(
           address_type == input$locationfilter
        )
    })
    
    leaflet() %>%addProviderTiles("Esri.NatGeoWorldMap") %>%
      addCircles(data = filtered(),lng = ~long, lat = ~lat,color = ~c("red")) %>%
      # controls
      setView(lng=center_lon, lat=center_lat,zoom = 12) %>%
      addResetMapButton()
  })
  
  
  ################# 2015 Dataset ##########################
  
  output$mapsoutput = renderLeaflet({
    
    till.2015 %>% leaflet() %>%addProviderTiles("Esri.NatGeoWorldMap") %>%
      addMarkers(lng = ~Longitude, lat = ~Latitude,clusterOptions = markerClusterOptions()) %>%
      # controls
      setView(lng=center_lon, lat=center_lat,zoom = 12) %>%
      addResetMapButton()
  })
  
  
  output$mytable5 <- DT::renderDataTable({
    DT::datatable(locationAnalysis2015, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
  
  
  output$monthlyServiceReq2015 = renderPlot({
    till.2015 %>%
      mutate(month = format(DateCreated, format="%m")) %>%
      filter(!is.na(month)) %>%
      group_by(month) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) %>%
      ungroup() %>%
      mutate(month = reorder(month,Count)) %>%
      
      ggplot(aes(x = month,y = Count)) +
      geom_bar(stat='identity',colour="white", fill = fillColor2) +
      geom_text(aes(x = month, y = 1, label = paste0("(",Count,")",sep="")),
                hjust=0, vjust=.5, size = 4, colour = 'black',
                fontface = 'bold') +
      labs(x = 'Month', 
           y = 'Count', 
           title = 'Months with service requests counts') +
      coord_flip() + 
      theme_bw()
  })
  
  ## trend of calls
  
  output$trendofcalls2015 = renderPlot({
    
    xhist %>%
      mutate(year = format(DateCreated, format="%Y") )%>%
      mutate(month = format(DateCreated, format="%m")) %>%
      filter(!is.na(year)) %>%
      filter(!is.na(month)) %>%
      group_by(year,month) %>%
      summarise(Count = n()) %>%
      arrange(year,month) %>%
      mutate(YearMonth = make_date(year=year,month=month) ) %>%
      
      
      ggplot(aes(x=YearMonth,y=Count)) +
      geom_line(size=1, color="red")+
      geom_point(size=3, color="red") +
      labs(x = 'Time', y = 'Count',title = 'Trend of 4311 Calls') +
      theme_bw()
    
  })
  
  output$fit.trend2015 = renderPlot({
    preds.2015 %>% autoplot(include=40) +theme_bw()
  })
  
  output$plotmaplatlong = renderLeaflet({
    
    
    filtered = reactive({  
      Newark4311SampleAll %>%
        filter(
          Complaint == input$top10complaints
        )
    })
    
    leaflet() %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
      
      addCircles(data = filtered(),lng = ~Longitude, lat = ~Latitude, 
                 color = ~c("red"))  %>%
      
      # controls
      setView(lng=center_lon, lat=center_lat,zoom = 15) %>%
      addResetMapButton() 
    
  })
  
  
  output$heatmap2015 = renderLeaflet({
    leaflet() %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
      setView(lng=center_lon, lat=center_lat,zoom = 17) %>%
      addHeatmap( data = Newark4311SampleAll,
                  lng = ~Longitude, lat = ~Latitude, 
                  blur = 20, max = 0.05, radius = 15
      )%>%
      addResetMapButton()
    
  })
  
  
  
  
  ################# Overall4311 Dataset ##########################
  
  
  
  output$mytable6 <- DT::renderDataTable({
    DT::datatable(Newark4311TrendData, options = list(lengthMenu = c(12, 24, 48), pageLength = 12))
  })
  
  
  output$overall4311compliants = renderPlot({
    
    overall4311 %>%
      group_by(Complaint) %>%
      summarise(Count = n()) %>%
      ungroup() %>%
      mutate(complaint_description = reorder(Complaint,Count)) %>%
      arrange(desc(Count)) %>%
      head(20) %>%
      
      ggplot(aes(x = reorder(Complaint,Count),y = Count )) +
      geom_bar(stat='identity',colour="white", fill = fillColor2) +
      geom_text(aes(x = complaint_description, y = 1, label = paste0("(",Count,")",sep="")),
                hjust=0, vjust=.5, size = 4, colour = 'black',
                fontface = 'bold') +
      labs(x = 'Complaint Descriptors', 
           y = 'Count', 
           title = 'Highest Complaint Typename') +
      coord_flip() + 
      theme_bw()
    
  })
  
  output$trendofcalls = renderPlot({
    
    overall4311trendofcalls %>%
      mutate(year = format(DateCreated, format="%Y") )%>%
      mutate(month = format(DateCreated, format="%m")) %>%
      filter(!is.na(year)) %>%
      filter(!is.na(month)) %>%
      group_by(year,month) %>%
      summarise(Count = n()) %>%
      arrange(year,month) %>%
      mutate(YearMonth = make_date(year=year,month=month) ) %>%
      
      
      ggplot(aes(x=YearMonth,y=Count)) +
      geom_line(size=1, color="red")+
      geom_point(size=3, color="red") +
      labs(x = 'Time', y = 'Count',title = 'Trend of 4311 Calls') +
      theme_bw()
    
  })
  
  output$mytable6 <- DT::renderDataTable({
    DT::datatable(locationAnalysisOverall, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
  output$fit.overall4311 = renderPlot({
    preds %>% autoplot(include=40) +theme_bw()
  })
  
  
  
  
}

complaints.list = unique(till.2015$Complaint)
location.list = unique(Geocoded2018$address_type)

################# UI ##########################


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
                  
                  navbarPage("Newark 4311 Data",
                             tabPanel("Till 2015", 
                                      tags$div(class = "h4 text-center" ,
                                               tags$br(),
                                               tags$br(),
                                               " Dataset of Newark Historic Dataset till 2015"),
                                      DT::dataTableOutput("mytable7"),
                                      tags$div(class = "h4 text-center" ,
                                               tags$br(),
                                               tags$br(),
                                               "Clusters of the provided locations of service requests"),
                                      fluidRow(
                                        
                                        column(8,
                                               leafletOutput("mapsoutput")
                                        ),
                                        
                                        column(4,
                                               tags$div(
                                                 tags$br(),
                                                 tags$br(),
                                                 "Filter",
                                                 tags$br(),
                                                 
                                                 selectizeInput(
                                                   'locationfilter', label = "Locations:", choices = location.list,
                                                   options = list(create = TRUE,placeholder = 'select or search Locations',maxItems = 4)
                                                 )
                                               )
                                          )
                                        ),
                                      
                                      
                                       
                                      
                                      tags$div(class = "h4 text-center" ,
                                               tags$br(),
                                               tags$br(),
                                               "Plot of Requests"),
                                      
                                      fluidRow(
                                        
                                        column(8,
                                               leafletOutput("plotmaplatlong")
                                        ),
                                        
                                        column(4,
                                               tags$div(
                                                        tags$br(),
                                                        tags$br(),
                                                        "Filter",
                                                        tags$br(),
                                                        
                                                        
                                                        # radioButtons("top10complaints", "Complaint type:",
                                                        #              c("(pol) Police Non-emergency Transfers",
                                                        #                "(nrs) Code Enforcement Inspection Requests",
                                                        #                "(nrs) Sanitation and Recyling General Information",
                                                        #                "(nrs) Recycling Missed Pick-up",
                                                        #                "(eng) Potholes",
                                                        #                "(nrs) Garbage/Debris On Property",
                                                        #                "(nrs) Sanitation Missed Pick-up",
                                                        #                "(nrs) Neighborhood Services - General Information",
                                                        #                "(oa) Outside Agency - General Information",
                                                        #                "(nrs) Illegal Dumping")
                                                        #              
                                                        # )
                                                        
                                                        
                                                        selectizeInput(
                                                          'top10complaints', label = "Complaint type:", choices = complaints.list,
                                                          options = list(create = TRUE,placeholder = 'select or search complaints',maxItems = 4)
                                                        )
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                               )
                                               
                                        )
                                      ),
                                      
                                      
                                      
                                      
                                      tags$div(class = "h4 text-center" ,
                                               tags$br(),
                                               tags$br(),
                                               "HeatMap of Requests"),
                                      leafletOutput("heatmap2015"),
                                      
                                      
                                      tags$div(class = "h4 text-center" ,
                                               tags$br(),
                                               tags$br(),
                                               "Trend of service calls"),
                                      
                                      
                                      tabsetPanel(
                                        tabPanel("Trend of Calls",
                                                 plotOutput("trendofcalls2015")
                                        ),
                                        tabPanel("Arima Forecast",
                                                 plotOutput("fit.trend2015"))
                                        
                                      ),
                                      
                                      
                                      
                                      tags$div(class = "h4 text-center" ,
                                               tags$br(),
                                               tags$br(),
                                               "Location based street analysis"),
                                      
                                      fluidRow(
                                        
                                        column(10,
                                               DT::dataTableOutput("mytable5")
                                        ),
                                        
                                        column(2,
                                               tags$div(class = "h5 text-center" ,
                                                        tags$br(),
                                                        tags$br(),
                                                        "Observations",
                                                        tags$br(),
                                                        "Broad Street has too many complaints which needs to given special attention")
                                               
                                        )
                                      )
                                      
                             ) ,
                             tabPanel("2018-2019", 
                                      tags$div(class = "h4 text-center" ,
                                               tags$br(),
                                               tags$br(),
                                               " Dataset of Newark for 2018-19"),
                                      DT::dataTableOutput("mytable8"),
                                      tags$div(class = "h4 text-center" ,
                                               tags$br(),
                                               tags$br(),
                                               "Highest Occuring Service Request"),
                                      plotOutput("complaint.typename"),
                                      tags$div(class = "h4 text-center" ,
                                               tags$br(),
                                               tags$br(),
                                               "Trend of Calls"),
                                      plotOutput("trendofcalls2018"),
                                      tags$div(class = "h4 text-center" ,
                                               tags$br(),
                                               tags$br(),
                                               "Busiest Departments getting highest service requests"),
                                      plotOutput("departments2018"),
                                      tags$div(class = "h4 text-center" ,
                                               tags$br(),
                                               tags$br(),
                                               "Location Based Street Analysis"),
                                      DT::dataTableOutput("mytable4"),
                                      tags$div(class = "h4 text-center" ,
                                               tags$br(),
                                               tags$br(),
                                               "Service requests per Month"),
                                      plotOutput("monthlyServiceReq2018"),
                                      
                                      tags$div(class = "h4 text-center" ,
                                               tags$br(),
                                               tags$br(),
                                               "Clusters of the provided locations of service requests"),
                                      fluidRow(
                                        
                                        column(8,
                                               leafletOutput("mapsoutput2018")
                                        ),
                                        
                                        column(4,
                                               tags$div(
                                                 tags$br(),
                                                 tags$br(),
                                                 "Filter",
                                                 tags$br(),
                                                 
                                                 selectizeInput(
                                                   'locationfilter', label = "Locations:", choices = location.list,
                                                   options = list(create = TRUE,placeholder = 'select or search Locations',maxItems = 4)
                                                 )
                                               )
                                        )
                                      )
                                      
                             ),
                             tabPanel("2013 - 2019 [Overal Combined]",
                                      tags$div(class = "h4 text-center" ,
                                               tags$br(),
                                               tags$br(),
                                               " Dataset of Newark for 2013-19[Merged]"),
                                      DT::dataTableOutput("mytable3"),
                                      tags$div(class = "h4 text-center" ,
                                               tags$br(),
                                               tags$br(),
                                               "Highest Occuring Service Request"),
                                      plotOutput("overall4311compliants"),
                                      
                                      
                                      
                                      tags$div(class = "h4 text-center" ,
                                               tags$br(),
                                               tags$br(),
                                               "Trend of Calls"),
                                      
                                      tabsetPanel(
                                        tabPanel("Trend of Calls",
                                                 plotOutput("trendofcalls")
                                        ),
                                        tabPanel("Arima Forecast",
                                                 plotOutput("fit.overall4311"))
                                        
                                      ),
                                      tags$div(class = "h4 text-center" ,
                                               tags$br(),
                                               tags$br(),
                                               "Location Based Street Analysis"),
                                      DT::dataTableOutput("mytable6")
                                      
                                      
                                      
                             )
                             
                             
                  )
                )
)



shinyApp(ui, server)