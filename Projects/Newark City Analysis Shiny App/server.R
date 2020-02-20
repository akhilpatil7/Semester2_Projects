library(shiny)
library(ggplot2)
# Required libs
library(httr)
library(jsonlite)
library(lubridate)
library(tidyverse)
library(stringr)
library(leaflet)
library(rpart)
library(DT)
library(forecast)
library(rpart.plot)
library(caret)


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

overall4311 = rbind(till.2015[,c(1,3,6)], x2018[,c(1,4,2)])

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
                          

locationAnalysisOverall =   na.omit(overall4311) %>%
  group_by(Location) %>%
  summarise(Count = length(Location)) %>%
  arrange(desc(Count), .by_group = TRUE)

Newark4311TrendData = overall4311 %>%
  mutate(year = format(DateCreated, format="%Y") ) %>%
  mutate(month = format(DateCreated, format="%m")) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(month)) %>%
  group_by(year,month) %>%
  summarise(Count = n()) %>%
  arrange(year,month)

tsNewark4311TrendData = ts(Newark4311TrendData)


server <- function(input, output) {
  
  
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(till.2015, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
  # sorted columns are colored now because CSS are attached to them
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(x2018, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
    
  })
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$mytable3 <- DT::renderDataTable({
    DT::datatable(overall4311, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
  
  ## 2018 Output
  
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
  
  
  ### 2015 outputs
  
  output$mapsoutput = renderLeaflet({
    
    till.2015 %>% leaflet() %>%addProviderTiles("Esri.NatGeoWorldMap") %>%
      addMarkers(lng = ~Longitude, lat = ~Latitude,clusterOptions = markerClusterOptions()) %>%
      # controls
      setView(lng=center_lon, lat=center_lat,zoom = 12) 
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
    
    till.2015 %>%
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
  
  ## Overall
  
  
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
    
    overall4311 %>%
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
  
}