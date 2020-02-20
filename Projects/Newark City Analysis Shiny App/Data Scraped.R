


#install.packages(c("httr", "jsonlite", "lubridate"))

library(httr)
library(jsonlite)
library(lubridate)

###### working 2018 ####

url = "http://data.ci.newark.nj.us/mk/api/3/action/datastore_search?resource_id=b923c1ad-7246-400b-a593-785f67677b94&limit=68408"

options(stringsAsFactors = FALSE)

raw.result <- GET(url = url)

this.raw.content <- rawToChar(raw.result$content)

this.content <- fromJSON(this.raw.content)

json_file <- lapply(this.content, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

x2018_part1 = this.content$result$records

dim(x2018_part1)

#############part2 ####################

url = "http://data.ci.newark.nj.us/mk/api/3/action/datastore_search_sql?sql=SELECT%20*%20from%20%22b923c1ad-7246-400b-a593-785f67677b94%22%20WHERE%20_id%20%3E%2031993"

options(stringsAsFactors = FALSE)

raw.result <- GET(url = url)

this.raw.content <- rawToChar(raw.result$content)

this.content <- fromJSON(this.raw.content)

json_file <- lapply(this.content, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

x2018_part2 = this.content$result$records

dim(x2018_part2)


######################


#############part2 ####################

url = "http://data.ci.newark.nj.us/mk/api/3/action/datastore_search_sql?sql=SELECT%20*%20from%20%22b923c1ad-7246-400b-a593-785f67677b94%22%20WHERE%20_id%20%3E%2066495"

options(stringsAsFactors = FALSE)

raw.result <- GET(url = url)

this.raw.content <- rawToChar(raw.result$content)

this.content <- fromJSON(this.raw.content)

json_file <- lapply(this.content, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

x2018_part3 = this.content$result$records

dim(x2018_part3)


######################


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



###### working 2015####

url = "http://data.ci.newark.nj.us/mk/api/3/action/datastore_search?resource_id=bc8e29eb-3c53-418c-a42a-4dbbaae668c2&limit=60000"

options(stringsAsFactors = FALSE)

raw.result <- GET(url = url)

this.raw.content <- rawToChar(raw.result$content)

this.content <- fromJSON(this.raw.content)

json_file <- lapply(this.content, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

x2015 = this.content$result$records

dim(x2015)
######################


###### working Newark Historic ####

url = "http://data.ci.newark.nj.us/mk/api/3/action/datastore_search?resource_id=1cebcf44-3b2c-4f60-9d5d-817cb35bf9df&limit=40000"
raw.result <- GET(url = url)
this.raw.content <- rawToChar(raw.result$content)
this.content <- fromJSON(this.raw.content)
json_file <- lapply(this.content, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

xhistoric = this.content$result$records

dim(xhistoric)
######################
#32000
#url1 = "http://data.ci.newark.nj.us/mk/api/3/action/datastore_search?resource_id=1cebcf44-3b2c-4f60-9d5d-817cb35bf9df&limit=40000"
#31889
#url2 = "http://data.ci.newark.nj.us/mk/api/3/action/datastore_search_sql?sql=SELECT%20*%20from%20%221cebcf44-3b2c-4f60-9d5d-817cb35bf9df%22%20WHERE%20_id%20%3E%2031889"
#63889
#url3 = "http://data.ci.newark.nj.us/mk/api/3/action/datastore_search_sql?sql=SELECT%20*%20from%20%221cebcf44-3b2c-4f60-9d5d-817cb35bf9df%22%20WHERE%20_id%20%3E%2063889"
#95888
#url4 = "http://data.ci.newark.nj.us/mk/api/3/action/datastore_search_sql?sql=SELECT%20*%20from%20%221cebcf44-3b2c-4f60-9d5d-817cb35bf9df%22%20WHERE%20_id%20%3E%2095888"


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
