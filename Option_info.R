# Scrape NSE website to extract option chain data 


#libraries 
library(rvest)
library(httr)
library(jsonlite)
library(lubridate)
library(robotstxt)
library(dplyr)
library(ggplot2)
#Check for permissions

paths_allowed(paths=c("https://www.nseindia.com/live_market/dynaContent/live_watch/option_chain/optionKeys.jsp?symbolCode=-10000&symbol=NIFTY&symbol=NIFTY&instrument=-&date=-&segmentLink=17&symbolCount=2&segmentLink=17"))

test.url <-  "https://www.nseindia.com/live_market/dynaContent/live_watch/option_chain/optionKeys.jsp?symbolCode=-10000&symbol=NIFTY&symbol=NIFTY&instrument=-&date=-&segmentLink=17&symbolCount=2&segmentLink=17"

option_table=list()

option_table <-  test.url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="octable"]')%>%
  html_table(.,fill=TRUE)

#option_table <- lapply(option_table, function(x) { gsub(",","",x)})


#Split calls and puts and transform data for analysis 
split_and_transform = function(option_table){
  table_rows=nrow(option_table[[1]])
  option_call_only=option_table[[1]][names(option_table[[1]]) == "CALLS"] #just calls
  colnames(option_call_only)=option_call_only[1,]
  option_call_only <-  option_call_only[-1,-1]
  option_call_only <-  data.frame(option_call_only)
  option_call_only <- data.frame(lapply(option_call_only,function(x) as.numeric(gsub(",","",x))))
  #option_call_only <- mutate_all(option_call_only, function(x) as.numeric(x))
  option_call_only$option = "CALLS"
  
  option_put_only=option_table[[1]][names(option_table[[1]]) == "PUTS"] #just PUTS
  colnames(option_put_only)=option_put_only[1,]
  option_put_only <-  option_put_only[-1,-11]
  option_put_only <-  data.frame(option_put_only)
  option_put_only <- data.frame(lapply(option_put_only, function(x) as.numeric(gsub(",","",x))))
  #option_put_only <- mutate_all(option_put_only, function(x) as.numeric(x))
  option_put_only <- option_put_only[,c(10,9,8,7,6,5,1,2,3,4)]  
  option_put_only$option = "PUTS"
  colnames(option_put_only) <-  colnames(option_call_only)
  
  #combine calls and puts
  option_call_only$'Strike' <- option_table[[1]][[12]][2:table_rows]
  option_transform_tbl <-  cbind(option_call_only,option_put_only)
  return(option_transform_tbl)

}

plot(option_transform_tbl$IV,option_transform_tbl$Strike)

