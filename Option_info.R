# Scrape NSE website to extract option chain data 


#libraries 
library(stringr)
library(rvest)
library(httr)
library(jsonlite)
library(lubridate)
library(robotstxt)
library(dplyr)
library(ggplot2)
library(RQuantLib)
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
split_and_transform <-  function(option_table){
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
  option_call_only$'Strike' <- as.numeric(option_table[[1]][[12]][2:table_rows])
  option_transform_tbl <-  cbind(option_call_only,option_put_only)
  return(option_transform_tbl)

}

## select the options that are x percentile above annd below current price; to avoid too OTM options
cal_quantile <- function(data){
  return(quantile(data, probs = c(0.50,0.75), na.rm = TRUE))
}

plot(option_transform_tbl$OI,option_transform_tbl$Strike)

most_traded <- cal_quantile(option_transform_tbl$Strike)
option_plot <- ggplot(data = option_transform_tbl[option_transform_tbl$Strike[option_transform_tbl$Strike < most_traded[[2]]] > most_traded[[1]], 1:12]
                      ,aes(x=Strike , y=LTP)) +geom_line()
option_plot



## Modified Kelly criterian 
## Calculate drawdown/max loss 
{

kelly <- function(odds,win,loss,stake){
  
}

### Side tracked - kelly criteria simulation

trials <- 1000 
periods <-100 # 100 periods (time steps) per simulation
win <- 0.6 # probablity of gain
gain <- 0.2 # gain % if win
loss <- -0.2 # loss % if not win
fractions <- seq(0.2,2,0.2)

set.seed(100)
wealth <- array(data=0, dim = c(trials, length(fractions), periods))
wealth[,,1]=1 # wealth in period 1 is 1

for(trial in 1:trials){
  outcome <- rbinom(n=periods, size=1, prob=win)
  returns <- ifelse(outcome,gain,loss)
  
  for( j in 2:length(returns)){
    for(k in 1:length(fractions)){
      bet <- fractions[k]
    wealth[trial,k,j] <- wealth[trial,k,j-1] * (1+ bet * returns[j])
    }
  }
  
}
}

## end of kelly simulation


## Black Scholes model to calculate greeks [ package: RQuantLib]

current_price <- test.url %>% # Get current underlying price
  read_html() %>%
  html_nodes(xpath='//*[@id="wrapper_btm"]/table[1]') %>%
  html_table(.,fill=TRUE)
current_price <- as.numeric(substring(current_price[[1]][2],25,32))
  
# Get option value and greeks for an option 

option_result <- AmericanOption(option_type, current_price, )









