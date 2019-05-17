##download dta afrom morningstar 

require(RCurl)
require(jsonlite)
library(rvest)
library(httr)
library(lubridate)


url.screener<-"https://www.screener.in/company/"

#NSE codes scrips whatever you call it
NSE_CODES=read.csv("C:/Users/ashivkumar/Documents/temp/Finfun/NSE_Codes.csv",header=TRUE, stringsAsFactors = FALSE)
NSE_symbols_only=NSE_CODES$Symbol[1:10]
#colnames(NSE_CODES)=c("Codes","Company")


get_incomestat=function(){
NSE500_fund=list()
NSE_symbols_only=NSE_CODES$Symbol[10:20]
for(t in 1:length(NSE_symbols_only)){
  myticker=NSE_symbols_only[t]
 
# Fundamentals 
str.keyratios<-getURL("https://financials.morningstar.com/finan/ajax/exportKR2CSV.html?&callback=?&t=0P0000BP80&region=ind&culture=en-US&version=SAL&cur=&order=asc"); 
if(length(str.keyratios)!= 0){
kr.fin <- sub(".*Financials\n(.*)Key Ratios -> Profitability.*","\\1",str.keyratios)
kr.margins <- sub(".*Key Ratios -> Profitability\n(.*)Profitability.*","\\1",str.keyratios)
kr.profit <- sub(".*Key Ratios -> Profitability.*(Profitability.*)Key Ratios -> Growth.*","\\1",str.keyratios)
kr.growth<-sub(".*Key Ratios -> Growth\n(.*)Key Ratios -> Cash Flow.*","\\1",str.keyratios)
kr.cashflow<-sub(".*Key Ratios -> Cash Flow\n(.*)Key Ratios -> Financial Health.*","\\1",str.keyratios)
kr.balance<-sub(".*Key Ratios -> Financial Health\n(Balance Sheet Items.*)Liquidity/Financial Health.*","\\1",str.keyratios)
kr.liquid<-sub(".*Key Ratios -> Financial Health.*(Liquidity/Financial Health.*)Key Ratios -> Efficiency Ratios.*","\\1",str.keyratios)
kr.eff<-sub(".*Key Ratios -> Efficiency Ratios\n(.*)","\\1",str.keyratios)

NSE500_fund[[NSE_symbols_only[t]]][["fin"]]= data.frame(read.csv(textConnection(kr.fin),stringsAsFactors = FALSE))
NSE500_fund[[NSE_symbols_only[t]]][["margins"]]= read.csv(textConnection(kr.margins))
NSE500_fund[[NSE_symbols_only[t]]][["profit"]]= read.csv(textConnection(kr.profit))
NSE500_fund[[NSE_symbols_only[t]]][["growth"]]= read.csv(textConnection(kr.growth))
NSE500_fund[[NSE_symbols_only[t]]][["cashflow"]]= read.csv(textConnection(kr.cashflow))
NSE500_fund[[NSE_symbols_only[t]]][["balance"]]= read.csv(textConnection(kr.balance))
NSE500_fund[[NSE_symbols_only[t]]][["liquid"]]= read.csv(textConnection(kr.liquid))
NSE500_fund[[NSE_symbols_only[t]]][["eff"]]= read.csv(textConnection(kr.eff))
}
Sys.sleep(10)
}
return(NSE500_fund)
}




#si=html_session("https://www.screener.in/company/3MINDIA/")

myticker <- "3MINIDIA"
for(t in 1:length(NSE_symbols_only)){
  myticker=NSE_symbols_only[t]
  url=paste0(url.screener, myticker)
  bal_sheet=get_balsheet()
  }

url=paste0(url.screener, myticker)

count_years=function(table){
  col_count=ncol(table)
  years_count=col_count-1
  count_char= nchar(colnames(table)[ncol(table)])
  extract_year= as.numeric(substring(colnames(test_cashflow)[ncol(test_cashflow)],check_char-3, check_char))
  if(extract_year== (year(Sys.Date())-1)){
    count_years_table=seq(year(Sys.Date())-(years_count),year(Sys.Date())-1)
  } else count_years_table=seq(year(Sys.Date())-(years_count-1),year(Sys.Date()))
  #if(table[1,1] == "Sales")
        return(count_years_table)
} # used to count #years in the table --used in transform_table()

transform_table=function(table){
  table_before=table
  row_names=table_before[,1]
  table_before=data.frame(lapply(table_before, function(x) { as.numeric(as.character(gsub("%","",gsub(",","",x))))}))
  table_after=data.frame(t(table_before)); table_after=table_after[-1,];
  colnames(table_after)=row_names
  rownames(table_after)=count_years(table)
  return(table_after[-1,])
} # transform table - columns = parameters; rows= years; rownames =years

#test
test_transform=transform_table(test_inc)

pull_url <- function(myticker){
  
  url.screener<-"https://www.screener.in/company/"
  url=paste0(url.screener, myticker)
  return(url)
  }

## pull fin statements ##

get_incomestat=function(myticker){
    url=pull_url(myticker)
    table_incomestat=list()
    table_incomestat[[myticker]]=url %>% #pipeline
    read_html() %>%
    html_nodes(xpath='//*[@id="profit-loss"]/div[1]/table')%>%
    html_table(.,fill=TRUE)
  return(transform_table(data.frame(table_incomestat)))
} # get income statement 
myticker="3MINDIA"
test_inc=get_incomestat(myticker,url)

get_cashflow=function(myticker,url){
  url=pull_url(myticker)
  table_cashflowstat=list()
  table_cashflowstat[[myticker]]= url %>%   #pipeline
    read_html() %>%
    html_nodes(xpath='//*[@id="cash-flow"]/div/table') %>%
    html_table(.,fill=TRUE)
  return(transform_table(data.frame(table_cashflowstat)))
} # get cashdlow data # Op, inv and fin cash flows only 
myticker="ADANIPORTS"
test_cashflow=get_cashflow(myticker,url)
test_transform_cashflow=transform_table(test_cashflow)

get_balsheet=function(myticker,url){
  url=pull_url(myticker)
  table_balstat=list()
  table_balstat[[myticker]]= url %>%  #pipeline
    read_html() %>%
    html_nodes(xpath='//*[@id="balance-sheet"]/div/table') %>%
    html_table(.,fill=TRUE)
  return(transform_table(data.frame(table_balstat))) 
} # get balancesheet data 

plot_table=function(table)# Simple Plots - with user input - give variable name 
  {
  print(names(table))
  var= readline(prompt="Plot variables")
  var=as.character(var)
  dates <- year(as.Date(rownames(table), format="%Y", origin="1970-01-01"))
  if(names(table)[1]=="Sales"){table_name <- "Income Statement"} else if (names(table)[1]=="Share Capital") {table_name <- "Bal Sheet"}
                                                                        else {table_name <- "Cash Flow"}
  y_var=table[[var]]; print(table_name)
  ini_chart<- ggplot(table, aes(x=dates)) +geom_line(aes(y=y_var), size=1)+ geom_point(aes(y=y_var),size=2, color="red")+
  labs(title=myticker, subtitle = paste(table_name,"-",var), y= var)+scale_x_continuous(breaks= seq(min(dates),max(dates),1))+ theme(panel.grid.minor = element_blank())
  return(ini_chart)
  }  

