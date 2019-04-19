##download dta afrom morningstar 

require(RCurl)
require(jsonlite)
library(rvest)
library(httr)



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
  
  table_incomestat[[myticker]]=url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="profit-loss"]/div[1]/table')%>%
  html_table(.,fill=TRUE)
Sys.sleep(10)
}
t=data.frame(table_incomestat[[12]])
rownames(t)<- t[1:12,1]; t<-t[,-1];
t[1:12,1:13]= as.numeric(t[1:12,1:13])

tt<- transform(t, t[1:12,] = as.numeric(t[1:12,]))

as.numeric(t[1,1])

tt=data.frame(lapply(t, function(x) { gsub("%","",gsub(",","",x))}))


#Retrieve historical prices
json.histprice<-getURL(url.histprice(myticker))
json.histprice<-sub("NaN","\"NA\"",json.histprice)
histprice <- fromJSON(json.histprice)
df.prices<-as.data.frame(histprice$PriceDataList$Datapoints)
df.prices$Volume<-histprice$VolumeList$Datapoints
df.prices$Date<-as.Date(histprice$PriceDataList$DateIndexs[[1]],origin="1900-01-01")
colnames(df.prices) <- c("Open","High","Low","Close", "Volume", "Date")


df.fin <-read.csv(textConnection(kr.fin))
df.margins <-read.csv(textConnection(kr.margins))
df.profit <-read.csv(textConnection(kr.profit))
df.growth <-read.csv(textConnection(kr.growth))
df.cashflow <-read.csv(textConnection(kr.cashflow))
df.balance <-read.csv(textConnection(kr.balance))
df.liquid<-read.csv(textConnection(kr.liquid))
df.eff<-read.csv(textConnection(kr.eff))