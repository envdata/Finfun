##download dta afrom morningstar 

require(RCurl)
require(jsonlite)
library(Quandl)

myticker<-"NSE\3MINDIA"
##url.histprice<-function(x){ return(paste0("http://globalquote.morningstar.com/globalcomponent/RealtimeHistoricalStockData.ashx?ticker=",x,"&showVol=true&dtype=his&f=d&curry=USD&range=1900-1-1|2014-10-10&isD=true&isS=true&hasF=true&ProdCode=DIRECT"))}
url.keyratios<-function(x){return(paste0("http://financials.morningstar.com/ajax/exportKR2CSV.html?t=",x))}

#Prices from Quandl
NSE_CODES=read.csv("C:/Users/ashivkumar/Documents/R/NSE Codes.csv",header=FALSE)
NSE_CODES=read.csv("C:/Users/adity/Documents/R/NSE Codes.csv",header=FALSE)

colnames(NSE_CODES)=c("Codes","Company")
testprice=Quandl("NSE/3MINDIA", api_key="uYHynQoeChxswuz2Sz6g")

##Import prices from Quandl
ls_codes=list()
##testing with 10 
for(pr in NSE_CODES[1:10,1]){
    ls_codes[[which(NSE_CODES==pr)]]=Quandl(pr, api_key="uYHynQoeChxswuz2Sz6g")
  } 

index(NSE_CODES[1,1])

#Retrieve historical prices
json.histprice<-getURL(url.histprice(myticker))
json.histprice<-sub("NaN","\"NA\"",json.histprice)
histprice <- fromJSON(json.histprice)
df.prices<-as.data.frame(histprice$PriceDataList$Datapoints)
df.prices$Volume<-histprice$VolumeList$Datapoints
df.prices$Date<-as.Date(histprice$PriceDataList$DateIndexs[[1]],origin="1900-01-01")
colnames(df.prices) <- c("Open","High","Low","Close", "Volume", "Date")



str.keyratios<-getURL(url.keyratios(myticker))
kr.fin <- sub(".*Financials\n(.*)Key Ratios -> Profitability.*","\\1",str.keyratios)
kr.margins <- sub(".*Key Ratios -> Profitability\n(.*)Profitability.*","\\1",str.keyratios)
kr.profit <- sub(".*Key Ratios -> Profitability.*(Profitability.*)Key Ratios -> Growth.*","\\1",str.keyratios)
kr.growth<-sub(".*Key Ratios -> Growth\n(.*)Key Ratios -> Cash Flow.*","\\1",str.keyratios)
kr.cashflow<-sub(".*Key Ratios -> Cash Flow\n(.*)Key Ratios -> Financial Health.*","\\1",str.keyratios)
kr.balance<-sub(".*Key Ratios -> Financial Health\n(Balance Sheet Items.*)Liquidity/Financial Health.*","\\1",str.keyratios)
kr.liquid<-sub(".*Key Ratios -> Financial Health.*(Liquidity/Financial Health.*)Key Ratios -> Efficiency Ratios.*","\\1",str.keyratios)
kr.eff<-sub(".*Key Ratios -> Efficiency Ratios\n(.*)","\\1",str.keyratios)
df.fin <-read.csv(textConnection(kr.fin))
df.margins <-read.csv(textConnection(kr.margins))
df.profit <-read.csv(textConnection(kr.profit))
df.growth <-read.csv(textConnection(kr.growth))
df.cashflow <-read.csv(textConnection(kr.cashflow))
df.balance <-read.csv(textConnection(kr.balance))
df.liquid<-read.csv(textConnection(kr.liquid))
df.eff<-read.csv(textConnection(kr.eff))