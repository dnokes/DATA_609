#=========================================================================
# load packages
#=========================================================================
library(data.table)
library(RMySQL)
library(xts)
library(hash)
library(dplyr)

#=========================================================================
# fetch data from the database
#=========================================================================


#-------------------------------------------------------------------------
# fetch USD futures prices
#-------------------------------------------------------------------------
fetchFuturesPricesUSD<- function (dbHandle,ibTicker){
  # build the query

  query<-paste0("SELECT dateTime,dayOfWeek,deliveryYYYYMM,",
    "openPrice,highPrice,lowPrice,closePrice,volume,openInterest,",
    "instrumentVolume,instrumentOpenInterest,trueRange,trueRangeLog,",
    "closePriceChg,rollFlag,rollDate,expiryDate,nextDeliveryYYYYMM,",
    "nextRollDate,nextExpiryDate,adjustment,adjOpenPrice,adjHighPrice,",
    "adjLowPrice,adjClosePrice,carry FROM global_monitoring.",
    "csi_usd_futures_back_adjusted_price_log WHERE ibTicker='",
    ibTicker,"' ORDER BY dateTime,deliveryYYYYMM;")

  # query the database
  instrumentPrices<-dbGetQuery (dbHandle, query)
  return (instrumentPrices)
}

#-------------------------------------------------------------------------
# fetch Non-USD futures prices
#-------------------------------------------------------------------------
fetchFuturesPricesNonUSD<- function (dbHandle,ibTicker){
  # build the query

  query<-paste0("SELECT dateTime,dayOfWeek,deliveryYYYYMM,",
    "openPrice,highPrice,lowPrice,closePrice,volume,openInterest,",
    "instrumentVolume,instrumentOpenInterest,trueRange,trueRangeLog,",
    "closePriceChg,rollFlag,rollDate,expiryDate,nextDeliveryYYYYMM,",
    "nextRollDate,nextExpiryDate,adjustment,adjOpenPrice,adjHighPrice,",
    "adjLowPrice,adjClosePrice,carry FROM global_monitoring.",
    "csi_non_usd_futures_back_adjusted_price_log WHERE ibTicker='",
    ibTicker,"' ORDER BY dateTime,deliveryYYYYMM;")

  # query the database
  instrumentPrices<-dbGetQuery (dbHandle, query)
  return (instrumentPrices)
}

#-------------------------------------------------------------------------
# fetch futures prices
#-------------------------------------------------------------------------
fetchFuturesPrices<- function (dbHandle,instrument){
  # extract instrument ticker
  ibTicker<-instrument$ibTicker
  # extract IB currency code
  ibCurrencyCode<-instrument$ibCurrencyCode
  # if USD
  if (ibCurrencyCode == 'USD'){
    instrumentPrices<-fetchFuturesPricesUSD(dbHandle,ibTicker)
  }
  else{
    instrumentPrices<-fetchFuturesPricesNonUSD(dbHandle,ibTicker)
  }
  return (instrumentPrices)
}

#-------------------------------------------------------------------------
# build the instrument object
#-------------------------------------------------------------------------
buildInstrumentObject<- function (dbHandle,instrumentDetails){
  # fetch the instrument prices
  instrumentPrices<-fetchFuturesPrices(dbHandle,instrumentDetails)
  # find the non roll index
  nonRollIndex<-instrumentPrices$rollFlag!=-1
  # extract prices for only the non-roll index
  instrumentPrices<-instrumentPrices[nonRollIndex,]
  # extract the report name
  instrumentName<-instrumentDetails$reportName
  # extract the IB ticker
  instrumentTicker<-instrumentDetails$ibTicker
  # extract the date
  dateTime<-as.POSIXct(instrumentPrices$dateTime)

  # create the close price data frame
  closePrice<-data.frame(row.names=dateTime,
    instrumentPrices$adjClosePrice)
  trueRange<-data.frame(row.names=dateTime,
    instrumentPrices$trueRange)
  trueRangeLog<-data.frame(row.names=dateTime,
    instrumentPrices$trueRangeLog)

  # re-label the instrument rows
  colnames(closePrice)<-instrumentTicker
  colnames(trueRange)<-instrumentTicker
  colnames(trueRangeLog)<-instrumentTicker

  # create the instrument objects (close price, total return
  # index, and true range)
  instrumentObject<-xts(closePrice,order.by=dateTime,
    desc=instrumentTicker)
  instrumentObjectTR<-xts(trueRange,order.by=dateTime,
    desc=instrumentTicker)
  instrumentObjectTRLog<-xts(trueRangeLog,order.by=dateTime,
    desc=instrumentTicker)

  # re-label the instrument rows
  colnames(instrumentObject)<-instrumentTicker
  colnames(instrumentObjectTR)<-instrumentTicker
  colnames(instrumentObjectTRLog)<-instrumentTicker

  # # add the instrument details as attributes
  # attributes(instrumentObject) <- list(
  #   reportName=instrumentDetails$reportName,
  #   reportGroup=instrumentDetails$reportGroup,
  #   ibTicker=instrumentDetails$ibTicker,
  #   ibExchange=instrumentDetails$ibExchange,
  #   ibMultiplier=instrumentDetails$ibMultiplier,
  #   ibCurrencyCode=instrumentDetails$ibCurrencyCode,
  #   csiNumber=instrumentDetails$csiNumber,
  #   csiMultiplier=instrumentDetails$csiMultiplier,
  #   term=instrumentDetails$term,
  #   rollType=instrumentDetails$rollType,
  #   nthMonthPrior=instrumentDetails$nthMonthPrior,
  #   mthDay=instrumentDetails$mthDay,
  #   allowableMonthsString=instrumentDetails$allowableMonthsString)

  return (list(price=instrumentObject,tr=instrumentObjectTR,
    trLog=instrumentObjectTRLog))
}

#-------------------------------------------------------------------------
# fetch USD futures prices
#-------------------------------------------------------------------------
fetchVolNormalizedFuturesPricesUSD<- function (dbHandle,tableUSD,
  ibTicker){
  # build the query

  query<-paste0("SELECT dateTime,dayOfWeek,deliveryYYYYMM,",
    "openPrice,highPrice,lowPrice,closePrice,volume,openInterest,",
    "instrumentVolume,instrumentOpenInterest,trueRange,trueRangeLog,",
    "closePriceChg,rollFlag,rollDate,expiryDate,nextDeliveryYYYYMM,",
    "nextRollDate,nextExpiryDate,adjustment,adjOpenPrice,adjHighPrice,",
    "adjLowPrice,adjClosePrice,carry,normalizedTradeSize,position,openTwr,",
    "highTwr,lowTwr,closeTwr,trueRangeTwr,trueRangeTwrLog FROM ",tableUSD,
    " WHERE ibTicker='",ibTicker,"' ORDER BY dateTime,deliveryYYYYMM;")

  # query the database
  instrumentPrices<-dbGetQuery (dbHandle, query)
  return (instrumentPrices)
}

#-------------------------------------------------------------------------
# fetch Non-USD futures prices
#-------------------------------------------------------------------------
fetchVolNormalizedFuturesPricesNonUSD<- function (dbHandle,tableNonUSD,
  ibTicker){
  # build the query

  query<-paste0("SELECT dateTime,dayOfWeek,deliveryYYYYMM,",
    "openPrice,highPrice,lowPrice,closePrice,volume,openInterest,",
    "instrumentVolume,instrumentOpenInterest,trueRange,trueRangeLog,",
    "closePriceChg,rollFlag,rollDate,expiryDate,nextDeliveryYYYYMM,",
    "nextRollDate,nextExpiryDate,adjustment,adjOpenPrice,adjHighPrice,",
    "adjLowPrice,adjClosePrice,carry,normalizedTradeSize,position,openTwr,",
    "highTwr,lowTwr,closeTwr,trueRangeTwr,trueRangeTwrLog FROM ",tableNonUSD,
    " WHERE ibTicker='",ibTicker,"' ORDER BY dateTime,deliveryYYYYMM;")

  # query the database
  instrumentPrices<-dbGetQuery (dbHandle, query)
  return (instrumentPrices)
}

#-------------------------------------------------------------------------
# fetch futures prices
#-------------------------------------------------------------------------
fetchVolNormalizedFuturesPrices<- function (dbHandle,tableUSD,tableNonUSD,
  instrument){
  # extract instrument ticker
  ibTicker<-instrument$ibTicker
  # extract IB currency code
  ibCurrencyCode<-instrument$ibCurrencyCode
  # if USD
  if (ibCurrencyCode == 'USD'){
    instrumentPrices<-fetchVolNormalizedFuturesPricesUSD(dbHandle,
      tableUSD,ibTicker)
  }
  else{
    instrumentPrices<-fetchVolNormalizedFuturesPricesNonUSD(dbHandle,
      tableNonUSD,ibTicker)
  }
  return (instrumentPrices)
}

#-------------------------------------------------------------------------
# build the instrument object
#-------------------------------------------------------------------------
buildVolNormalizedInstrumentObject<- function (dbHandle,tableUSD,
  tableNonUSD,instrumentDetails){
  # fetch the instrument prices
  instrumentPrices<-fetchVolNormalizedFuturesPrices(dbHandle,
    tableUSD,tableNonUSD,instrumentDetails)
  # filter
  instrumentPrices<-filter(instrumentPrices,rollFlag!=-1)
  # extract the report name
  instrumentName<-instrumentDetails$reportName
  # extract the IB ticker
  instrumentTicker<-instrumentDetails$ibTicker
  # extract the date
  dateTime<-as.POSIXct(instrumentPrices$dateTime)

  # create the close price data frame
  closePrice<-data.frame(row.names=dateTime,
    instrumentPrices$adjClosePrice)
  closeTwr<-data.frame(row.names=dateTime,
    instrumentPrices$closeTwr)
  trueRangeTwr<-data.frame(row.names=dateTime,
    instrumentPrices$trueRangeTwr)
  trueRangeTwrLog<-data.frame(row.names=dateTime,
    instrumentPrices$trueRangeTwrLog)

  # re-label the instrument rows
  colnames(closePrice)<-instrumentTicker
  colnames(closeTwr)<-instrumentTicker
  colnames(trueRangeTwr)<-instrumentTicker
  colnames(trueRangeTwrLog)<-instrumentTicker

  # create the instrument objects (close price, total return
  # index, and true range)
  instrumentObject<-xts(closePrice,order.by=dateTime,
    desc=instrumentTicker)
  instrumentObjectTwr<-xts(closeTwr,order.by=dateTime,
    desc=instrumentTicker)
  instrumentObjectTR<-xts(trueRangeTwr,order.by=dateTime,
    desc=instrumentTicker)
  instrumentObjectTRLog<-xts(trueRangeTwrLog,order.by=dateTime,
    desc=instrumentTicker)


  # re-label the instrument rows
  colnames(instrumentObject)<-instrumentTicker
  colnames(instrumentObjectTwr)<-instrumentTicker
  colnames(instrumentObjectTR)<-instrumentTicker
  colnames(instrumentObjectTRLog)<-instrumentTicker

  return (list(price=instrumentObject,
    twr=instrumentObjectTwr,tr=instrumentObjectTR,
    trLog=instrumentObjectTRLog))
}

#-------------------------------------------------------------------------
# fetch the USD report groups
#-------------------------------------------------------------------------
fetchReportGroupsUSD<- function (dbHandle){
  # build the query
  query<-paste0("SELECT reportName,reportGroup,ibTicker,",
    "ibExchange,ibMultiplier,ibCurrencyCode,csiNumber,",
    "csiMultiplier,term,rollType,nthMonthPrior,mthDay,",
    "allowableMonthsString FROM ",
    "global_monitoring.csi_usd_futures_back_adjusted_price_log",
    " GROUP BY ibTicker ORDER BY reportGroup,reportName;")
  # query the database
  futuresDetails<-dbGetQuery (dbHandle, query)
  # iterate over the tickers and extract the data
  return (futuresDetails)
}

#-------------------------------------------------------------------------
# fetch the non-USD report groups
#-------------------------------------------------------------------------
fetchReportGroupsNonUSD<- function (dbHandle){
  # build the query
  query<-paste0("SELECT reportName,reportGroup,ibTicker,",
    "ibExchange,ibMultiplier,ibCurrencyCode,csiNumber,",
    "csiMultiplier,term,rollType,nthMonthPrior,mthDay,",
    "allowableMonthsString FROM ",
    "global_monitoring.csi_non_usd_futures_back_adjusted_price_log",
    " GROUP BY ibTicker ORDER BY reportGroup,reportName;")
  # query the database
  futuresDetails<-dbGetQuery (dbHandle, query)
  # iterate over the tickers and extract the data
  return (futuresDetails)
}

#-------------------------------------------------------------------------
# fetch the report groups
#-------------------------------------------------------------------------
fetchReportGroups<- function (dbHandle){
  # extract the back-adjusted futures (USD)
  contractDetailsUSD<-fetchReportGroupsUSD(dbHandle)
  # extract the back-adjusted futures (non-USD)
  contractDetailsNonUSD<-fetchReportGroupsNonUSD(dbHandle)
  # merge the USD and non-USD contract details
  contractDetails<-rbind(contractDetailsUSD,contractDetailsNonUSD)
  return (contractDetails)
}

#-------------------------------------------------------------------------
# connect to the database and fetch global futures
#-------------------------------------------------------------------------

# set connection parameters
dbUserName<-'root'
dbPassword<-'TGDNrx78'
dbHost<-'localhost'
dbPort<-3306
dbName<-'global_monitoring'

# connect to the database
dbHandle <-  dbConnect(RMySQL::MySQL(), username = dbUserName,
  password = dbPassword,host = dbHost, port = dbPort,
  dbname = dbName)


# define tables
#startYYYY<-1979
startYYYY<-1989
#startYYYY<-1999

tableNonUSD<-paste0("csi_non_usd_futures_back_adjusted_twr_log_",
  startYYYY)
tableUSD<-paste0("csi_usd_futures_back_adjusted_twr_log_",
  startYYYY)

# extract the contract details
contractDetails<-fetchReportGroups(dbHandle)

# determine number of instruments
nInstruments<-nrow(contractDetails)
#
instrumentTickers<-matrix(NA,1,nInstruments)
#
instrumentNames<-matrix(NA,1,nInstruments)
#
instrumentGroups<-matrix(NA,1,nInstruments)
#
currencyCodes<-matrix(NA,1,nInstruments)
#
csiMultipliers<-matrix(NA,1,nInstruments)

# report groups
reportGroups<-unique(select(contractDetails,reportGroup))
#
nReportGroups<-nrow(reportGroups)

# create the global futures objects
globalFuturesPrice<-xts()
globalFuturesTr<-xts()
globalFuturesTrLog<-xts()

globalFuturesTwr<-xts()
globalFuturesTwrTr<-xts()
globalFuturesTwrTrLog<-xts()

#
globalInstrumentIndex<-0
#
for (groupIndex in 1:nReportGroups){
  groupName<-reportGroups[groupIndex,]
  #groupDetails<-filter(contractDetails,reportGroup==groupName)
  groupInstrumentIndex<-contractDetails$reportGroup==groupName
  groupDetails<-contractDetails[groupInstrumentIndex,]
  nGroupDetails<-nrow(groupDetails)

  groupPrice<-xts()
  groupTr<-xts()
  groupTrLog<-xts()

  groupTwr<-xts()
  groupTwrTr<-xts()
  groupTwrTrLog<-xts()

  # iterate over each instrument ticker

  for (instrumentIndex in 1:nGroupDetails){
    # index
    globalInstrumentIndex<-globalInstrumentIndex+1
    # fetch the instrument details
    instrumentDetails<-groupDetails[instrumentIndex,]
    # fetch the instrument prices (vol normalized)
    try(instrumentObjectVolN<-buildVolNormalizedInstrumentObject(dbHandle,
      tableUSD,tableNonUSD,instrumentDetails),silent=FALSE)
    # fetch the instrument prices
    try(instrumentObject<-buildInstrumentObject(dbHandle,
      instrumentDetails),silent=FALSE)

    # extract the CSI multiplier
    instrumentTickers[globalInstrumentIndex]<-instrumentDetails$ibTicker

    # extract the CSI multiplier
    csiMultipliers[globalInstrumentIndex]<-instrumentDetails$csiMultiplier

    # extract the instrument name
    instrumentNames[globalInstrumentIndex]<-instrumentDetails$reportName

    # extract the group name
    instrumentGroups[globalInstrumentIndex]<-instrumentDetails$reportGroup

    # extract the group name
    currencyCodes[globalInstrumentIndex]<-instrumentDetails$ibCurrencyCode

#
#     # # extract the CSI multiplier
#     # instrumentTickers[instrumentIndex]<-instrumentDetailList$ibTicker
#     #
#     # # extract the CSI multiplier
#     # csiMultipliers[instrumentIndex]<-instrumentDetailList$csiMultiplier
#     #
#     # # extract the instrument name
#     # instrumentNames[instrumentIndex]<-instrumentDetailList$reportName
#     #
#     # # extract the group name
#     # instrumentGroups[instrumentIndex]<-instrumentDetailList$reportGroup
#     #
#     # # extract the group name
#     # currencyCodes[instrumentIndex]<-instrumentDetailList$ibCurrencyCode
#
#     # # extract the CSI multiplier
#     # try(instrumentTickers<-cbind(instrumentTickers,instrumentDetailList$ibTicker),silent=FALSE)
#     # # extract the instrument name
#     # try(instrumentNames<-cbind(instrumentNames,instrumentDetailList$reportName),silent=FALSE)
#     # # extract the group name
#     # try(instrumentGroups<-cbind(instrumentGroups,instrumentDetailList$reportGroup),silent=FALSE)
#     # # extract the group name
#     # try(currencyCodes<-cbind(currencyCodes,instrumentDetailList$ibCurrencyCode),silent=FALSE)
#     # # extract the CSI multiplier
#     # try(csiMultipliers<-cbind(csiMultipliers,instrumentDetailList$csiMultiplier),silent=FALSE)
#
#     # buildInstrumentObject
#
#     #print(tail(instrumentObject$price))
#     #print(tail(instrumentObject$twr))
#     #print(tail(instrumentObject$tr))
#
    # add the instrument object to the group object
    try(groupPrice<-merge(groupPrice,instrumentObject$price),
      silent=FALSE)
    #
    try(globalFuturesPrice<-merge(globalFuturesPrice,
      instrumentObject$price),silent=FALSE)
    #
    try(groupTr<-merge(groupTr,instrumentObject$tr),silent=FALSE)
    #
    try(globalFuturesTr<-merge(globalFuturesTr,instrumentObject$tr),
        silent=FALSE)
    #
    try(groupTrLog<-merge(groupTrLog,instrumentObject$trLog),silent=FALSE)
    #
    try(globalFuturesTrLog<-merge(globalFuturesTrLog,instrumentObject$trLog),
        silent=FALSE)

    #
    try(groupTwr<-merge(groupTwr,instrumentObjectVolN$twr),
        silent=FALSE)

    #
    try(globalFuturesTwr<-merge(globalFuturesTwr,instrumentObjectVolN$twr),
        silent=FALSE)

    try(globalFuturesTwrTr<-merge(globalFuturesTwrTr,instrumentObjectVolN$tr),
        silent=FALSE)

    try(globalFuturesTwrTrLog<-merge(globalFuturesTwrTrLog,
      instrumentObjectVolN$trLog),silent=FALSE)

    instrumentObject<-NULL
    instrumentObjectVolN<-NULL
  }
}

# fill na
dailyPrices<-na.locf(globalFuturesPrice, fromLast=FALSE,maxgap=5)
dailyTr<-na.locf(globalFuturesTr, fromLast=FALSE,maxgap=5)
dailyTrLog<-na.locf(globalFuturesTrLog, fromLast=FALSE,maxgap=5)

dailyTwr<-na.locf(globalFuturesTwr, fromLast=FALSE,maxgap=5)
dailyTwrTr<-na.locf(globalFuturesTwrTr, fromLast=FALSE,maxgap=5)
dailyTwrTrLog<-na.locf(globalFuturesTwrTrLog, fromLast=FALSE,maxgap=5)


pricePaths<-coredata(dailyPrices)
trPaths<-coredata(dailyTrLog)

twrPaths<-coredata(dailyTwr)
twrTrPaths<-coredata(dailyTwrTrLog)

dateTime<-index(dailyTrLog)

instrumentDetailsList<-list(instrumentTickers=instrumentTickers,
  instrumentNames=instrumentNames,instrumentGroups=instrumentGroups,
  currencyCodes=currencyCodes,csiMultipliers=csiMultipliers)

fileDirectory<-'C:/Users/Derek/Documents/GitHub/IS609/final_project/'

fileNamePrice<-paste0("dailyPrices_",startYYYY,".rds")
fileNameTr<-paste0("dailyTr_",startYYYY,".rds")
fileNameTrLog<-paste0("dailyTrLog_",startYYYY,".rds")
fileNameContractDetails<-paste0("contractDetails_",startYYYY,".rds")
fileNameInstrumentDetailsList<-paste0("instrumentDetailsList_",
  startYYYY,".rds")

fileNameTwr<-paste0("dailyTwr_",startYYYY,".rds")
fileNameTwrTr<-paste0("dailyTwrTr_",startYYYY,".rds")
fileNameTwrTrLog<-paste0("dailyTwrTrLog_",startYYYY,".rds")

filePrice<-paste0(fileDirectory,'/',fileNamePrice)
fileTr<-paste0(fileDirectory,'/',fileNameTr)
fileTrLog<-paste0(fileDirectory,'/',fileNameTrLog)
fileContractDetails<-paste0(fileDirectory,'/',fileNameContractDetails)
fileInstrumentDetailList<-paste0(fileDirectory,'/',
  fileNameInstrumentDetailsList)

fileTwr<-paste0(fileDirectory,'/',fileNameTwr)
fileTwrTr<-paste0(fileDirectory,'/',fileNameTwrTr)
fileTwrTrLog<-paste0(fileDirectory,'/',fileNameTwrTrLog)

#=========================================================================
# write the data to disk
#=========================================================================

# write contract details
saveRDS(contractDetails, fileContractDetails)
# write daily global futures prices
saveRDS(dailyPrices, filePrice)
# write daily global futures true range
saveRDS(dailyTr, fileTr)
# write daily global futures log true range
saveRDS(dailyTrLog, fileTrLog)
# write instrument details list
saveRDS(instrumentDetailsList, fileInstrumentDetailList)

# write daily global futures TWR
saveRDS(dailyTwr, fileTwr)
# write daily global futures TWR true range
saveRDS(dailyTwrTr, fileTwrTr)
# write daily global futures TWR  log true range
saveRDS(dailyTwrTrLog, fileTwrTrLog)

