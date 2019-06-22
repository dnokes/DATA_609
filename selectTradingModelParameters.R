library(Rcpp)
library(GA)
library(xts)

#-------------------------------------------------------------------------
# compute cumulative return (from log changes)
#-------------------------------------------------------------------------
cumulative2LogReturn <- function (x){
  return(diff(log(x),lag=1))
}

# change this to a list that returns the matrix of inital prices
# and the returns so that it can be converted back to prices

#-------------------------------------------------------------------------
# compute cumulative return (from log changes)
#-------------------------------------------------------------------------
cumulativeLogReturn <- function (x){
  return(exp(cumsum(x)))
}

#=========================================================================
# read data from disk
#=========================================================================
startYYYY<-1999
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
# read the data from disk
#=========================================================================

# read daily global futures prices
dailyPrices <- readRDS(filePrice)
# read daily global futures true range
dailyTr <- readRDS(fileTr)
# read daily global futures log true range
dailyTrLog <- readRDS(fileTrLog)

# read daily global futures prices
dailyTwr <- readRDS(fileTwr)
# read daily global futures true range
dailyTwrTr <- readRDS(fileTwrTr)
# read daily global futures log true range
dailyTwrTrLog <- readRDS(fileTwrTrLog)

# read contract details
contractDetails<-readRDS(fileContractDetails)
# read instrument details
instrumentDetailsList<-readRDS(fileInstrumentDetailList)
# extract instrument details
instrumentTickers<-instrumentDetailsList$instrumentTickers
instrumentNames<-instrumentDetailsList$instrumentNames
instrumentGroups<-instrumentDetailsList$instrumentGroups
currencyCodes<-instrumentDetailsList$currencyCodes
csiMultipliers<-instrumentDetailsList$csiMultipliers


#
#trPaths<-coredata(dailyTrLog)
#
dateTime<-index(dailyTrLog)
#
#pricePaths<-coredata(dailyPrices)
#
twrPaths<-coredata(dailyTwr)
#
twrTrPaths<-coredata(dailyTwrTr)

#
sourceCpp("C:/Users/Derek/Documents/GitHub/IS609/final_project/crossoverWithStopSingleInstrumentObjectiveC.cpp")

#
# define parameters
randomSeed<-1234567

# trading model parameters

atrMultiplier <- 4
fastLookback <- 120
slowLookback <- 180


# market model parameters
# nRowsScenario<-1250
# nPathsScenario<-1000
# S0<-1
# T<-5
# mu<-
# xSigma2<-
# xMean<-
#
# #
# result<-singleMarketModel_LRD(S0,mu,T,nRowsScenario,
#   nPathsScenario,parameterListD,xSigma2,xMean)

#
#matplot(result$pricePaths,type='l')

#
#matplot(result$tr,type='l')


strategyObjectiveFunction<-function(fastLookback,slowLookback){

  pricePaths<-twrPaths
  tr<-twrTrPaths
  atrMultiplier<-4
  atrLookback <- 20
  longOnly <- FALSE
  commissionPerShare <- 0
  accountSize <- 100000
  fPercent <- 0.005
  minRisk <-0.005
  stopTWR <-0.85

  # # create the strategy input object
  # strategyInput<-list(pricePaths=result$pricePaths,
  #                     trueRangePaths=result$tr,
  #                     atrLookback=atrLookback,
  #                     atrMultiplier=atrMultiplier,
  #                     fastLookback=fastLookback,
  #                     slowLookback=slowLookback,
  #                     longOnly=longOnly,
  #                     commissionPerShare=commissionPerShare,
  #                     accountSize=accountSize,
  #                     fPercent=fPercent,
  #                     minRisk=minRisk,
  #                     stopTWR=stopTWR)

  # create the strategy input object
  strategyInput<-list(pricePaths=pricePaths,
                      trueRangePaths=tr,
                      atrLookback=atrLookback,
                      atrMultiplier=atrMultiplier,
                      fastLookback=fastLookback,
                      slowLookback=slowLookback,
                      longOnly=longOnly,
                      commissionPerShare=commissionPerShare,
                      accountSize=accountSize,
                      fPercent=fPercent,
                      minRisk=minRisk,
                      stopTWR=stopTWR)

  # run the strategy for a single instrument (N paths)
  strategyOutput<-crossoverWithStopSingleInstrumentC(strategyInput)

  meanTWR<-mean(strategyOutput$TWR,na.rm = TRUE)

  AHPR<-meanTWR^(1/T)
}

#minAtrMultiplier<-1
#maxAtrMultiplier<-10
minFastLookback<-80
maxSFastLookback<-180
minSlowLookback<-120
maxSlowLookback<-250

# GA <- ga(type = "real-valued", fitness = function(x) -strategyObjectiveFunction(x[1],
#   x[2],x[3]), min = c(minAtrMultiplier,minFastLookback,minSlowLookback),
#   max = c(maxAtrMultiplier, maxSFastLookback,maxSlowLookback), popSize = 50,
#   maxiter = 100,seed=randomSeed)

# GA <- ga(type = "real-valued", fitness = function(x) -strategyObjectiveFunction(x[1],
#   x[2]), min = c(minFastLookback,minSlowLookback),
#   max = c(maxSFastLookback,maxSlowLookback), popSize = 50,
#   maxiter = 100,seed=randomSeed,replace=FALSE)

# brute force
fastLookbackRange<-seq(minFastLookback,maxSFastLookback)
slowLookbackRange<-seq(minSlowLookback,maxSlowLookback)
nFastLookback<-length(fastLookbackRange)
nSlowLookback<-length(slowLookbackRange)

performance<-matrix(NA,nFastLookback,nSlowLookback)

for (slowLookbackIndex in  1:nSlowLookback){
  slowLookback<-slowLookbackRange[slowLookbackIndex]

  for (fastLookbackIndex in 1:nFastLookback){
    fastLookback<-fastLookbackRange[fastLookbackIndex]
    if (fastLookback<slowLookback){
      performance[fastLookbackIndex,slowLookbackIndex]<-strategyObjectiveFunction(fastLookback,slowLookback)
    }
  }
}

image(performance)
