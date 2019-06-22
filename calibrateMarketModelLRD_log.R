#=========================================================================
# load packages
#=========================================================================
library(data.table)
library(RMySQL)
library(xts)
library(hash)
library(dplyr)


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
trPaths<-coredata(dailyTrLog)
#
dateTime<-index(dailyTrLog)
#
pricePaths<-coredata(dailyPrices)
#
twrPaths<-coredata(dailyTwr)
#
twrTrPaths<-coredata(dailyTwrTrLog)

# convert the price paths to returns
logReturn<-apply(pricePaths,2,cumulative2LogReturn)

logTwrReturn<-apply(twrPaths,2,cumulative2LogReturn)


dimension<-dim(twrTrPaths)
nRows<-dimension[1]
nPaths<-dimension[2]

#
models_CARR<-hash()
# create results storage matrices
# mean true range estimate
tr_carr<-matrix(NA,nRows,nPaths)
# mean true range estimate
atr_carr<-matrix(NA,nRows,nPaths)
# gamma innovations
errorD<-matrix(NA,nRows,nPaths)
fittedD<-matrix(NA,nRows,nPaths)

errorD<-matrix(NA,nRows,nPaths)
fittedD<-matrix(NA,nRows,nPaths)
modelTrLog<-matrix(NA,nRows,nPaths)

# p-values
pValuesARFIMA<-matrix(NA,nPaths,4)
pValuesD<-matrix(NA,nPaths,2)
# all model coefficients
coefficientsByInstrumentARFIMA<-matrix(NA,nPaths,5)
colnames(coefficientsByInstrumentARFIMA)<-c('phi','theta','d','mean','var')
coefficientsByInstrumentD<-matrix(NA,nPaths,3)
colnames(coefficientsByInstrumentD)<-c('d','mean','var')

randomSeed<-1234567

annualDrift<-matrix(NA,1,nPaths)
annualSigma<-matrix(NA,1,nPaths)

for (pathIndex in 1:nPaths){
#for (pathIndex in 1:10){
  instrumentName<-instrumentNames[pathIndex]
  instrumentTicker<-instrumentTickers[pathIndex]
  nonNaIndexX<-!is.na(twrTrPaths[,pathIndex]) & twrTrPaths[,pathIndex]!=0
  nonNaIndexR<-!is.na(logTwrReturn[,pathIndex]) & logTwrReturn[,pathIndex]!=0

  x<-twrTrPaths[nonNaIndexX,pathIndex]

  r<-logTwrReturn[nonNaIndexR,pathIndex]

  # estimate the annual drift
  annualDrift[pathIndex]<-mean(r,na.rm = TRUE)*252
  # estimate the annual standard deviation
  annualSigma[pathIndex]<-sd(r,na.rm=TRUE)*sqrt(252)

  # find the ARFIMA parameters
  # try(fitARFIMA <- arfima(log(x), order = c(1, 0, 1),dmean=TRUE,useC=3,
  #   lmodel='d',seed=randomSeed),silent=TRUE)
  try(fitD <- arfima(log(x), order = c(0, 0, 0),dmean=TRUE,useC=3,
    lmodel='d',seed=randomSeed),silent=TRUE)

  # find the best fits
  # try(bestFitARFIMA <- bestModes(fitARFIMA, 1),silent=TRUE)
  try(bestFitD <- bestModes(fitD, 1),silent=TRUE)


  modelSummaryD<-summary(bestFitD)$coef[[1]]
  # modelSummaryARFIMA<-summary(bestFitARFIMA)$coef[[1]]
  # sigma2bestFitARFIMA<-summary(bestFitARFIMA)$sigma2
  # parameterListARFIMA=list(phi=modelSummaryARFIMA[1,1],dfrac=modelSummaryARFIMA[3,1],
  #   theta=modelSummaryARFIMA[2,1])
  # fittedMeanARFIMA<-modelSummaryARFIMA[4,1]
  parameterListD=list(dfrac=modelSummaryD[1,1])
  fittedMeanD<-modelSummaryD[2,1]
  sigma2bestFitD<-summary(bestFitD)$sigma2

  # coefficientsByInstrumentARFIMA[pathIndex,1]<-modelSummaryARFIMA[1,1]
  # coefficientsByInstrumentARFIMA[pathIndex,2]<-modelSummaryARFIMA[2,1]
  # coefficientsByInstrumentARFIMA[pathIndex,3]<-modelSummaryARFIMA[3,1]
  # coefficientsByInstrumentARFIMA[pathIndex,4]<-modelSummaryARFIMA[4,1]
  # coefficientsByInstrumentARFIMA[pathIndex,5]<-sigma2bestFitARFIMA

  # pValuesARFIMA<-modelSummaryARFIMA[pathIndex,1]<-modelSummaryARFIMA[1,5]
  # pValuesARFIMA<-modelSummaryARFIMA[pathIndex,2]<-modelSummaryARFIMA[2,5]
  # pValuesARFIMA<-modelSummaryARFIMA[pathIndex,3]<-modelSummaryARFIMA[3,5]
  # pValuesARFIMA<-modelSummaryARFIMA[pathIndex,4]<-modelSummaryARFIMA[4,5]

  coefficientsByInstrumentD[pathIndex,1]<-modelSummaryD[1,1]
  coefficientsByInstrumentD[pathIndex,2]<-modelSummaryD[2,1]
  coefficientsByInstrumentD[pathIndex,3]<-sigma2bestFitD

  pValuesD[pathIndex,1]<-modelSummaryD[1,5]
  pValuesD[pathIndex,2]<-modelSummaryD[2,5]


  eD<-residuals(bestFitD)
  fD<-fitted(bestFitD)

  # eARFIMA<-residuals(bestFitARFIMA)
  # fARFIMA<-fitted(bestFitARFIMA)

  errorD[nonNaIndexX,pathIndex]<-eD$Mode1
  fittedD[nonNaIndexX,pathIndex]<-fD$Mode1

  modelTrLog[nonNaIndexX,pathIndex]<-exp(((fD$Mode1*sqrt(sigma2bestFitD))+fittedMeanD)+eD$Mode1)


  # atr_carr[nonNaIndex,pathIndex]<-modelFitGamma$muHats
  # g[nonNaIndex,pathIndex]<-modelFitGamma$residuals
  # tr_carr[nonNaIndex,pathIndex]<-atr_carr[nonNaIndex,pathIndex]*g[nonNaIndex,pathIndex]
  #
  # # store the model object
  # models_CARR[instrumentTickers[pathIndex]]<-modelFitGamma

}

# # label the model parameters
# colnames(coefficientsByInstrument)<-list('omega','alpha1',
#                                          'beta1','kappa','gamma')
# # label the rows
# rownames(coefficientsByInstrument)<-instrumentTickers
#
# tableModelParametersCARR<-cbind(instrumentNames,coefficientsByInstrument)
#
# output<-list(tr_fitObjects=models_CARR,tr=tr_carr,atr=atr_carr,tr_residuals=g,
#              tr_parameters=tableModelParametersCARR)
# annualDrift=annualDrift,annualSigma=annualSigma

coefficientsByInstrumentLog<-cbind(t(instrumentNames),
  t(instrumentGroups),t(csiMultipliers),t(currencyCodes),
  coefficientsByInstrumentD,pValuesD,t(annualDrift),t(annualSigma))

outputDirectory<-fileDirectory
outputFileName='coefficientsByInstrumentLog.csv'
write.csv(data.frame(coefficientsByInstrumentLog),
  file = paste0(outputDirectory,outputFileName))

fileNameLrdLog<-'coefficientsByInstrumentLog.rds'
fileLrdLog<-paste0(fileDirectory,'/',fileNameLrdLog)
# write
saveRDS(coefficientsByInstrumentLog, fileLrdLog)


fileNameLrdLogE<-'logE.rds'
fileLrdLogE<-paste0(fileDirectory,'/',fileNameLrdLogE)
saveRDS(errorD, fileLrdLogE)

fileNameLrdLogF<-'logF.rds'
fileLrdLogF<-paste0(fileDirectory,'/',fileNameLrdLogF)
saveRDS(fittedD, fileLrdLogF)

fileNameLrdLogModel<-'logModel.rds'
fileLrdLogModel<-paste0(fileDirectory,'/',fileNameLrdLogModel)
saveRDS(modelTrLog, fileLrdLogModel)





