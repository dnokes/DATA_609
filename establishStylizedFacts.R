library(ggplot2)
library(ggthemes)
library(gridExtra)
library(ggExtra)
library(ACDm)
library(hash)
library(xts)
library(Rcpp)
library(MASS)
library(hexbin)
library(reshape2)
library(ztable)


# define ploting functions

#-------------------------------------------------------------------------
# Multiple plot function (Source: Cookbook for R)
#-------------------------------------------------------------------------
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot
# objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout),
                                               ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this
      # subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



#-------------------------------------------------------------------------
# plot median with confidence bounds
#-------------------------------------------------------------------------
plotSecenarioPerformanceWithCI<- function (marketModelParameterScenarios,table_figure,
                                           titleName,xLabel,yLabel){

  figure_df <- data.frame(marketModelParameterScenarios=marketModelParameterScenarios,
                          Lower=table_figure[,1],Median=table_figure[,2],Upper=table_figure[,3])

  limits <- aes(ymax = Upper, ymin=Lower)

  p1 <- ggplot(figure_df, aes(marketModelParameterScenarios)) +
    geom_line(aes(y=Median), colour="blue") +
    scale_color_grey() +
    geom_ribbon(limits, alpha=0.2) +
    ggtitle(titleName) + xlab(xLabel) + ylab(yLabel)

  return (p1)

}

#-------------------------------------------------------------------------
# plot median with confidence bounds
#-------------------------------------------------------------------------
plotSecenarioPerformanceWithCI_2<- function (marketModelParameterScenarios,table_figure,
                                             titleName,xLabel,yLabel){

  figure_df <- data.frame(marketModelParameterScenarios=marketModelParameterScenarios,
                          Lower=table_figure[,1],Median=table_figure[,2],Upper=table_figure[,3])

  limits <- aes(ymax = Upper, ymin=Lower)

  p1 <- ggplot(figure_df, aes(marketModelParameterScenarios)) +
    geom_line(aes(y=Median), colour="blue") +
    scale_color_grey() +
    geom_ribbon(limits, alpha=0.2) +
    ggtitle(titleName) + xlab(xLabel) + ylab(yLabel) +
    theme(axis.text=element_text(size=6),
          axis.title=element_text(size=8,face="bold"))

  return (p1)

}


#-------------------------------------------------------------------------
# plot histogram with normal density
#-------------------------------------------------------------------------
normalHistFit<-function (e,xLabel,titleName,breaks){
  # estimate distribution parameters
  result<-fitdistr(e,'normal')
  mu<-result$estimate[1]
  sigma<-result$estimate[2]
  hist(e,freq=FALSE,breaks=breaks,
       xlab=xLabel,main=titleName)
  curve(dnorm(x, mean=mu, sd=sigma),
        add=TRUE, col='darkblue', lwd=2)

  return (result)
}

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

#-------------------------------------------------------------------------
# compute ACF for each path, return ACF by path, percentile bounds, and
# mean
#-------------------------------------------------------------------------
acfPaths <- function(e,maxLag,alpha){

  dimension <- dim(e)
  nRows<-dimension[1]
  nPaths <- dimension[2]

  eAcfC<-matrix(0,nrow=maxLag,ncol=nPaths)
  eAcfLag<-matrix(0,nrow=maxLag,ncol=nPaths)

  for (pathIndex in 1:nPaths){
    # filter out NAs
    nonNaIndex<-!is.na(e[,pathIndex]) & e[,pathIndex]!=0
    eNoNA<-e[nonNaIndex,pathIndex]
    # autocorrelation
    eAcfData<-acf(eNoNA,lag.max=maxLag,plot=FALSE)
    eAcfC[,pathIndex]<-eAcfData$acf[2:(maxLag+1)]
    eAcfLag[,pathIndex]<-eAcfData$lag[2:(maxLag+1)]
  }

  lowerPercentile<-alpha/2
  upperPercentile<-1-alpha/2
  ePercentileAcf<-apply(eAcfC,1, quantile, probs=c(lowerPercentile,
    0.5,upperPercentile), na.rm=TRUE)
  eMeanAcf<-apply(eAcfC,1, mean, na.rm=TRUE)

  output<-list(percentileAcf=ePercentileAcf,meanAcf=eMeanAcf,
    acfPaths=eAcfC)

  return (output)
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

logModel<-readRDS('C:/Users/Derek/Documents/GitHub/IS609/final_project/logModel.rds')
logE<-readRDS('C:/Users/Derek/Documents/GitHub/IS609/final_project/logE.rds')
logF<-readRDS('C:/Users/Derek/Documents/GitHub/IS609/final_project/logF.rds')




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

# compute log return ACF
maxLag<-250
alphaCI<-0.05
logReturnACF<-acfPaths(logTwrReturn,maxLag,alphaCI)
# replace with ggplot2
matplot(logReturnACF$acfPaths,type='l')

# compute log true range ACF
maxLag<-250
alphaCI<-0.05
twrTrACF<-acfPaths(twrTrPaths,maxLag,alphaCI)
# replace with ggplot2
matplot(twrTrACF$acfPaths,type='l',main='')

# compute log true range ACF
maxLag<-250
alphaCI<-0.05
eACF<-acfPaths(logE,maxLag,alphaCI)
# replace with ggplot2
matplot(eACF$acfPaths,type='l',main='')

# compute log true range ACF
maxLag<-250
alphaCI<-0.05
fACF<-acfPaths(logF,maxLag,alphaCI)
# replace with ggplot2
matplot(fACF$acfPaths,type='l',main='')


# plot the energy ACF
energyIndex<-c(1,2,3,4,5,6,7,8,9,10)
matplot(twrTrACF$acfPaths[,energyIndex],main='Energy',type='l')

# plot the energy ACF
fxIndex<-c(11,12,13,14,15,16,17,18,19,20,21)
matplot(twrTrACF$acfPaths[,fxIndex],main='FX (Developed)',type='l')

fxEmergingIndex<-c(22,23,24,25,26,27,28,29)
matplot(twrTrACF$acfPaths[,fxEmergingIndex],main='FX (Emerging)',type='l')

grainIndex<-c(30,31,32,33,34,35,36,37,38,39,40)
matplot(twrTrACF$acfPaths[,grainIndex],main='Grain',type='l')

equityIndex<-c(41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59)
matplot(twrTrACF$acfPaths[,equityIndex],main='Equity Index (Developed)',type='l')

equityEmIndex<-c(60,61,62,63,64,65,66,67)
matplot(twrTrACF$acfPaths[,equityEmIndex],main='Equity Index (Emerging)',type='l')

meatIndex<-c(68,69,70)
matplot(twrTrACF$acfPaths[,meatIndex],main='Meat',type='l')

metalIndex<-c(71,72,73,74,75)
matplot(twrTrACF$acfPaths[,metalIndex],main='Metal',type='l')

ratesIndex<-c(76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95)
matplot(twrTrACF$acfPaths[,ratesIndex],main='Rates',type='l')

softIndex<-c(96,97,98,99,100,101,102,103,104,105,106,107)
matplot(twrTrACF$acfPaths[,softIndex],main='Soft',type='l')

stirIndex<-c(108,109,110,111,112,113)
matplot(twrTrACF$acfPaths[,stirIndex],main='STIR',type='l')

volIndex<-c(114,115)
matplot(twrTrACF$acfPaths[,volIndex],main='Volatility',type='l')

# show a sample of the scaling law estimated via linear regression


xLabel<-'Lag'
yLabel<-'ACF'
titleName<-'ACF (Calibrated) \n Residuals'
marketModelParameterScenarios <- (2:(maxLag+1))
table_figure<-round(t(eACF$percentileAcf),4)

g3_ACF<-plotSecenarioPerformanceWithCI(marketModelParameterScenarios,
  table_figure,titleName,xLabel,yLabel)
print(g3_ACF)

xLabel<-'Lag'
yLabel<-'ACF'
titleName<-'ACF (Model) \n Conditional Mean True Range'
marketModelParameterScenarios <- (2:(maxLag+1))
table_figure<-round(t(fACF$percentileAcf),4)

g4_ACF<-plotSecenarioPerformanceWithCI(marketModelParameterScenarios,
  table_figure,titleName,xLabel,yLabel)
print(g4_ACF)

xLabel<-'Lag (k)'
yLabel<-'ACF'
titleName<-'ACF (Actual) \n Log Return'
marketModelParameterScenarios <- (2:(maxLag+1))
table_figure<-round(t(logReturnACF$percentileAcf),4)

g1_ACF<-plotSecenarioPerformanceWithCI(marketModelParameterScenarios,
  table_figure,titleName,xLabel,yLabel)
print(g1_ACF)

xLabel<-'Lag (k)'
yLabel<-'ACF'
titleName<-'ACF (Actual) \n True Range'
marketModelParameterScenarios <- (2:(maxLag+1))
table_figure<-round(t(twrTrACF$percentileAcf),4)

g2_ACF<-plotSecenarioPerformanceWithCI(marketModelParameterScenarios,
                                       table_figure,titleName,xLabel,yLabel)
print(g2_ACF)
