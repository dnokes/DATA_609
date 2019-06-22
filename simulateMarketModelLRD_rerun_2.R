#-------------------------------------------------------------------------
# load packages
#-------------------------------------------------------------------------

library(arfima)
library(Rcpp)

#-------------------------------------------------------------------------
# convert true range to sigma (based on Brunetti & Lildholdt (2002))
#-------------------------------------------------------------------------
range2Sigma <- function (tr){
  # Brunetti & Lildholdt (2002) provides the relationship
  # between volatility and range
  sigma_tr<-tr*sqrt(pi/8)
  return (sigma_tr)
}

#-------------------------------------------------------------------------
# simulate N true range paths based on EFARIMA model
#-------------------------------------------------------------------------
simulateTrueRangeNPaths_LRD<-function(nRows,nPaths,parameterListLRD,
                                      xSigma2,xMean){

  xSigma<-sqrt(xSigma2)

  tr<-matrix(0,nRows,nPaths);

  for (pathIndex in 1:nPaths){
    #
    e<-arfima.sim(nRows,parameterListLRD)
    #
    tr[,pathIndex]<-exp(e*xSigma+xMean)

  }
  #
  return (tr)
}

#-------------------------------------------------------------------------
# simulate nPaths price and true range paths based on CARR model
#-------------------------------------------------------------------------
singleMarketModel_LRD<-function(S0,mu,T,nRows,nPaths,parameterListLRD,
                                xSigma2,xMean){
  # simulate the true range based on EFARIMA model
  tr<-simulateTrueRangeNPaths_LRD((nRows-1),nPaths,parameterListLRD,
                                  xSigma2,xMean)
  # convert the true range to sigma using relationship in
  sigma_tr<-range2Sigma(tr)
  # simulate the return
  e <- matrix(rnorm((nRows-1)*nPaths,mean = 0, sd = 1),(nRows-1),nPaths)
  # determine delta time increment
  dt <- T/(nRows-1)
  # determine drift per delta time increment
  nudt_t <- (mu*dt)
  #nudt <- (mu-0.5*sigma_tr^2)*dt
  # add drift and multiply by true range (%)
  # (we multiply the true range percent by 100 during model estimation
  # so we have to divide by 100)
  increments <- nudt_t + ((sigma_tr)*e)
  # combine initial price (S0) and increments
  x <- rbind(matrix(log(S0),1,nPaths),increments)
  # convert returns to prices
  pricePaths=exp(apply(x,2,cumsum))
  # return price and true range paths
  return (list(pricePaths=pricePaths,tr=tr,sigma_tr=sigma_tr))
}

sourceCpp("C:/Users/Derek/Documents/GitHub/IS609/final_project/crossoverWithStopSingleInstrumentC.cpp")

fileDirectory<-'D:/MS/IS609/final_project/rerun_2/'

# define parameters
randomSeed<-1234567

# trading model parameters
atrLookback <- 20
atrMultiplier <- 4
fastLookback <- 120
slowLookback <- 180
longOnly <- FALSE
commissionPerShare <- 0
accountSize <- 100000
fPercent <- 0.01
minRisk <-0.001
stopTWR <-0.7

# market model parameters
nRowsScenario<-1250
nPathsScenario<-1000
S0<-1
T<-5

#
dScenario<-seq(0.05,0.45,by=0.05)
driftScenario<-seq(-0.1,0.1,by=0.005)
volLevelScenario<-log(seq(0.0005,0.0035,by=0.0005))
volOfVolScenario<-seq(0.1,0.9,by=0.05)

vovScenarioIndex<-2
vScenarioIndex<-4
xMean<-volLevelScenario[vScenarioIndex]
xSigma2<-volOfVolScenario[vovScenarioIndex]

#
nScenariosD<-length(dScenario)
nScenariosV<-length(volLevelScenario)
nScenariosVoV<-length(volOfVolScenario)
nScenarios<-length(driftScenario)

driftMeanTrueRangeACF<-matrix(0,maxLag,nScenarios)
driftScenarioTWR<-matrix(0,nScenarios,nPathsScenario)
driftScenarioPriceTWR<-matrix(0,nScenarios,nPathsScenario)

meanTwrByScenarioDrift<-matrix(0,nScenariosD,nScenarios)
meanPriceTwrByScenarioDrift<-matrix(0,nScenariosD,nScenarios)

twrPercentileByScenarioDriftL<-matrix(0,nScenariosD,nScenarios)
twrPercentileByScenarioDriftM<-matrix(0,nScenariosD,nScenarios)
twrPercentileByScenarioDriftU<-matrix(0,nScenariosD,nScenarios)

priceTwrPercentileByScenarioDriftL<-matrix(0,nScenariosD,nScenarios)
priceTwrPercentileByScenarioDriftM<-matrix(0,nScenariosD,nScenarios)
priceTwrPercentileByScenarioDriftU<-matrix(0,nScenariosD,nScenarios)




startTime<-proc.time()

for (dScenarioIndex in 1:nScenariosD){
  #
  d<-dScenario[dScenarioIndex]
  #
  parameterListD=list(dfrac=d)

  for (scenarioIndex in 1:nScenarios){
    # define drift
    mu<-driftScenario[scenarioIndex]
    # set random seed
    set.seed(randomSeed)

    # simulate paths
    result<-singleMarketModel_LRD(S0,mu,T,nRowsScenario,
      nPathsScenario,parameterListD,xSigma2,xMean)

    # store the terminal price (TWR)
    driftScenarioPriceTWR[scenarioIndex,]<-result$pricePaths[nRowsScenario,]

    # create the strategy input object
    strategyInput<-list(pricePaths=result$pricePaths,
      trueRangePaths=result$tr,
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

    # extract the strategy TWR
    driftScenarioTWR[scenarioIndex,]<-strategyOutput$twr[nRowsScenario,]

    # save trading model output
    fileName<-paste0('strategyOutput_dI_',dScenarioIndex,'_sI_',
      scenarioIndex,'_vI',vScenarioIndex,'_vovI_',vovScenarioIndex,'.rds')

    file<-paste0(fileDirectory,'/',fileName)
    saveRDS(strategyOutput, file)

    # save market model output
    fileName<-paste0('marketModelResult_dI_',dScenarioIndex,'_sI_',
       scenarioIndex,'_vI',vScenarioIndex,'_vovI_',vovScenarioIndex,'.rds')

    file<-paste0(fileDirectory,'/',fileName)
    saveRDS(result, file)

    # # save price paths
    # fileName<-paste0('pricePaths_dI_',dScenarioIndex,'_sI_',
    #   scenarioIndex,'_vI',vScenarioIndex,'_vovI_',vovScenarioIndex,'.rds')
    #
    # file<-paste0(fileDirectory,'/',fileName)
    # saveRDS(result$pricePaths, file)
    #
    # # save true range paths
    # fileName<-paste0('tr_dI_',dScenarioIndex,'_sI_',
    #   scenarioIndex,'_vI',vScenarioIndex,'_vovI_',vovScenarioIndex,'.rds')
    #
    # file<-paste0(fileDirectory,'/',fileName)
    # saveRDS(result$tr, file)
  }

  alphaCI<-0.05
  lowerPercentile<-alphaCI/2
  upperPercentile<-1-alphaCI/2

  # save true range paths
  fileName<-paste0('driftScenarioTWR_dI_',dScenarioIndex,'_sI_',
    scenarioIndex,'_vI',vScenarioIndex,'_vovI_',vovScenarioIndex,'.rds')

  file<-paste0(fileDirectory,'/',fileName)
  saveRDS(driftScenarioTWR, file)

  #
  twrPercentileByScenarioDrift<-apply(driftScenarioTWR,1, quantile,
    probs=c(lowerPercentile,0.5,upperPercentile), na.rm=TRUE)
  meanTwrByScenarioDrift[dScenarioIndex,]<-apply(driftScenarioTWR,1,
    mean, na.rm=TRUE)

  twrPercentileByScenarioDriftL[dScenarioIndex,]<-twrPercentileByScenarioDrift[1,]
  twrPercentileByScenarioDriftM[dScenarioIndex,]<-twrPercentileByScenarioDrift[2,]
  twrPercentileByScenarioDriftU[dScenarioIndex,]<-twrPercentileByScenarioDrift[3,]

  #
  priceTwrPercentileByScenarioDrift<-apply(driftScenarioPriceTWR,1, quantile,
    probs=c(lowerPercentile,0.5,upperPercentile), na.rm=TRUE)
  meanPriceTwrByScenarioDrift[dScenarioIndex,]<-apply(driftScenarioPriceTWR,1,
    mean, na.rm=TRUE)

  priceTwrPercentileByScenarioDriftL[dScenarioIndex,]<-priceTwrPercentileByScenarioDrift[1,]
  priceTwrPercentileByScenarioDriftM[dScenarioIndex,]<-priceTwrPercentileByScenarioDrift[2,]
  priceTwrPercentileByScenarioDriftU[dScenarioIndex,]<-priceTwrPercentileByScenarioDrift[3,]

}

endTime<-proc.time()
runTime_drift<-endTime-startTime

# strategy against drift

plotIndex<-1

matplot(driftScenario,cbind(twrPercentileByScenarioDriftL[plotIndex,],
  meanTwrByScenarioDrift[plotIndex,],
  twrPercentileByScenarioDriftU[plotIndex,]),type='l',
  main=paste0('Trend Following - Sensitivity to Drift\n (d=',dScenario[plotIndex],')'),
  xlab='Drift',
  ylab='Terminal Wealth Relative (TWR)')


plotIndex<-7

matplot(driftScenario,cbind(twrPercentileByScenarioDriftL[plotIndex,],
  meanTwrByScenarioDrift[plotIndex,],
  twrPercentileByScenarioDriftU[plotIndex,]),type='l',
  main=paste0('Trend Following - Sensitivity to Drift\n (d=',dScenario[plotIndex],')'),
  xlab='Drift',
  ylab='Terminal Wealth Relative (TWR)')


plotIndex<-9

matplot(driftScenario,cbind(twrPercentileByScenarioDriftL[plotIndex,],
  meanTwrByScenarioDrift[plotIndex,],
  twrPercentileByScenarioDriftU[plotIndex,]),type='l',
  main=paste0('Trend Following - Sensitivity to Drift\n (d=',dScenario[plotIndex],')'),
  xlab='Drift',
  ylab='Terminal Wealth Relative (TWR)')

# underlying against drift scenario

plotIndex<-1

matplot(driftScenario,
        cbind(priceTwrPercentileByScenarioDriftL[plotIndex,],
              meanPriceTwrByScenarioDrift[plotIndex,],
              priceTwrPercentileByScenarioDriftU[plotIndex,]),type='l',
        main=paste0('Trend Following - Sensitivity to Drift\n (d=',dScenario[plotIndex],')'),
        xlab='Drift',
        ylab='Terminal Wealth Relative (TWR)')

plotIndex<-7

matplot(driftScenario,
        cbind(priceTwrPercentileByScenarioDriftL[plotIndex,],
              meanPriceTwrByScenarioDrift[plotIndex,],
              priceTwrPercentileByScenarioDriftU[plotIndex,]),type='l',
        main=paste0('Trend Following - Sensitivity to Drift\n (d=',dScenario[plotIndex],')'),
        xlab='Drift',
        ylab='Terminal Wealth Relative (TWR)')

plotIndex<-9

matplot(driftScenario,
        cbind(priceTwrPercentileByScenarioDriftL[plotIndex,],
              meanPriceTwrByScenarioDrift[plotIndex,],
              priceTwrPercentileByScenarioDriftU[plotIndex,]),type='l',
        main=paste0('Trend Following - Sensitivity to Drift\n (d=',dScenario[plotIndex],')'),
        xlab='Drift',
        ylab='Terminal Wealth Relative (TWR)')


# strategy against underlying
plotIndex<-1

matplot(meanPriceTwrByScenarioDrift[plotIndex,],
        cbind(twrPercentileByScenarioDriftL[plotIndex,],
              meanTwrByScenarioDrift[plotIndex,],
              twrPercentileByScenarioDriftU[plotIndex,]),type='l',
        main=paste0('Trend Following - Sensitivity to Drift\n (d=',dScenario[plotIndex],')'),
        xlab='Mean Underlying Level',
        ylab='Terminal Wealth Relative (TWR)')


plotIndex<-7

matplot(meanPriceTwrByScenarioDrift[plotIndex,],
        cbind(twrPercentileByScenarioDriftL[plotIndex,],
              meanTwrByScenarioDrift[plotIndex,],
              twrPercentileByScenarioDriftU[plotIndex,]),type='l',
        main=paste0('Trend Following - Sensitivity to Drift\n (d=',dScenario[plotIndex],')'),
        xlab='Mean Underlying Level',
        ylab='Terminal Wealth Relative (TWR)')

plotIndex<-9

matplot(meanPriceTwrByScenarioDrift[plotIndex,],
        cbind(twrPercentileByScenarioDriftL[plotIndex,],
              meanTwrByScenarioDrift[plotIndex,],
              twrPercentileByScenarioDriftU[plotIndex,]),type='l',
        main=paste0('Trend Following - Sensitivity to Drift\n (d=',dScenario[plotIndex],')'),
        xlab='Mean Underlying Level',
        ylab='Terminal Wealth Relative (TWR)')


# underlying against drift
plotIndex<-1

matplot(meanPriceTwrByScenarioDrift[plotIndex,],
        cbind(priceTwrPercentileByScenarioDriftL[plotIndex,],
              priceTwrPercentileByScenarioDriftM[plotIndex,],
              priceTwrPercentileByScenarioDriftU[plotIndex,]),type='l',
        main=paste0('Trend Following - Sensitivity to Drift\n (d=',dScenario[plotIndex],')'),
        xlab='Mean Underlying Level',
        ylab='Terminal Wealth Relative (TWR)')


plotIndex<-7

matplot(meanPriceTwrByScenarioDrift[plotIndex,],
        cbind(priceTwrPercentileByScenarioDriftL[plotIndex,],
              priceTwrPercentileByScenarioDriftM[plotIndex,],
              priceTwrPercentileByScenarioDriftU[plotIndex,]),type='l',
        main=paste0('Trend Following - Sensitivity to Drift\n (d=',dScenario[plotIndex],')'),
        xlab='Mean Underlying Level',
        ylab='Terminal Wealth Relative (TWR)')


plotIndex<-9

matplot(meanPriceTwrByScenarioDrift[plotIndex,],
        cbind(priceTwrPercentileByScenarioDriftL[plotIndex,],
              priceTwrPercentileByScenarioDriftM[plotIndex,],
              priceTwrPercentileByScenarioDriftU[plotIndex,]),type='l',
        main=paste0('Trend Following - Sensitivity to Drift\n (d=',dScenario[plotIndex],')'),
        xlab='Mean Underlying Level',
        ylab='Terminal Wealth Relative (TWR)')

# export
fileName<-paste0('meanPriceTwrByScenarioDrift',
  '_vI_',vScenarioIndex,'_vovI_',vovScenarioIndex,'.rds')

file<-paste0(fileDirectory,'/',fileName)
saveRDS(meanPriceTwrByScenarioDrift, file)

fileName<-paste0('priceTwrPercentileByScenarioDriftL',
  '_vI_',vScenarioIndex,'_vovI_',vovScenarioIndex,'.rds')

file<-paste0(fileDirectory,'/',fileName)
saveRDS(priceTwrPercentileByScenarioDriftL, file)

fileName<-paste0('priceTwrPercentileByScenarioDriftM',
  '_vI_',vScenarioIndex,'_vovI_',vovScenarioIndex,'.rds')

file<-paste0(fileDirectory,'/',fileName)
saveRDS(priceTwrPercentileByScenarioDriftM, file)

fileName<-paste0('priceTwrPercentileByScenarioDriftU',
  '_vI_',vScenarioIndex,'_vovI_',vovScenarioIndex,'.rds')

file<-paste0(fileDirectory,'/',fileName)
saveRDS(priceTwrPercentileByScenarioDriftU, file)


fileName<-paste0('meanTwrByScenarioDrift',
  '_vI_',vScenarioIndex,'_vovI_',vovScenarioIndex,'.rds')

file<-paste0(fileDirectory,'/',fileName)
saveRDS(meanTwrByScenarioDrift, file)

fileName<-paste0('twrPercentileByScenarioDriftL',
  '_vI_',vScenarioIndex,'_vovI_',vovScenarioIndex,'.rds')

file<-paste0(fileDirectory,'/',fileName)
saveRDS(twrPercentileByScenarioDriftL, file)

fileName<-paste0('twrPercentileByScenarioDriftM',
  '_vI_',vScenarioIndex,'_vovI_',vovScenarioIndex,'.rds')

file<-paste0(fileDirectory,'/',fileName)
saveRDS(twrPercentileByScenarioDriftM, file)

fileName<-paste0('twrPercentileByScenarioDriftU',
  '_vI_',vScenarioIndex,'_vovI_',vovScenarioIndex,'.rds')

file<-paste0(fileDirectory,'/',fileName)
saveRDS(twrPercentileByScenarioDriftU, file)











