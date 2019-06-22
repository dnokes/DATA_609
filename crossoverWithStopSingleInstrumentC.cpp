#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List crossoverWithStopSingleInstrumentC(List strategyInput) {
  //-------------------------------------------------------------------------------------
  // extract strategy inputs/parameters
  //-------------------------------------------------------------------------------------
  NumericMatrix pricePaths = as<NumericMatrix>(strategyInput["pricePaths"]);
  NumericMatrix trueRangePaths = as<NumericMatrix>(strategyInput["trueRangePaths"]);
  int atrLookback = as<int>(strategyInput["atrLookback"]);
  double atrMultiplier = as<double>(strategyInput["atrMultiplier"]);
  int fastLookback = as<int>(strategyInput["fastLookback"]);
  int slowLookback = as<int>(strategyInput["slowLookback"]);
  bool longOnly = as<bool>(strategyInput["longOnly"]);
  double commissionPerShare = as<double>(strategyInput["commissionPerShare"]);
  double accountSize = as<double>(strategyInput["accountSize"]);
  double fPercent = as<double>(strategyInput["fPercent"]);
  double minRisk = as<double>(strategyInput["minRisk"]);
  double stopTWR = as<double>(strategyInput["stopTWR"]);
  
  // find the longest lookback
  int lookback=max(NumericVector::create(atrLookback,fastLookback,slowLookback));
  
  // determine the number of rows (T) and paths (instruments)
  int T = pricePaths.nrow();
  int nPaths = pricePaths.ncol();
  // output
  NumericVector tradeSize(nPaths);
  NumericVector tradePrice(nPaths);
  NumericVector exitMarkToMarket(nPaths);
  NumericVector costOfExposure(nPaths);
  NumericVector PnL_Realized(nPaths);
  NumericVector PnL_Unrealized(nPaths);
  NumericVector Total_Commission(nPaths);
  
  //-------------------------------------------------------------------------------------
  // create the output arrays
  //-------------------------------------------------------------------------------------   
  NumericMatrix trades(T,nPaths);
  NumericMatrix position(T,nPaths);
  NumericMatrix stopLevel(T,nPaths);
  
  NumericMatrix cumulativeCommission(T,nPaths);
  NumericMatrix cumulativePnL(T,nPaths);
  //NumericMatrix cumulativeRealizedPnL(T,nPaths);
  NumericMatrix equityCurves(T,nPaths); 
  NumericMatrix closedEquityCurves(T,nPaths);
  NumericMatrix twr(T,nPaths); 
  //NumericVector totalCommission(T);
  //NumericVector equityCurve(T);
  //NumericVector closedEquityCurve(T);
  
  //NumericVector riskCapital(nPaths);
  //NumericVector cash(nPaths);
  
  //NumericVector totalCash(nPaths);
  //NumericVector totalRiskCapital(nPaths);
  //NumericVector numberOfPositions(nPaths);
  
  NumericMatrix emaFast(T,nPaths);
  NumericMatrix emaSlow(T,nPaths);
  NumericMatrix atr(T,nPaths);

  double aFast = 2.0 / ( double(fastLookback) + 1.0 );
  double aSlow = 2.0 / ( double(slowLookback) + 1.0 );
  double aATR = 2.0 / ( double(atrLookback) + 1.0 );
  
  // iterate over each point in time
  for(int t = 1; t < T; t++){
    //  iterate over each price path
    for(int pathIndex=0; pathIndex<nPaths; pathIndex++){
      //-------------------------------------------------------------------------------------
      // indicators
      //-------------------------------------------------------------------------------------   
      
      // initialize indicators
      if (t==1){
        emaFast(0,pathIndex) = pricePaths(0,pathIndex); 
        emaSlow(0,pathIndex) = pricePaths(0,pathIndex); 
        atr(0,pathIndex) = trueRangePaths(0,pathIndex); 
        equityCurves(0,pathIndex)=accountSize;
        closedEquityCurves(0,pathIndex)=accountSize;
        twr(0,pathIndex)=1;
      }
      //   
      emaFast(t,pathIndex)=aFast * (pricePaths(t,pathIndex)-emaFast(t-1,pathIndex)) + emaFast(t-1,pathIndex);
      //
      emaSlow(t,pathIndex)=aSlow * (pricePaths(t,pathIndex)-emaSlow(t-1,pathIndex)) + emaSlow(t-1,pathIndex);
      //
      atr(t,pathIndex)=aATR * (trueRangePaths(t,pathIndex)-atr(t-1,pathIndex)) + atr(t-1,pathIndex);
      
      //-------------------------------------------------------------------------------------
      // trading model
      //-------------------------------------------------------------------------------------      
      
      if (t>lookback){
        // we are flat - start processing when we do not have a position
        // 
        if ( (position(t-1,pathIndex) == 0 ) & (twr(t-1,pathIndex) > stopTWR) ){
          // trend is up, rank is high enough, and we have less than nPositions
          if ( ( emaFast(t-1,pathIndex) > emaSlow(t-1,pathIndex) ) ){
            // go long
            trades(t,pathIndex)=floor( ( fPercent*(closedEquityCurves(t-1,pathIndex)) ) / max(NumericVector::create(atr(t-1,pathIndex)*atrMultiplier,minRisk) ));
            // set the initial stop (multiplier x atr) below the current price
            stopLevel(t,pathIndex) = pricePaths(t,pathIndex) - (atr(t-1,pathIndex)*atrMultiplier);
            // adjust cash to reflect new long position (removes cash)
            //cash[pathIndex]=(trades(t,pathIndex)*pricePaths(t,pathIndex))*-1.0;
            // adjust risk capital to reflect new long position
            //riskCapital[pathIndex]=(atr(t-1,pathIndex)*atrMultiplier)*trades(t,pathIndex); 
            // increment number of positions
            //numberOfPositions[t]=numberOfPositions[t]+1;
          } // end long condition
          
          // trend is down and rank is high enough
          else if ( ( emaFast(t-1,pathIndex) < emaSlow(t-1,pathIndex) ) & (longOnly==false) ){
            // go short
            trades(t,pathIndex) = -floor( ( fPercent*(closedEquityCurves(t-1,pathIndex)) ) / max(NumericVector::create(atr(t-1,pathIndex)*atrMultiplier,minRisk) ));
            // place the initial stop (multiplier x atr) above the current price
            stopLevel(t,pathIndex) = pricePaths(t,pathIndex) + (atr(t-1,pathIndex)*atrMultiplier);
            
            // adjust cash to reflect new short position (adds cash)
            //cash[pathIndex] = (trades(t,pathIndex)*pricePaths(t,pathIndex))*-1.0;
            // adjust risk capital to reflect new short position
            //riskCapital[pathIndex] = (atr(t-1,pathIndex)*atrMultiplier)*trades(t,pathIndex);
            // increment number of positions
            //numberOfPositions[t]=numberOfPositions[t]+1;
          } // end short condition
          
        } // end processing when we do not have a position
        
        // we have a position - start processing when we have a position
        else if (position(t-1,pathIndex) !=0){
          // if we are long - start we have a position and we are long
          if (position(t-1,pathIndex) > 0){
            
            // check if the price is at or below the stop or rank is too low
            if ( ( pricePaths(t,pathIndex) <= stopLevel(t-1,pathIndex) ) ){
              // trade to flatten position, clear stop
              trades(t,pathIndex)=position(t-1,pathIndex)*-1;
              // clear the stop level
              stopLevel(t,pathIndex)=0;
              
              // adjust the cash (adds cash)
              //cash(1,pathIndex)=(pricePaths(t,pathIndex)*trades(t,pathIndex))*-1.0; 
              // clear the risk capital 
              //riskCapital[pathIndex]=0;
              // decrement the number of positions
              //numberOfPositions[t]=numberOfPositions[t]-1;
            } // end check if the price is at or below the stop or rank is too low 
            
            // we are not stopped out maintain the position and update the stop level
            else{
              // update the stop
              // check if the stop should be moved up
              if ( ( pricePaths(t,pathIndex) - (atr(t-1,pathIndex)*atrMultiplier) ) > stopLevel(t-1,pathIndex) ){
                // update the stop
                stopLevel(t,pathIndex)=pricePaths(t,pathIndex) - (atr(t-1,pathIndex)*atrMultiplier);
              }
              // keep the stop the same
              else{
                stopLevel(t,pathIndex)=stopLevel(t-1,pathIndex);
              }
              
            } //end we are not stopped out
          
          } // end we have a position and we are long
          
          // if we are short - start we have a position and we are short
          else if ( ( position(t-1,pathIndex) < 0 ) ){
            // check if the price is at or above the stop or rank is too low
            if ( ( pricePaths(t,pathIndex) >= stopLevel(t-1,pathIndex) ) ){
              // trade to flatten position, clear stop
              trades(t,pathIndex)=position(t-1,pathIndex)*-1;
              // clear the stop level
              stopLevel(t,pathIndex)=0;
              
              // clear the risk capital 
              //riskCapital[pathIndex]=0.0; 
              // adjust the cash (removes cash)
              //cash[pathIndex]=(pricePaths(t,pathIndex)*trades(t,pathIndex))*-1.0; 
              // decrement number of positions
              //numberOfPositions[t]=numberOfPositions[t]-1;
            }
            // we are not stopped out maintain the position and update the stop level
            else{
              // update the stop
              // check if the stop should be moved down
              if ( (pricePaths(t,pathIndex) + (atr(t-1,pathIndex)*atrMultiplier) ) < stopLevel(t-1,pathIndex)){
                // update the stop
                stopLevel(t,pathIndex) = pricePaths(t,pathIndex) + (atr(t-1,pathIndex)*atrMultiplier);
              }
              // keep the stop the same
              else{
                stopLevel(t,pathIndex)=stopLevel(t-1,pathIndex);
              }
            }
            
          } // end we have a position and we are short
          
        } // end processing when we have a position
          
      } // end t>lookback

  //-------------------------------------------------------------------------------------
  // P&L
  //-------------------------------------------------------------------------------------        
        
      // Determine the trade size, mtm prices, and mtms
      if (t>0){
        // update position
        position(t,pathIndex) = position(t-1,pathIndex)+trades(t,pathIndex);
      }
      else{
        // initialize position
        position[pathIndex] = trades(t,pathIndex);
      }
      // infer the trade size
      tradeSize[pathIndex] = trades(t,pathIndex);
      // if we have a trade
      if (tradeSize[pathIndex] != 0){
        // determine the trade price
        tradePrice[pathIndex] = pricePaths(t,pathIndex);
        // determine the commission
        Total_Commission[pathIndex] = commissionPerShare*abs(tradeSize[pathIndex])*-1;
      }
      else{
        // set the trade price to zero
        tradePrice[pathIndex] = 0;
        // set commission is zero
        Total_Commission[pathIndex]=0;
      }
      // determine the  current liquidation value
      exitMarkToMarket[pathIndex] = pricePaths(t,pathIndex) * position(t,pathIndex);
        
      // Compute the P&L
        
      // if we have no trade
      if(tradeSize[pathIndex]==0){
        // update the unrealized P&L
        PnL_Unrealized[pathIndex] = exitMarkToMarket[pathIndex] - costOfExposure[pathIndex];
      }
      // if we have a trade
      else{
        // if we closed the position
        if(position(t,pathIndex)==0.0){
          // update the realized P&L
          PnL_Realized[pathIndex] = ( tradePrice[pathIndex] * position(t-1,pathIndex) - costOfExposure[pathIndex] + PnL_Realized[pathIndex] );
          // reset the cost
          costOfExposure[pathIndex] = 0.0;
          // rest the unrealized P&L
          PnL_Unrealized[pathIndex] = 0.0;
        } // end had a position and closed it
          
        // # if we have a position
        else{
          // carry forward the realized P&L
          PnL_Realized[pathIndex] = PnL_Realized[pathIndex];
          // update the cost of exposure (WAC)
          costOfExposure[pathIndex] = costOfExposure[pathIndex] + tradePrice[pathIndex] * tradeSize[pathIndex];
          // update the unrealized P&L
          PnL_Unrealized[pathIndex] = exitMarkToMarket[pathIndex] - costOfExposure[pathIndex];
        } // end have and trade and have a position
          
      } // end if we have a trade
        
      // compute the commission
      if (t>0){
        cumulativeCommission(t,pathIndex) = Total_Commission[pathIndex] + cumulativeCommission(t-1,pathIndex);
      }
      else{
        cumulativeCommission(t,pathIndex) = Total_Commission[pathIndex];
      }
      // cumulative P&L (realized + unrealized)
      cumulativePnL(t,pathIndex) = PnL_Realized[pathIndex] + PnL_Unrealized[pathIndex] + cumulativeCommission(t,pathIndex);
      //
      equityCurves(t,pathIndex)=cumulativePnL(t,pathIndex)+accountSize;
      //
      closedEquityCurves(t,pathIndex)=PnL_Realized[pathIndex]+accountSize;
      //
      twr(t,pathIndex)=equityCurves(t,pathIndex)/accountSize;
      
        
    } // end path loop
      
  //-------------------------------------------------------------------------------------
  // Total realized, unrealized, and total P&L
  //-------------------------------------------------------------------------------------     
      
    // total risk capital
    //totalRiskCapital[t]=sum(riskCapital);
    // sum total cash
    //totalCash[t]=sum(cash);
      
    // sum commission for each instrument in the portfolio
    //totalCommission[t]=sum(cumulativeCommission(t,_));
    // sum P&L for each instrument in the portfolio
    //equityCurve[t]=sum(cumulativePnL(t,_))+accountSize;
    // sum realized P&L for each instrument in the portfolio
    //closedEquityCurve[t]=sum(cumulativeRealizedPnL(t,_))+accountSize;
  } // end t loop
    
  return List::create(
    _["position"] = position,
    _["stopLevel"] = stopLevel,
    _["atr"] = atr,
    _["emaFast"] = emaFast,
    _["emaSlow"] = emaSlow,
    _["cumulativeCommission"] = cumulativeCommission,
    _["cumulativePnL"] = cumulativePnL,
    _["equityCurves"] = equityCurves,
    _["closedEquityCurves"] = closedEquityCurves,
    _["twr"] = twr);
}
