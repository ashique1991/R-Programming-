
## Interactive Grapg Creation ############


#### Import Necessary Libraries
options(warn=-1)
# Load Library 
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(pdfetch)
library(dplyr)
######################
library(dygraphs)

##########################
# Importing Data set
twitter  <-pdfetch_YAHOO('TWTR34.SA')
spaceX <-pdfetch_YAHOO('ARKX')

###############Piloting Twitter Data###########
chartSeries(twitter, subset = 'last 10 month', type = 'auto',theme=chartTheme('white'))

candleChart(twitter$TWTR34.SA.adjclose, TA=c(addMACD(),addVo()), subset = '2022', theme=chartTheme('white'))

chartSeries(twitter$TWTR34.SA.adjclose, theme="white",
            TA="addVo();addBBands();addCCI()", subset = '2022-01::')


###############Piloting spaceX Data###########
chartSeries(spaceX, subset = 'last 10 month', type = 'auto',theme=chartTheme('white'))

candleChart(spaceX$ARKX.adjclose, TA=c(addMACD(),addVo()), subset = '2022', theme=chartTheme('white'))

chartSeries(spaceX$ARKX.adjclose, theme="white",
            TA="addVo();addBBands();addCCI()", subset = '2022-01::')


###############Plot My Data###########

########## Plottiong twitter Data with dygraph ######################

library(dplyr)
library(tidyverse)
library(quantmod)
twitter_last  <-pdfetch_YAHOO('TWTR34.SA', from = '2022-01-01', to = '2022-11-08')
spaceX_last <- pdfetch_YAHOO('ARKX', from = '2022-01-01', to = '2022-11-08')

twitter_close <- twitter_last$TWTR34.SA.adjclose
spaceX_close <- spaceX_last$ARKX.adjclose


##################

twitter_price <- ts(frequency = 12, start = c(2020, 12), end = c(2022,11),
                  data = c(twitter_close))
spaceX_price <- ts(frequency = 12, start = c(2020, 12),end = c(2022,11),
                    data = c(spaceX_close))

Mask <- cbind(twitter_price, spaceX_price)

 dygraph(Mask, main = "Impact of Twitter sell announcement ") %>%
  dySeries("spaceX_price", axis = 'y2') 
 
########### Adding Roll Periods ##################
 
 dygraph(Mask, main = "Impact of Twitter sell announcement ") %>%
   dySeries("spaceX_price", axis = 'y2') %>%
   dyRoller(rollPeriod = 5)
######################## Straw Broom Charts #################
 
 library(quantmod)

 tickers <- c('TWTR34.SA', 'ARKX')
 getSymbols(tickers)
 closePrices <- do.call(merge, lapply(tickers, function(x) Cl(get(x))))
 dateWindow <- c("2021-09-01", "2022-09-01")
 
 dygraph(closePrices, main = "Value", group = "stock") %>%
   dyRebase(value = 100) %>%
   dyRangeSelector(dateWindow = dateWindow)
 
 dygraph(closePrices, main = "Percent", group = "stock") %>%
   dyRebase(percent = TRUE) %>%
   dyRangeSelector(dateWindow = dateWindow)
 
 dygraph(closePrices, main = "None", group = "stock") %>%
   dyRangeSelector(dateWindow = dateWindow)



