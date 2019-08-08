.First <- function() {
  path_to_libraries <- "C:/Users/scott/R/R-Library"
  .libPaths(c(path_to_libraries, .libPaths()))
}


# Load Libraries
library(tidyverse)
library(ggplot2)
library(modelr)
library(splines) # ns function
library(zoo) # as.yearqtr function

CGR <- function(sales){
  # Find the compound growth rate for a vector containing timeseries of values, starting with the oldest value and ending with the most recent value
  CGR <- (sales[length(sales)]/sales[1])^(1/length(sales)) - 1
  return(CGR)
}





rev.forcast <- function(now, years, assump.fixed, rates, market){
  
  total.patients = ceiling(market$incidence*(1+market$growth)^(0:years))
  treat.price = assump.fixed$revenue.per.patient*(1+assump.fixed$price.inflation)^(0:years)
  market.share = seq(from = market$share.init, by = market$share.escalation, length.out = (years+1))
  treated.patients = total.patients*market.share
  sales = treat.price*treated.patients
  
  
  rev <- data.frame(year = seq(now, (now+years)),
                    total.patients = total.patients,
                    market.share = market.share,
                    treated.patients = treated.patients,
                    treat.price = treat.price,
                    sales = sales
  )
  
  return(rev)
  
}
