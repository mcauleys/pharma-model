.First <- function() {
  path_to_libraries <- "C:/Users/scott/R/R-Library"
  .libPaths(c(path_to_libraries, .libPaths()))
}


# Load Libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(RPostgreSQL)
require(XML)
library(jsonlite)
library(gridExtra)
library(httr)
library(tidytext) # For text analysis
library(quanteda) # For text anlaysis
library(cowplot) # For aligning ggplots

databaseConnect <- function(){
  #Establishes connection to the clinical trial database, reference https://aact.ctti-clinicaltrials.org/
  drv <- dbDriver('PostgreSQL')
  con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org", port=5432, user="smcaul1", password="o8ewfhluwf3hf8a3923")
  
  return(con)
}

trialSearch <- function(search, term){
  # Returns a dataframe of the trials that match the search terms
  # Ex: search = "sponsors", term = "name = 'Tetra Bio-Pharma'"
  con <- databaseConnect()
  query <- paste("Select * FROM", search, "WHERE", term, sep = " ")
  search.aact <- dbGetQuery(con, query)
  dbDisconnect(con)
  
  return(search.aact)
}    

trialData <- function(nct_id){
  # Returns dataframe of the trial information based on nct_id
  # A list of ids will return as separate rows in the data frame
  con <- databaseConnect()
  results <- data.frame()
  
  for (id in nct_id){
    query <- paste("Select * FROM studies WHERE nct_id = '", id, "'", sep = "")
    search.aact <- dbGetQuery(con, query)
    results <- rbind(results, search.aact)
  }
  
  dbDisconnect(con)
  
  return(results)
}

trialIntervention <- function(nct_id){
  # Returns dataframe of the trial information based on nct_id
  # A list of ids will return as separate rows in the data frame
  con <- databaseConnect()
  results <- data.frame()
  
  for (id in nct_id){
    query <- paste("Select * FROM interventions WHERE nct_id = '", id, "'", sep = "")
    search.aact <- dbGetQuery(con, query)
    results <- rbind(results, search.aact)
  }
  
  dbDisconnect(con)
  
  return(results)
}

searchNameVariants <- function(companyname){
  # This function creates name variants to submit into the trialSearch function
  # Ex: name = "Karyopharm Therapeutics Inc"
  # Returns a vector of variants ended by "", "Inc", and "Inc."
  var <- c(gsub(", Inc|, Inc.| Inc| Inc.| Corporation| Corp|, Corp| Corp.|, Corp.| Ltd.| Ltd", " Inc", companyname),
           gsub(", Inc|, Inc.| Inc| Inc.| Corporation| Corp|, Corp| Corp.|, Corp.| Ltd.| Ltd", " Inc.", companyname),
           gsub(", Inc|, Inc.| Inc| Inc.| Corporation| Corp|, Corp| Corp.|, Corp.| Ltd.| Ltd", "", companyname),
           gsub(", Inc|, Inc.| Inc| Inc.| Corporation| Corp|, Corp| Corp.|, Corp.| Ltd.| Ltd", ", Inc.", companyname),
           companyname)
  
  var <- unique(var)
  
  var <- paste("name = '", var, "'", sep = "")
  
  for (name in var){
    print(name)
    ret <- trialSearch("sponsors", name)
    if (!is.na(names(ret)[1])){
      print(paste("Found: ", name))
      
      ret <- ret[ret$lead_or_collaborator == "lead",]
      
      return(ret)
    }
  }
}

flexTrialSearch <- function(search, term, return){
  # Returns a dataframe of the trials that match the search terms
  # Reference https://aact.ctti-clinicaltrials.org/static/documentation/aact_schema.png
  # Ex: search = "sponsors", term = "name = 'Tetra Bio-Pharma'", return = * - for all
  con <- databaseConnect()
  query <- paste("Select ", return," FROM", search, "WHERE", term, sep = " ")
  search.aact <- dbGetQuery(con, query)
  dbDisconnect(con)
  
  if (is_empty(search.aact)){
    search.aact <- NA
  }
  
  return(search.aact)
}   

getpmid <- function(nct.id){
  tmp <- GET(paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=", nct.id, sep = ""))
  tmp <- xmlParse(tmp)
  tmp <- xmlToList(tmp)
  
  
  if (is.null(unlist(tmp$IdList))){
    return(NA)
  } else{
    return(unlist(tmp$IdList))
  }
}

fdaLabel <- function(company.name){
  label <- fromJSON(paste("https://api.fda.gov/drug/label.json?search=openfda.manufacturer_name:'", company.name,"'&limit=50", sep = ""))
  return(label)
}

findLabelTrials <- function(label){
  pattern <- "NCT\\w+"
  trial <- regmatches(label$results$clinical_studies, regexpr(pattern, label$results$clinical_studies)) 
  
  return(trial)
}

findLabelBrand <- function(label){
  pattern <- "(?<=DESCRIPTION\\s)\\w+"
  
  brands <- c()
  
  for (des in label$results$description){
    brands <- c(brands, regmatches(des, regexpr(pattern, des, perl = TRUE)))
  }
  
  return(brands)
}


# ================ Data visualizations ================

plot.prices.diff <- function(prices.df, press.df, ticker, phrase){
  # This plots the timecourse of the price differences and the density function
  # Then searches for the phrase and plots it as a red line on both the timecourse and price figures
  
  d <- press.df[press.df$ticker == ticker,][grep(phrase, press.df$text[press.df$ticker == ticker]), "dates"]
  p <- press.df[press.df$ticker == ticker,][grep(phrase, press.df$text[press.df$ticker == ticker]), "price.change"]

  p1 <- ggplot(prices.df[prices.df$ticker == ticker,]) +
    geom_line(aes(x = date, y = market.cap)) +
    geom_vline(xintercept = as.Date(d), colour = "red")
  
  p2 <- ggplot(prices.df[prices.df$ticker == ticker,]) +
    geom_line(aes(x = date, y = price.change)) +
    geom_vline(xintercept = as.Date(d), colour = "red")
  
  p3 <- ggplot(prices.df[prices.df$ticker == ticker,]) +
    geom_density(aes(price.change)) +
    geom_vline(xintercept = p, colour = "red") +
    coord_flip()
  
  grid.arrange(p1, p2, p3, nrow = 1)
  
}

plot.total.density <- function(prices.df, press.df, phrase){
  
  p <- press.df[grep(phrase, press.df$text), "price.change"]
  
  ggplot(prices.df) +
    geom_density(aes(price.change)) +
    geom_vline(xintercept = p, colour = "red") +
    xlim(c(-100, 100))

}

tidy.press <- function(release, ws){
  press <- gather(release, key = "ticker", value = "release")
  
  # Clean up the data by removing any #NA values and empty press releases
  press <- press[-which(press$release == "#N/A"),]
  
  if (ws == "BW"){
    pattern <- "BW$" # Will need to figure out a way to include the other wire services
    press <- press[-grep(pattern, press$release, perl = TRUE),] 
  }
  
  if (ws != "BW"){
    press <- press[-which(press$release == ""),]
  }
  
  # Pull the press release date
  pattern <- "[0-9//]+"
  dates <- as.Date(regmatches(press$release, regexpr(pattern, press$release, perl = TRUE)), format = "%m/%d/%Y")
  press <- cbind(press, dates)
  
  pattern <- paste("(?<=", ws, "\\s).+", sep = "")
  title <- as.character(regmatches(press$release, regexpr(pattern, press$release, perl = TRUE)))
  press <- cbind(press, title)
  
  # Remove the original column
  press <- press[,c(1,3,4)]
  
  # Clean the initial ticker
  press$ticker[press$ticker == "ï..LGND.USA"] <- "LGND.USA"
  
  return(press)
}

