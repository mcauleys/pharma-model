# Title: Clinical Trial Database Analysis
# Author: Scott McAuley
# Last Upodated: 2019-07-23

# TODO Have the trial search return results from partial names and not just full proper names
# TODO Create a way to illustrate overlapping trials in the same phase
# TODO 

library(ggplot2)
library(RPostgreSQL)
require(XML)
library(jsonlite)
library(gridExtra)
library(httr)

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

# Import Prices
d.prices <- read.csv(file = "NBI-Prices.csv", stringsAsFactors = FALSE)
names <- d.prices[1,]
names(d.prices) <- c("date", names(d.prices)[2:ncol(d.prices)])

d.prices <- d.prices[-1,]
d.prices[, c(2: ncol(d.prices))] <- sapply(d.prices[, c(2: ncol(d.prices))], as.numeric)
d.prices[,1] <- as.Date(d.prices[,1], format = "%m/%d/%y")

tickers <- paste(names(d.prices)[2:length(names(d.prices))], collapse = ", ")

# Import index price movements
nbi <- read.csv(file = "NBI-Index.csv", stringsAsFactors = FALSE)

# Create a relative value by subtracting the index from the daily returns
d.prices.diff <- d.prices[,2:ncol(d.prices)] - nbi$ï..NBI.USA

d.prices.diff <- cbind(d.prices$date, d.prices.diff) 
names(d.prices.diff) <- c("date", names(d.prices.diff)[2:ncol(d.prices.diff)])

# Rearrange the dataframes
d.prices <- gather(d.prices, key = "ticker", value = "price.change", -date)
d.prices.diff <- gather(d.prices.diff, key = "ticker", value = "price.change", -date)

# Add the company names to the pricing dataframe
names <- cbind(t(names), names(names))
rownames(names) <- c()
names <- data.frame(names)

d.prices <- cbind(d.prices, names$X1[match(d.prices$ticker, names$V2)])
d.prices.diff <- cbind(d.prices.diff, names$X1[match(d.prices.diff$ticker, names$V2)])

names(d.prices) <- c("date", "ticker", "price.change", "name")
names(d.prices.diff) <- c("date", "ticker", "price.change", "name")


p1 <- ggplot(d.prices[d.prices$ticker == "ALXN.USA",]) +
      geom_line(aes(x = date, y = price.change))

p2 <- ggplot(d.prices[d.prices$ticker == "ALXN.USA",]) +
      geom_density(aes(price.change)) +
      coord_flip()

p3 <- ggplot(d.prices.diff[d.prices.diff$ticker == "ALXN.USA",]) +
      geom_line(aes(x = date, y = price.change))

p4 <- ggplot(d.prices.diff[d.prices.diff$ticker == "ALXN.USA",]) +
      geom_density(aes(price.change)) +
      coord_flip()

grid.arrange(p1, p2, p3, p4, nrow = 2)


# Import the press releases
press <- read.csv(file = "bw.csv", header = TRUE, stringsAsFactors = FALSE)

# Rearrange the dataframe
press <- gather(press, key = "ticker", value = "release")

# Remove any #NA values
press <- press[-which(press$release == "#N/A"),]

# Pull the press release date
pattern <- "[0-9//]+"
dates <- as.Date(regmatches(press$release, regexpr(pattern, press$release, perl = TRUE)), format = "%m/%d/%Y")
press <- cbind(press, dates)

pattern <- "(?<=BW\\s\\s\\s).+"
title <- as.character(regmatches(press$release, regexpr(pattern, press$release, perl = TRUE)))
press <- cbind(press, title)

dates <- data.frame(dates[!is.na(dates)])

pattern <- "(?<=BW\\s\\s\\s).+"
title <- regmatches(press$ï..MRNA.USA, regexpr(pattern, press$ï..MRNA.USA, perl = TRUE))
title <- data.frame(title[!is.na(title)])

releases <- cbind(dates, title)
names(releases) <- c("date", names(press))


# Build an entry
id <- "NCT02025985"

nct.id <- searchNameVariants("Seres Therapeutics")

id <- nct.id$nct_id[30]

df <- data.frame(
    drug.generic = flexTrialSearch("interventions", paste("nct_id = '",id,"'", sep = ""), "name"), # This may need to be updated if there are multiple interventions
    drug.id = flexTrialSearch("intervention_other_names", paste("nct_id = '",id,"'", sep = ""), "name"),
    start.date = flexTrialSearch("studies", paste("nct_id = '",id,"'", sep = ""), "start_date"),
    primary.end.date = flexTrialSearch("studies", paste("nct_id = '",id,"'", sep = ""), "primary_completion_date"),
    end.date = flexTrialSearch("studies", paste("nct_id = '",id,"'", sep = ""), "completion_date"),
    trial.name = flexTrialSearch("studies", paste("nct_id = '",id,"'", sep = ""), "official_title"),
    enrollment = flexTrialSearch("studies", paste("nct_id = '",id,"'", sep = ""), "enrollment"),
    nct.id = id,
    phase = flexTrialSearch("studies", paste("nct_id = '",id,"'", sep = ""), "phase"),
    pmid = getpmid(id)
)


test <- fdaLabel("Alexion")
test <- findLabelBrand("Alexion")
test <- findLabelTrials("Puma")
test$results$clinical_studies

pattern <- "\\(([^\\)]+)\\)"
pattern <- "\\(\\w+\\)"

# This works
pattern <- "(?<=\\()\\w+"
regmatches(test$results$description, regexpr(pattern, test$results$description, perl = TRUE)) 


test$results$description

pmid[["IdList"]]

flexTrialSearch("links", paste("nct_id = '",id,"'", sep = ""), "*")


flexTrialSearch("interventions", "name = 'Selinexor'", "*")


trials <- searchNameVariants("Karyopharm Therapeutics Inc.")

test <- flexTrialSearch("studies", paste("nct_id = '",id,"'", sep = ""), "start_date, completion_date, official_title, enrollment, phase")


kpti <- trialData(trials$nct_id[1])
kpti.therapy <- trialIntervention(trials$nct_id)

sel <- trialSearch("interventions", "name = 'Selinexor'")


plotTrial <- function(df){

    nudge <- position_nudge(y = seq(-0.3, 0.3, length.out=nrow(tetra)))
    
    ggplot(data) +
    geom_point(aes(x = start_date, y = phase, size = enrollment, color = overall_status), position = nudge) +
    geom_point(aes(x = completion_date, y = phase, size = enrollment, color = overall_status), position = nudge) +
    geom_segment(aes(x = start_date, y = phase, xend = completion_date, yend = phase, color = overall_status), position = nudge) +
    xlab("Date") +
    ylab("") +
    labs(color = "Trial Status", size = "Enrollment Size") + 
    scale_x_date(date_labels = "%Y", breaks='2 years') + 
    theme(axis.line = element_line())
    
    
    
    scale_color_manual(breaks = c("Completed", "Recruiting", "Not yet recruiting", "Known status", "Terminated", "Withdrawn"),
                       values=c("green", "blue", "yellow", "grey", "red", "orange"))
    
}


data <- data[,c("nct_id", "start_date", "completion_date", "brief_title", "official_title", "overall_status", "phase", "enrollment", "source")]
therapy <- "Topical A"

new <- c("N/A", "2019-09-01", "2020-03-01", "Proof of concept study for slow release Dranabinol formulation", "Proof of concept study for slow release Dranabinol formulation", "Planning", "Phase 2", "0", "Tetra Bio-Pharma", "PPP002", "Pain")

png("tetra.png", width = 960, height = 480, units = "px", pointsize = 12)
ggplot(tetra) +
    geom_point(aes(x = start_date, y = therapy, size = 3, shape = phase, color = overall_status), position = nudge) +
    geom_point(aes(x = completion_date, y = therapy, size =3, shape = phase, color = overall_status), position = nudge) +
    geom_segment(aes(x = start_date, y = therapy, xend = completion_date, yend = therapy, size = 2.5, color = overall_status), position = nudge) +
    #geom_label(aes(label = therapy, x = start_date, y = phase), position = nudge, hjust=0, vjust=-0.5) + 
    xlab("Date") +
    ylab("") +
    labs(color = "Trial Status", shape = "Trial Phase") + 
    scale_y_discrete(limits=c("PPP001", "PPP005", "PPP002", "Topical A", "PPP003", "PPP011")) +
    scale_x_date(date_labels = "%Y", breaks='1 year') + 
    guides(size = FALSE) +
    theme(axis.line = element_line(),
          axis.text = element_text(size = 20),
          axis.title = element_text(size = 20, face = "bold"),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 18))
dev.off()


e# References:
# https://aact.ctti-clinicaltrials.org/schema
