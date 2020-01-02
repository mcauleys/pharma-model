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
             gsub(", Inc|, Inc.| Inc| Inc.| Corporation| Corp|, Corp| Corp.|, Corp.| Ltd.| Ltd", "", companyname))
    
    var <- paste("name = '", var, "'", sep = "")
    
    for (name in var){
        ret <- trialSearch("sponsors", name)
        if (!is.na(names(ret)[1])){
            print(paste("Found: ", name))
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

#Build an entry

id <- "NCT02025985"

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




test <- fdaLabel("Karyopharm")
test$results$clinical_studies

getpmid(id)


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
