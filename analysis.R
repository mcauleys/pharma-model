# Title: Clinical Trial Database Analysis
# Author: Scott McAuley
# Last Upodated: 2019-07-23

# TODO Include other press release sources
# TODO Change the price correction to S&P 500 instead of the index
# TODO Allow the trial system to pull multiple trials at once

# ================ This code imports and processes the pricing data ================
# Import Prices
d.prices <- read.csv(file = "NBI-Prices.csv", stringsAsFactors = FALSE)
names <- d.prices[1,]
names(d.prices) <- c("date", names(d.prices)[2:ncol(d.prices)])

d.prices <- d.prices[-1,]
d.prices[, c(2: ncol(d.prices))] <- sapply(d.prices[, c(2: ncol(d.prices))], as.numeric)
d.prices[,1] <- as.Date(d.prices[,1], format = "%m/%d/%y")

tickers <- paste(names(d.prices)[2:length(names(d.prices))], collapse = ", ")

# Import index price movements
nbi <- read.csv(file = "indices.csv", stringsAsFactors = FALSE)

# Import market cap 
nbi.market.cap <- read.csv(file = "NBI-Mkt-Cap.csv", stringsAsFactors = FALSE)
names <- nbi.market.cap[1,]
names(nbi.market.cap) <- c("date", names(nbi.market.cap)[2:ncol(nbi.market.cap)])

nbi.market.cap <- nbi.market.cap[-1,]
nbi.market.cap[, c(2: ncol(nbi.market.cap))] <- sapply(nbi.market.cap[, c(2: ncol(nbi.market.cap))], as.numeric)
nbi.market.cap[,1] <- as.Date(nbi.market.cap[,1], format = "%m/%d/%y")

# Create a relative value by subtracting the index from the daily returns
d.prices.diff <- d.prices[,2:ncol(d.prices)] - nbi$ï..NBI.USA

d.prices.diff <- cbind(d.prices$date, d.prices.diff) 
names(d.prices.diff) <- c("date", names(d.prices.diff)[2:ncol(d.prices.diff)])

# Rearrange the dataframes
d.prices <- gather(d.prices, key = "ticker", value = "price.change", -date)
d.prices.diff <- gather(d.prices.diff, key = "ticker", value = "price.change", -date)
nbi.market.cap <- gather(nbi.market.cap, key = "ticker", value = "market.cap", -date)

# Add the company names to the pricing dataframe
names <- cbind(t(names), names(names))
rownames(names) <- c()
names <- data.frame(names)

d.prices <- cbind(d.prices, names$X1[match(d.prices$ticker, names$V2)])
d.prices.diff <- cbind(d.prices.diff, names$X1[match(d.prices.diff$ticker, names$V2)])

names(d.prices) <- c("date", "ticker", "price.change", "name")
names(d.prices.diff) <- c("date", "ticker", "price.change", "name")

d.prices <- d.prices[-which(is.na(d.prices$price.change)),]
d.prices.diff <- d.prices.diff[-which(is.na(d.prices.diff$price.change)),]

d.prices.diff <- merge(nbi.market.cap, d.prices.diff, by.x=c('ticker', 'date'), by.y=c('ticker','date'))

# ================ Import and process press release data ================
# Import the press releases
press <- read.csv(file = "bw.csv", header = TRUE, stringsAsFactors = FALSE)

# Rearrange the dataframe
press <- gather(press, key = "ticker", value = "release")

# Clean up the data by removing any #NA values and empty press releases
press <- press[-which(press$release == "#N/A"),]

pattern <- "BW$" # Will need to figure out a way to include the other wire services
press <- press[-grep(pattern, press$release, perl = TRUE),]

# Pull the press release date
pattern <- "[0-9//]+"
dates <- as.Date(regmatches(press$release, regexpr(pattern, press$release, perl = TRUE)), format = "%m/%d/%Y")
press <- cbind(press, dates)

pattern <- "(?<=BW\\s\\s\\s).+"
title <- as.character(regmatches(press$release, regexpr(pattern, press$release, perl = TRUE)))
press <- cbind(press, title)

# Remove the original column
press <- press[,c(1,3,4)]

# Clean the initial ticker
press$ticker[press$ticker == "ï..LGND.USA"] <- "LGND.USA"

# Add the corrected price movement to the press release day for the correct ticker
db <- merge(press, d.prices.diff, by.x=c('ticker', 'dates'), by.y=c('ticker','date'))

# ================ Load the already processed data ================
load(file = "return.RData")
load(file = "corr-return.RData")
load(file = "db.RData")

# ================ Search ticker news for keywords and plot on timecourse and density ================
plot.prices.diff(d.prices.diff, db, "SGEN.USA", "FDA Approval")

# ================ Search news for keywords and plot on total density plot ================
plot.total.density(d.prices.diff, db, "AVBS")

# ================ Find releases associated with biggest moves ================
avg <- mean(d.prices.diff$price.change)
sd <- sd(d.prices.diff$price.change)

pos <- avg + 3*sd
neg <- avg - 3*sd

test <- db[db$price.change > pos | db$price.change < neg,]
test <- db[((db$price.change > pos | db$price.change < neg) & db$ticker == "SPPI.USA"),]

# ================ Testing ================
db[db$ticker == "SPPI.USA",][grep("Phase 3", db$title[db$ticker == "SPPI.USA"]), "dates"]
test <- db[db$ticker == "SPPI.USA",][grep("Phase 3", db$title[db$ticker == "SPPI.USA"]), "price.change"]

test <- as.Date(c("2010-01-12", "2012-04-05"))

d.prices.diff[(d.prices.diff$date == test & d.prices.diff$ticker == "SPPI.USA"),]

d.prices.diff[(d.prices.diff$ticker == "SPPI.USA" & d.prices.diff$date == test),]

plot.prices.diff(d.prices.diff, "SPPI.USA", test)


ggplot(d.prices.diff[d.prices.diff$ticker == "ALXN.USA",]) +
  geom_point(aes(x = reorder(date, price.change), y = price.change)) +
  geom_vline(xintercept = as.Date("2010-01-15")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


# ================ Text mining processing ================

names(db) <- c("ticker", "dates", "text", "price.change", "name")
db$text <- as.character(db$text)
corp <- corpus(db)
tok <- tokens(corp)

kwic(tok, pattern =  phrase('Phase 3'))
tok_comp <- tokens_compound(tok, pattern = phrase(c('Phase 1', 
                                                    'Phase 2', 
                                                    'Phase 3')))

dfmat_inaug <- dfm(tok)
tstat_dist <- as.dist(textstat_dist(dfmat_inaug))
clust <- hclust(tstat_dist)
plot(clust, hang = -1, xlab = "Distance", ylab = NULL)

head(clust)

topfeatures(dfmat_inaug, 10)

release_words <- test %>%
  unnest_tokens(word, title)

b <- release_words %>%
     group_by(word) %>%
     summarize(return = mean(price.change),
            words = n())


release_words %>%
  count(word, sort = TRUE)

words_by_ticker <- release_words %>%
  count(ticker, word, sort = TRUE) %>%
  ungroup()

words_by_newsgroup

tf_idf <- words_by_ticker %>%
  bind_tf_idf(word, ticker, n) %>%
  arrange(desc(tf_idf))

tf_idf

db$title[db$price.change < -10]

match(d.prices.diff$date, press$dates)
match(d.prices.diff$ticker, press$ticker)


names$X1[match(d.prices$ticker, names$V2)]

d.pr

match(press$dates, d.prices.diff$date)

t <- d.prices.diff$price.change[match(press$dates, d.prices.diff$date)]


grep("Phase 1", press$title[press$ticker == "PGNX.USA"])

press$dates[press$ticker == "PGNX.USA"][grep("Phase 1", press$title[press$ticker == "PGNX.USA"])]



press[press$ticker == "PGNX.USA",]
press

# Build an entry
id <- "NCT02025985"

nct.id <- searchNameVariants("Seattle Genetics Inc.")

nct.id.t <- nct.id$nct_id[50:52]

id <- nct.id.t[1]

df <- data.frame(
  #drug.generic = flexTrialSearch("interventions", paste("nct_id = '",id,"'", sep = ""), "name"), # This may need to be updated if there are multiple interventions
  #drug.id = flexTrialSearch("intervention_other_names", paste("nct_id = '",id,"'", sep = ""), "name"),
  start.date = flexTrialSearch("studies", paste("nct_id = '",id,"'", sep = ""), "start_date"),
  primary.end.date = flexTrialSearch("studies", paste("nct_id = '",id,"'", sep = ""), "primary_completion_date"),
  end.date = flexTrialSearch("studies", paste("nct_id = '",id,"'", sep = ""), "completion_date"),
  trial.name = flexTrialSearch("studies", paste("nct_id = '",id,"'", sep = ""), "official_title"),
  enrollment = flexTrialSearch("studies", paste("nct_id = '",id,"'", sep = ""), "enrollment"),
  nct.id = id,
  phase = flexTrialSearch("studies", paste("nct_id = '",id,"'", sep = ""), "phase")
  #pmid = getpmid(id)
)

for(id in nct.id.t[2:length(nct.id.t)]){ 
  t <- data.frame(
                #drug.generic = flexTrialSearch("interventions", paste("nct_id = '",id,"'", sep = ""), "name"), # This may need to be updated if there are multiple interventions
                #drug.id = flexTrialSearch("intervention_other_names", paste("nct_id = '",id,"'", sep = ""), "name"),
                start.date = flexTrialSearch("studies", paste("nct_id = '",id,"'", sep = ""), "start_date"),
                primary.end.date = flexTrialSearch("studies", paste("nct_id = '",id,"'", sep = ""), "primary_completion_date"),
                end.date = flexTrialSearch("studies", paste("nct_id = '",id,"'", sep = ""), "completion_date"),
                trial.name = flexTrialSearch("studies", paste("nct_id = '",id,"'", sep = ""), "official_title"),
                enrollment = flexTrialSearch("studies", paste("nct_id = '",id,"'", sep = ""), "enrollment"),
                nct.id = id,
                phase = flexTrialSearch("studies", paste("nct_id = '",id,"'", sep = ""), "phase")
                #pmid = getpmid(id)
  )
  
  df <- rbind(df, t)
}


ggplot() +
  geom_line(data = d.prices.diff[d.prices.diff$ticker == "SGEN.USA",], aes(x = date, y = market.cap)) +
  geom_segment(data = df, aes(x = start_date, xend = primary_completion_date, y = jitter(1, 1), yend = jitter(1,1), colour = phase)) 

p1 <- ggplot(d.prices.diff[d.prices.diff$ticker == "SGEN.USA",]) +
        geom_line(aes(x = date, y = market.cap)) +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank())

p2 <- ggplot(df) +
        geom_segment(aes(x = start_date, xend = primary_completion_date, y = nct.id, yend = nct.id, colour = phase)) +
        xlim(c(as.Date("2010-01-01"), as.Date("2020-01-01")))

gA <- ggplotGrob(p1)
gB <- ggplotGrob(p2)
grid::grid.newpage()
grid::grid.draw(rbind(gA, gB))

grid.arrange(p1, p2, nrow = 2)

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
