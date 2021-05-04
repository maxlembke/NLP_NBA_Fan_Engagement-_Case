#' Title: The effect of a Sh*tstorm: Analysis
#' Purpose: Data Cleaning
#' Author: Max Lembke

# Warning

# Some pre-processing steps can be limited by system memory and the single threaded 
# nature of R computations. Memory limits can be adjusted using the following code. 
# Be weary the current memory limit set might have adverse effects. 

memory.size() #Check memory 
memory.limit(size = 500000)#Adjust size limit 

# Set-up 

# Load packages 

library(tidyverse)
library(lubridate) 
library(tm)
library(qdap)
library(quanteda)

# Setting WD 

setwd("~/University/Master (MsBA)/Spring/Text Analysis/R - Files/hult_NLP_student/cases/NBA Fan Engagement/data")

options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

getwd()# Check if set properly

# Functions and cleaning

# To lower function 

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# Clean corpus function 

cleanCorpus <- function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, content_transformer(gsub),pattern = "hong|kong", replacement = "hongkong")#To avoid hong-kong seperated in different terms 
  corpus <- tm_map(corpus, content_transformer(gsub),pattern = "/hong/", replacement = "hongkong")#To avoid hong-kong seperated in different terms
  corpus <- tm_map(corpus, content_transformer(gsub),pattern = "/kong/", replacement = "hongkong")#To avoid hong-kong seperated in different terms
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) 
  corpus <- tm_map(corpus, content_transformer(gsub),pattern = "[^\x01-\x7F]", replacement = "")#remove non non-ASCII characters
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Clean column function for dfn cleaning 
#FYI: stopword removal is not handled in this function, as it is handled in the creation of the dfn 

cleanColumn <- function(column){
  column <- tryTolower(column)
  column <- gsub(pattern = "hong|kong", replacement = "hongkong", column)
  column <- qdapRegex::rm_url(column)
  column <- replace_contraction(column)
  column <- gsub(pattern = "[^\x01-\x7F]", replacement = "",column)
  column <- removeNumbers(column)
  column <- removePunctuation(column)
  column <- stripWhitespace(column)
  return(column)
}

# Stopword for clean corpus 

stops_corp <- c(stopwords('english'), 'amp', 'b', 'tb', 'itb', 'said', 'new', 'nba','game','preseason','basketball','win','now','live','will','last','just', 'team','first','like','one')

# Stopword for clean dfn 

stops_dfn <- c(stopwords('english'), 'amp', 'b', 'tb', 'itb', 'said', 'new', 'nba','game','preseason','basketball','win','now','live','will','last','just', 'team','first','like','one', 'rt','vs','g','m','th')

# Data Reading and Prep 

# Reading data
text_oct <- read.csv("~/University/Master (MsBA)/Spring/Text Analysis/R - Files/hult_NLP_student/cases/NBA Fan Engagement/data/A_Oct2019.csv")

text_oct <- data.frame(text_oct)

# Renaming date field & simplifying date 
text_oct = text_oct %>% 
  rename(date = created)

text_oct$date <- parse_date_time(text_oct$date, orders = "ymd HMS")
text_oct$date <- as.Date(text_oct$date, format = "%m-%d-%y", origin = "ymd HMS")

# Drop Team column (data minimization)

text_oct <- subset(text_oct, select = -c(team))

# Filtering out prev. tweets that are irrelevant (data minimization)

text <- text_oct %>% 
  filter(text_oct$date > as.Date("2019-10-04") & text_oct$date <= as.Date("2019-10-31") )

# Drawing random sample 

set.seed(1) #Always used seed (1) 
text_sample <- sample_frac(text, 0.01)

# Creating copy for dfn 

text_dfn <- text

# Corpus for DTM/DTM (Please be aware that several sample sizes were taken on the same seed,to program and then finally the full size was used)

# Building sample corpus 
txtCorpus_s <- VCorpus(DataframeSource(text_sample)) 

# Cleaning sample 
txtCorpus_s <- cleanCorpus(txtCorpus_s,stops_corp)

# Saving cleaned sample corpus
cleaned_sample <- data.frame(text = unlist(sapply(txtCorpus_s, `[`, "content")),
                             stringsAsFactors=F)

write.csv(cleaned_sample,"data_oct_all.csv", row.names = FALSE)

# Corpus for dfm:

# Cleaning with respective function (has to be done first due to function of corpus function vs. VCorpus)

text_dfn$text <- cleanColumn(text_dfn$text)

# Saving cleaned sample corpus 

write.csv(text_dfn,"data_oct_dfn.csv", row.names = TRUE)

# Inital Data Exploration - Can be ignored 

# Creating DTM/TDM of sample

txtDtm_s  <- DocumentTermMatrix(txtCorpus_s)
txtTdm_s  <- TermDocumentMatrix(txtCorpus_s)
txtDtmM_s <- as.matrix(txtDtm_s)
txtTdmM_s <- as.matrix(txtTdm_s)

# Get the most frequent terms

topTermsA <- colSums(txtDtmM_s)

# Add the terms

topTermsA <- data.frame(terms = colnames(txtDtmM_s), freq = topTermsA)

# Remove row attributes

rownames(topTermsA) <- NULL

# Order

sampleReOrder <- topTermsA[order(topTermsA$freq, decreasing = T),]

# Building corpus for this analysis (needed due to different date)

text_dfn_filtered <- text_dfn%>% 
  filter(text_dfn$date < as.Date("2019-10-24")) 

myCorpus_filtered <- corpus(text_dfn_filtered$text, 
                            docvars = data.frame(date = as.Date(text_dfn_filtered$date, "%m-%d-%y")))

# Building dfm 
myDfm_filtered <- dfm(myCorpus_filtered, remove = stops_dfn, groups = "date")

# Sorting dfm (not required
myDfm_filtered <- dfm_sort(myDfm_filtered, decreasing = TRUE, "features")


# Ranking the terms 
featureRanksByDate <- as.data.frame(t(apply(myDfm_filtered, 1, order, decreasing = TRUE)))

# Mapping the term names 
names(featureRanksByDate) <- featnames(myDfm_filtered)

# Top 20 terms per day 
n <- 20
as.data.frame(apply(featureRanksByDate, 1, function(x) {
  todaysTopFeatures <- names(featureRanksByDate)
  names(todaysTopFeatures) <- x
  todaysTopFeatures[as.character(1:n)]
}), row.names = 1:n)

# Credit to initial code idea to https://stackoverflow.com/questions/33272308/word-frequency-over-time-by-user-in-r
