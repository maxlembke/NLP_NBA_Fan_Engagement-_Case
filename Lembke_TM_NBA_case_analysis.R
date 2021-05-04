#' Title: The effect of a Sh*tstorm: Analysis
#' Purpose: Analysis 
#' Author: Max Lembke

# Set up 

library(tidyverse)
library(lubridate) 
library(tm)
library(qdap)
library(ggplot2)
library(ggthemes)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(pbapply)
library(quanteda)
library(reshape2)
library(dplyr)
library(tidyr)

# Warning

# Some pre-processing steps can be limited by system memory and the single threaded 
# nature of R computations. Memory limits can be adjusted using the following code. 
# Be weary the current memory limit set might have adverse effects. 

memory.size() #Check memory 
memory.limit(size = 500000)#Adjust size limit 

# Set WD 

setwd("~/University/Master (MsBA)/Spring/Text Analysis/R - Files/hult_NLP_student/cases/NBA Fan Engagement/data")

options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

getwd()# Check if set properly

# Important Information 

# In order to accommodate for RAM issues, the rm() command to clear the global 
# environment is used frequently -> this has no other reason than to account for 
# RAM issues  by clearing the Global environment of not needed elements. 

# Analysis

##Basic Overview of Oct

# Plot 1: Word Cloud 

# Reading pre-prepared file (this can also be conducted through a sample to run code more swiftly - same can be adjusted in the cleaning rmd file)

text_oct <- read.csv("~/University/Master (MsBA)/Spring/Text Analysis/R - Files/hult_NLP_student/Private/Case I/data/data_oct_all.csv")

# Creating corpus 
txtCorpus_s <- VCorpus(VectorSource(text_oct))

# Removing the text_oct from memory to have more ram space 
rm(text_oct)

# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}

# Make bi-gram TDM according to the tokenize control & convert it to matrix

wordcloudTDM  <- TermDocumentMatrix(txtCorpus_s, 
                                    control=list(tokenize=bigramTokens))

# Text corpus not needed anymore, hence removed 
rm(txtCorpus_s)

# Creating matrix 
wordcloudTDMm <- as.matrix(wordcloudTDM)

# Get Row Sums & organize

wordcloudTDMv <- sort(rowSums(wordcloudTDMm), decreasing = TRUE)
wordcloudDF   <- data.frame(word = names(wordcloudTDMv), freq = wordcloudTDMv)

# Choose a color & drop light ones

pal <- brewer.pal(8, "Purples")
pal <- pal[-(1:2)]

# Creating simple word cloud

set.seed(1)
wordcloud(wordcloudDF $word,
          wordcloudDF $freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))


# Clear Environment
rm(list = ls())

# Plot 2: Top Terms

# Reading pre-prepared file 
text_oct <- read.csv("~/University/Master (MsBA)/Spring/Text Analysis/R - Files/hult_NLP_student/Private/Case I/data/data_oct_all.csv")

# Creating corpus 
txtCorpus_s <- VCorpus(VectorSource(text_oct))

# Removing the text_oct from memory to have more ram space 
rm(text_oct)

# Creating DTM/TDM of sample
txtTdm_s  <- TermDocumentMatrix(txtCorpus_s)

rm(txtCorpus_s)

txtTdmM_s <- as.matrix(txtTdm_s)

metionsoct <- data.frame(txtTdmM_s)

rm(txtTdmM_s)

# Filtering and renaming
metionsoct = metionsoct %>% 
  rename(frequency = X1)%>% 
  filter(frequency > 12548)

terms = c('hongkong','china','rockets','houston')

# Preparing for plot 
metionsoct<- mutate(metionsoct, word = rownames(metionsoct))
metionsoct <- metionsoct %>% 
  mutate(ToHighlight = ifelse(word %in% terms, "yes", "no" ))

# Plotting 
toptermsplot <- ggplot(data = metionsoct, aes(x=reorder(word, -frequency), y=frequency, fill= ToHighlight))+
  geom_bar(stat = "identity")+ 
  coord_flip()+
  theme_minimal()+ 
  scale_fill_manual( values = c( "yes"="lightgreen", "no"="steelblue" ), guide = FALSE )+
  labs(title = "Top Words in October 2019",
       x = "Frequency",
       y = "Term")

# Save plot 
ggsave("bartopterms.png", toptermsplot, bg = "transparent")

# Clear Environment
rm(list = ls())

##Time series analysis 

# Plot 1: Exploration over time

# Reading pre-prepared file for dfn 
text_dfn <- read.csv("~/University/Master (MsBA)/Spring/Text Analysis/R - Files/hult_NLP_student/Private/Case I/data/data_oct_dfn.csv")

text_dfn <- data.frame(text_dfn)

# Fixing date format -> required for dfn to work 
text_dfn$date <- parse_date_time(text_dfn$date, orders = "ymd")
text_dfn$date <- as.Date(text_dfn$date, format = "%m-%d-%y", origin = "ymd")

# Dropping x column 
text_dfn <- subset(text_dfn, select = -c(X))

# Stopwords for dfn creation 
stops_dfn <- c(stopwords('english'), 'amp', 'b', 'tb', 'itb', 'said', 'new', 'nba','game','preseason','basketball','win','now','live','will','last','just', 'team','first','like','one', 'rt','vs','g','m','th')


myCorpus <- corpus(text_dfn$text, 
                   docvars = data.frame(date = as.Date(text_dfn$date, "%m-%d-%y")))

# Building dfm 
myDfm <- dfm(myCorpus, remove = stops_dfn, groups = "date")

# Clearing Global environment
rm(myCorpus, stops_dfn,text_dfn)

# Sorting dfm (not required
myDfm <- dfm_sort(myDfm, decreasing = TRUE, "features")

# Ranking the terms 
featureRanksByDate <- as.data.frame(t(apply(myDfm, 1, order, decreasing = TRUE)))

# Mapping the term names 
names(featureRanksByDate) <- featnames(myDfm)

# Removing myDfm 
rm(myDfm)

# Preparing plot: 

# Subsetting related terms
featureRanksByDate <- subset(featureRanksByDate, select = c(rockets, china, houston, hongkong))

# Adding date as column 
featureRanksByDate<- mutate(featureRanksByDate, date = rownames(featureRanksByDate))

# Pivoting
featureRanksByDate_long <- melt(featureRanksByDate, id="date")

# Renaming
featureRanksByDate_long = featureRanksByDate_long%>% 
  rename(term = variable)

# Plotting
featureRanksByDate_long_plot <- featureRanksByDate_long %>%
  ggplot(., aes(x=factor(date), y= value, color = term, group = term)) +
  geom_line() + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_minimal()+ 
  geom_point(aes(x=1,y=16),colour="lightblue", size=3) + 
  geom_point(aes(x=3,y=277),colour="lightgreen", size=3) + 
  geom_point(aes(x=10,y=1442),colour="red", size=3) + 
  scale_color_manual(values=c("#9999CC", "#CC6666","#4682b4", "#add8e6"))+
  labs(title = "Frequency by day",
       x = "Date",
       y = "Mentions")

# Save plot 
ggsave("featureRanksByDate_long_plot.png", featureRanksByDate_long_plot, bg = "transparent")

# Clear viz 
rm(featureRanksByDate_long_plot)

# Plot 2: Exploration over time 2 

# Calculating total field 
featureRanksByDate2<- mutate(featureRanksByDate, sum = (featureRanksByDate$rockets+featureRanksByDate$china+featureRanksByDate$hongkong+featureRanksByDate$houston))

# Pivoting 
featureRanksByDate_long2 <- melt(featureRanksByDate2, id="date")

# Renaming 
featureRanksByDate_long2 = featureRanksByDate_long2%>% 
  rename(term = variable)

# Plotting
featureRanksByDate_long2_plot <- featureRanksByDate_long2 %>%
  filter(term == 'sum')%>%
  ggplot(., aes(x=factor(date), y= value, color = term, group = term)) +
  geom_line() + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_minimal()+ 
  geom_point(aes(x=1,y=110),colour="lightblue", size=3) + 
  geom_point(aes(x=3,y=772),colour="lightgreen", size=3) + 
  geom_point(aes(x=10,y=1540),colour="red", size=3) + 
  scale_color_manual(values=c("#9999CC"))+
  labs(title = "Total incident related mentions by day",
       x = "Date",
       y = "Mentions")

# Save plot 
ggsave("featureRanksByDate_long2_plot.png", featureRanksByDate_long2_plot , bg = "transparent")

# Clear Environment
rm(list = ls())

# Plot 3: Top 10 terms over time 

# Reading pre-prepared file for dfn 
text_dfn <- read.csv("~/University/Master (MsBA)/Spring/Text Analysis/R - Files/hult_NLP_student/Private/Case I/data/data_oct_dfn.csv")

text_dfn <- data.frame(text_dfn)

# Fixing date format -> required for dfn to work 
text_dfn$date <- parse_date_time(text_dfn$date, orders = "ymd")
text_dfn$date <- as.Date(text_dfn$date, format = "%m-%d-%y", origin = "ymd")

# Dropping x column 
text_dfn <- subset(text_dfn, select = -c(X))

# Building corpus for  analysis 2 (needed due to different date)
text_dfn_filtered <- text_dfn%>% 
  filter(text_dfn$date < as.Date("2019-10-24")) 

# Clearing environment
rm(text_dn)

# Stopwords for dfn creation 
stops_dfn <- c(stopwords('english'), 'amp', 'b', 'tb', 'itb', 'said', 'new', 'nba','game','preseason','basketball','win','now','live','will','last','just', 'team','first','like','one', 'rt','vs','g','m','th')


myCorpus_filtered <- corpus(text_dfn_filtered$text, 
                            docvars = data.frame(date = as.Date(text_dfn_filtered$date, "%m-%d-%y")))

# Building dfm 
myDfm_filtered <- dfm(myCorpus_filtered, remove = stops_dfn, groups = "date")

# Clearing Global environment
rm(myCorpus_filtered, stops_dfn,text_dfn_filtered)

# Sorting dfm (not required
myDfm_filtered <- dfm_sort(myDfm_filtered, decreasing = TRUE, "features")


# Ranking the terms 
featureRanksByDate <- as.data.frame(t(apply(myDfm_filtered, 1, order, decreasing = TRUE)))

# Mapping the term names 
names(featureRanksByDate) <- featnames(myDfm_filtered)

# Top 10 terms per day 
n <- 10
as.data.frame(apply(featureRanksByDate, 1, function(x) {
  todaysTopFeatures <- names(featureRanksByDate)
  names(todaysTopFeatures) <- x
  todaysTopFeatures[as.character(1:n)]
}), row.names = 1:n)

#Credit to initial code idea to https://stackoverflow.com/questions/33272308/word-frequency-over-time-by-user-in-r

#Clear Environment
rm(list = ls())

##Share of voice analysis

# Read file 
text_oct <- read.csv("~/University/Master (MsBA)/Spring/Text Analysis/R - Files/hult_NLP_student/Private/Case I/data/data_oct_all.csv")

# Definition of terms 
hongkong <- c('hongkong|\\bhong\\b|\\bkong\\b')
china <- c('china')
team <- c('rockets|houston') 
gm <- c("daryl|morey")
keywordscrisis <- c('hongkong|china|rockets|houston|daryl|morey|\\bhong\\b|\\bkong\\b')

# Find occurance
amounthongkong <- grepl(hongkong, text_oct$text,ignore.case=TRUE)
amountchina <- grepl(china, text_oct$text,ignore.case=TRUE)
amountteam <- grepl(team, text_oct$text,ignore.case=TRUE)
amountgm <- grepl(gm, text_oct$text,ignore.case=TRUE)
amountkey <- grepl(keywordscrisis, text_oct$text,ignore.case=TRUE)

# Calculation of perc of total tweets 
per_hongkong = (sum(amounthongkong) / nrow(text_oct))*100
per_china = (sum(amountchina) / nrow(text_oct))*100
per_team = (sum(amountteam) / nrow(text_oct))*100
per_gm = (sum(amountgm) / nrow(text_oct))*100
per_total = (sum(amountkey) / nrow(text_oct))*100


# Create df from the data calculated 
df <- data.frame (term  = c("Hong Kong", "China", "Team", "General Manager","Sum"),
                  percentage = c(per_hongkong, per_china, per_team,per_gm,per_total)
)

# Plot bar
shareofvoice <- ggplot(data = df, aes(x=reorder(term, -percentage), y=percentage))+
  geom_bar(stat = "identity",fill="steelblue")+ 
  theme_minimal()+ 
  labs(title = "Share of Voice: Percentage of total Tweets",
       x = "Term",
       y = "Percentage of all Tweets")

# Save plot 
ggsave("shareofvoice.png", shareofvoice, bg = "transparent")

# Clear Environment
rm(list = ls())

##Association Analysis 

# Setup 

# Reading pre-prepared file 

text_oct <- read.csv("~/University/Master (MsBA)/Spring/Text Analysis/R - Files/hult_NLP_student/Private/Case I/data/data_oct_all.csv")

text_oct <- as.data.frame(text_oct)

text_oct <- mutate(text_oct, doc_id = rownames(text_oct))

text_oct <- text_oct[ , c("doc_id", "text")]    

# Creating corpus 
txtCorpus_s <- VCorpus(VectorSource(text_oct$text))

# Removing the text_oct from memory to have more ram space 
rm(text_oct)

# Creating DTM/TDM of sample
txtTdm_s  <- TermDocumentMatrix(txtCorpus_s)

# Removing corpus 
rm(txtCorpus_s) 

# Plot 1

# Renaming
tdm <- txtTdm_s

# Terms of interest 
toi1 <- "china" 
toi2 <- "rockets"
toi3 <- "hongkong"
toi4 <- "houston"

# Lower correlation limit 
corlimit <- 0.25

# Computing association and storing 
corr1 <-  findAssocs(tdm, toi1, corlimit)[[1]]
corr1 <- cbind(read.table(text = names(corr1), stringsAsFactors = FALSE), corr1)
corr2 <- findAssocs(tdm, toi2, corlimit)[[1]]
corr2 <- cbind(read.table(text = names(corr2), stringsAsFactors = FALSE), corr2)
corr3 <- findAssocs(tdm, toi3, corlimit)[[1]]
corr3 <- cbind(read.table(text = names(corr3), stringsAsFactors = FALSE), corr3)
corr4 <- findAssocs(tdm, toi4, corlimit)[[1]]
corr4 <- cbind(read.table(text = names(corr4), stringsAsFactors = FALSE), corr4)

# Merging stored results 
two_terms_corrs_1 <- full_join(corr1, corr2)
two_terms_corrs_2 <- full_join(corr3, corr4)
two_terms_corrs <- full_join(two_terms_corrs_1,two_terms_corrs_2)

# Clearing select elements in environment 
x<- which(ls()=="two_terms_corrs"|ls()=="toi1"|ls()=="toi2"|ls()=="toi3"|ls()=="toi4")
ls1<- ls()[-x]
rm(list = ls1)

# Gathering for plot
two_terms_corrs_gathered <- gather(two_terms_corrs, term, correlation, corr1:corr4)

# Cleaning Environment 
x<- which(ls()=="two_terms_corrs_gathered"|ls()=="toi1"|ls()=="toi2"|ls()=="toi3"|ls()=="toi4")
ls1<- ls()[-x]
rm(list = ls1)

# Renaming corr back with terms 
two_terms_corrs_gathered$term <- ifelse(two_terms_corrs_gathered$term  == "corr1", toi1, two_terms_corrs_gathered$term)
two_terms_corrs_gathered$term <- ifelse(two_terms_corrs_gathered$term  == "corr2", toi2, two_terms_corrs_gathered$term)
two_terms_corrs_gathered$term <- ifelse(two_terms_corrs_gathered$term  == "corr3", toi3, two_terms_corrs_gathered$term)
two_terms_corrs_gathered$term <- ifelse(two_terms_corrs_gathered$term  == "corr4", toi4, two_terms_corrs_gathered$term)

# Removing outliers 
two_terms_corrs_gathered <- two_terms_corrs_gathered[-c(28, 88), ]


# Plotting 
associationplot <- ggplot(two_terms_corrs_gathered, aes(x = V1, y = correlation, colour =  term ) ) +
  geom_point(size = 3) +
  ylab(paste0("Correlation with the terms ", "\"", toi1,  "\"", " and ",  "\"", toi2, "\"")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# Save plot 
ggsave("associationplot.png", associationplot, bg = "transparent")

#Credit to initial code to https://stackoverflow.com/questions/12328056/how-do-i-delete-rows-in-a-data-frame

#Clear Environment
rm(list = ls())

# Exploring link to Nike 

# Reading pre-prepared file 

text_oct <- read.csv("~/University/Master (MsBA)/Spring/Text Analysis/R - Files/hult_NLP_student/Private/Case I/data/data_oct_all.csv")

text_oct <- as.data.frame(text_oct)

text_oct <- mutate(text_oct, doc_id = rownames(text_oct))

text_oct <- text_oct[ , c("doc_id", "text")]    

# Creating corpus 
txtCorpus_s <- VCorpus(VectorSource(text_oct$text))

# Removing the text_oct from memory to have more ram space 
rm(text_oct)

# Creating DTM/TDM of sample
txtTdm_s  <- TermDocumentMatrix(txtCorpus_s)

# Removing corpus 
rm(txtCorpus_s) 

# Find associations
associations <- findAssocs(txtTdm_s, 'nike', 0.10)

# Organize the word associations
assocDF <- data.frame(terms=names(associations[[1]]),
                      value=unlist(associations))

assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
rownames(assocDF) <- NULL

# Selecting interesting terms through filter 
assocDF2 <- assocDF %>%
  filter(terms %in% c("conscience","removing","accused","woke","kaepernick","spokesman","chinese","houston","rockets","condemnation","china","maoist","idiot")) 

# Plotting 
associations_nike <- assocDF2 %>%
  ggplot(., aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF, col='steelblue') +
  theme_minimal()+
  geom_text(aes(x=value,label=value), colour="steelblue",hjust="inward", vjust ="inward" , size=3)+
  labs(title = "Associations to Nike") 

# Save plot 
ggsave("vizassociations_nike.png", associations_nike, bg = "transparent")





