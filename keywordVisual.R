library(RSQLite)
library(tm)
library(readr)
library(wordcloud)
library(grid)
library(png)

# Get the data
#con0 <- dbConnect(RSQLite::SQLite(), dbname="C:\\Users\\Shagun\\Desktop\\Applied DataScience\\french-presidential-election\\database_11_1.sqlite")
#dbListTables(con0)
#tweets0 <- dbGetQuery( con0,'select * from data' )
#con1 <- dbConnect(RSQLite::SQLite(), dbname="C:\\Users\\Shagun\\Desktop\\Applied DataScience\\french-presidential-election\\database_12_0.sqlite")
#dbListTables(con1)
#tweets1 <- dbGetQuery( con1,'select * from data' )
#con2 <- dbConnect(RSQLite::SQLite(), dbname="C:\\Users\\Shagun\\Desktop\\Applied DataScience\\french-presidential-election\\database_12_1.sqlite")
#dbListTables(con2)
#tweets2 <- dbGetQuery( con2,'select * from data' )
#con3 <- dbConnect(RSQLite::SQLite(), dbname="C:\\Users\\Shagun\\Desktop\\Applied DataScience\\french-presidential-election\\database_13_0.sqlite")
#dbListTables(con3)
#tweets3 <- dbGetQuery( con1,'select * from data' )
#con4 <- dbConnect(RSQLite::SQLite(), dbname="C:\\Users\\Shagun\\Desktop\\Applied DataScience\\french-presidential-election\\database_13_1.sqlite")
#dbListTables(con4)
#tweets4 <- dbGetQuery( con0,'select * from data' )
#con5 <- dbConnect(RSQLite::SQLite(), dbname="C:\\Users\\Shagun\\Desktop\\Applied DataScience\\french-presidential-election\\database_14_0.sqlite")
#dbListTables(con5)
#tweets5 <- dbGetQuery( con1,'select * from data' )
con6 <- dbConnect(RSQLite::SQLite(), dbname="C:\\Users\\Shagun\\Desktop\\Applied DataScience\\french-presidential-election\\database_15_0.sqlite")
dbListTables(con6)
tweets6 <- dbGetQuery( con6,'select * from data' )
con7 <- dbConnect(RSQLite::SQLite(), dbname="C:\\Users\\Shagun\\Desktop\\Applied DataScience\\french-presidential-election\\database_15_1.sqlite")
dbListTables(con7)
tweets7 <- dbGetQuery( con7,'select * from data' )
con8 <- dbConnect(RSQLite::SQLite(), dbname="C:\\Users\\Shagun\\Desktop\\Applied DataScience\\french-presidential-election\\database_16_0.sqlite")
dbListTables(con8)
tweets8 <- dbGetQuery( con8,'select * from data' )
con9 <- dbConnect(RSQLite::SQLite(), dbname="C:\\Users\\Shagun\\Desktop\\Applied DataScience\\french-presidential-election\\database_16_1.sqlite")
dbListTables(con9)
tweets9 <- dbGetQuery( con9,'select * from data' )
#tweets = rbind(tweets0,tweets1,tweets2,tweets3,tweets4,tweets5,tweets6,tweets7,tweets8,tweets9)
tweets = rbind(tweets6,tweets7,tweets8,tweets9)
nrow(tweets) 


# Text analysis
tryToLower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

textdata <- tweets$text
textdata <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", textdata) # remove retweet entities
textdata <- gsub("@\\w+", "", textdata) # remove at people
textdata <- gsub("[[:punct:]]", "", textdata) # remove punctuation
textdata <- gsub("[[:digit:]]", "", textdata) # remove numbers
textdata <- gsub("http\\w+", "", textdata) # remove http links
textdata <- gsub("[\t]", "", textdata) # remove tab spaces
textdata <- gsub("[ ]{2,}", " ", textdata) # remove mulitple spaces
textdata <- gsub("^\\s+|\\s+$", "", textdata) # remove unnecessary spaces
textdata <- sapply(textdata, tryToLower)

# textdata <- textdata[!is.na(textdata)]
names(textdata) <- NULL

# Removing stop words (including the english one as our dataset also includes english tweets)
textdata <- removeWords(textdata, c(stopwords("french"), stopwords("english"), "cest", "quand", "jamais", "plus", "pourquoi", "jai", "quil", "tout", "fait"))

corpus <- Corpus(VectorSource(textdata))
dtm <- DocumentTermMatrix(corpus)
dtms <- removeSparseTerms(dtm, 0.995) # Removeterms with sparsity >= 95%
sparseData <- as.data.frame(as.matrix(dtms))
colnames(dtms) <- make.names(colnames(dtms))
# Most common word in all tweets
names(sparseData)[which.max(colSums(sparseData))]

# General wordcloud over all tweets
wordcloud(colnames(sparseData), 
          colSums(sparseData), 
          scale=c(4, 0.5),
          random.color=TRUE, 
          colors = c("#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B"),
          rot.per = 0.5) # rot.per = percentage of words written at a 90 degree angle

# Make wordcloud for each candidate, remove the candidates names so that the wordcloud is more balanced
candidates_names <- c("françois", "fillon", "emmanuel", "macron", "benoît", "hamon", "marine", "pen", "jeanluc", "mélenchon")
fillon <- subset(sparseData, tweets$mention_Fillon == 1)
wordcloud(colnames(fillon[,-which(names(fillon) %in% candidates_names)]), 
          colSums(fillon[,-which(names(fillon) %in% candidates_names)]), 
          scale=c(3, 0.5),
          random.color=TRUE, 
          colors = c("#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B"),
          rot.per = 0.5) 

macron <- subset(sparseData, tweets$mention_Macron == 1)
wordcloud(colnames(macron[,-which(names(macron) %in% candidates_names)]), 
          colSums(macron[,-which(names(macron) %in% candidates_names)]),MACRON 
          scale=c(3, 0.5),
          random.color=FALSE, 
          colors = c("#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B"),
          rot.per = 0.5) 

hamon <- subset(sparseData, tweets$mention_Hamon == 1)
wordcloud(colnames(hamon[,-which(names(hamon) %in% candidates_names)]), 
          colSums(hamon[,-which(names(hamon) %in% candidates_names)]), 
          scale=c(3, 0.5),
          random.color=FALSE, 
          colors = c("#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B"),
          rot.per = 0.5) 

lepen <- subset(sparseData, tweets$`mention_Le Pen` == 1)
wordcloud(colnames(lepen[,-which(names(lepen) %in% candidates_names)]), 
          colSums(lepen[,-which(names(lepen) %in% candidates_names)]), 
          scale=c(3, 0.5),
          random.color=FALSE, 
          colors = c("#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B"),
          rot.per = 0.5)

melenchon <- subset(sparseData, tweets$mention_Mélenchon == 1)
wordcloud(colnames(melenchon[,-which(names(melenchon) %in% candidates_names)]), 
          colSums(melenchon[,-which(names(melenchon) %in% candidates_names)]), 
          scale=c(3, 0.5),
          random.color=FALSE, 
          colors = c("#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B"),
          rot.per = 0.5) 