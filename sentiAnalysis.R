# Installing packages for the project:
# install.packages("tidytext")
# install.packages(c("SnowballC","wordcloud","syuzhet","corpus"))
# install.packages("wordcloud2")
# install.packages(c("RColorBrewer","SentimentAnalysis","randomForest","sentimentr"))
# install.packages(c("lubridate","scales","reshape2"))
# Importing Libraries:
library("tidyverse")
library(tm)
library(tidytext)
library(SnowballC)
library(wordcloud)
library(syuzhet)
library(corpus)
library(wordcloud2)
library(RColorBrewer)
library(SentimentAnalysis)
library(randomForest)
library(sentimentr)
library(dplyr)
library(reshape)
library(reshape2)
library(lubridate)
library(scales)

# Importing Data Set:
sentimentData <- read.csv(file = "imdbDataset.csv", sep=",", header=TRUE)
View(sentimentData)


colnames(sentimentData)
str(sentimentData)

# Building Corpus:
corpus1 <- iconv(sentimentData$ï..review, to = 'utf-8')
corpus1 <- Corpus(VectorSource((corpus1)))
inspect(corpus1[1:5])

# Cleaning of Data:
# To convert the data into lower-case data format:
corpus1 <- tm_map(corpus1,tolower)
inspect(corpus1[1:5])

# To remove Numbers:
corpus1 <- tm_map(corpus1,removeNumbers)
inspect(corpus1[1:5])

# To remove HTML tags from the data:
removeURLTags <- function(s) {
  gsub("</?[^>]+>", "", s)
}

corpus1 <- tm_map(corpus1, removeURLTags)
inspect(corpus1[1:5])

# To remove Punctuation:
corpus1 <- tm_map(corpus1,removePunctuation)
inspect(corpus1[1:5])

# To remove English words that are so common that they don't add value:
cData <- tm_map(corpus1, removeWords, stopwords('english'))
inspect(cData[1:5])

# Cleaning further by combining frequencies of words having same meaning or are in plural form:
# cData <- tm_map(cData, gsub, pattern = 'characters', replacement = 'character')


# To remove unnecessary/extra white spaces:
cData <- tm_map(cData, stripWhitespace)
inspect(cData[1:5])

# Text stemming - which reduces words to their root form
# cData <- tm_map(cData, stemDocument)
# inspect(cData[1:5])

# Term Document Matrix:
# We all know that reviews are very unstructured data.
tdmcData <- TermDocumentMatrix(cData)
tdmcData

inspect(tdmcData[1:10, 1:20])

findAssocs(tdmcData, terms = c("good", "work", "health"), corlimit = 0.25)
findAssocs(tdmcData, terms = findFreqTerms(tdmcData, lowfreq = 50), corlimit = 0.25)

tdmcData <- as.matrix(tdmcData)



# Bar Plot:

w <- rowSums(tdmcData)
w <- subset(w, w>=50)
barplot(w,las = 2, col = rainbow(10))


# Word Cloud:

x <- data.frame(names(w),w)
colnames(x) <- c("word","freq")
wordcloud2(x, size = 0.7, shape = 'triangle', minSize = 1)

head(x)



# To get Sentiment Scores:
sentiData <- read.csv(file.choose(), header = T)
s <- iconv(sentiData$ï..review, to = 'utf-8')
snrc <- get_nrc_sentiment(s)


# Bar Plot to see the Sentiment Scores of our Data Set:
barplot(colSums(snrc), las = 2, col = rainbow(10), 
        ylab = 'Count', main = 'Sentiment Scores for Reviews')

get_nrc_Sentiment("")



'Petter Matteis Love Time Money visually stunning film watch Mr Mattei offers us vivid portrait human relations This movie seems telling us money power success people different situations encounter This variation Arthur Schnitzlers play theme director transfers action present time New York different characters meet connect Each one connected one way another next person one seems know previous point contact Stylishly film sophisticated luxurious look We taken see people live world live habitatThe thing one gets souls picture different stages loneliness one inhabits A big city exactly best place human relations find sincere fulfillment one discerns case people encounterThe acting good Mr Matteis direction Steve Buscemi Rosario Dawson Carol Kane Michael Imperioli Adrian Grenier rest talented cast make characters come aliveWe wish Mr Mattei good luck await anxiously next work' %>% extract_sentiment_terms()
'I thought wonderful way spend time hot summer weekend sitting air conditioned theater watching lighthearted comedy The plot simplistic dialogue witty characters likable even well bread suspected serial killer While may disappointed realize Match Point 2 Risk Addiction I thought proof Woody Allen still fully control style many us grown loveThis Id laughed one Woodys comedies years dare I say decade While Ive never impressed Scarlet Johanson managed tone sexy image jumped right average spirited young womanThis may crown jewel career wittier Devil Wears Prada interesting Superman great comedy go see friends' %>% extract_sentiment_terms()

'I thought wonderful way spend time hot summer weekend sitting air conditioned theater watching lighthearted comedy The plot simplistic dialogue witty characters likable even well bread suspected serial killer While may disappointed realize Match Point 2 Risk Addiction I thought proof Woody Allen still fully control style many us grown loveThis Id laughed one Woodys comedies years dare I say decade While Ive never impressed Scarlet Johanson managed tone sexy image jumped right average spirited young womanThis may crown jewel career wittier Devil Wears Prada interesting Superman great comedy go see friends' %>% 
  sentiment_by(by = NULL) %>%
  highlight()
