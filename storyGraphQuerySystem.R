#Creating Story Graph from knoweldge base and querying it
setwd("/home/jhavarharshita/PycharmProjects/MxP")
library(igraph)
library(stringr)
library(dplyr)
library(tidytext)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
kb = read.csv("storygraph.csv")  # read csv file

#Read the characters
char1 <- readline(prompt="Enter the first character name: ")
char2 <-  readline(prompt="Enter the second character name: ")

#Cleaning Actions and Objects column
kb$Objects.and.Actions <- lapply(kb$Objects.and.Actions , function(x) (strsplit(toString(x[1]),"\\[|,|\\(|\\]|\\)|'")))
kb$Objects.and.Actions <- lapply(kb$Objects.and.Actions , function(x) x[[1]][x[[1]]!=""])
kb$Objects.and.Actions <- lapply(kb$Objects.and.Actions , function(x) x[x!=" "])

#Selecting Rows which have Character 1 and Character 2 both
temp <- subset(kb, Character_1 == char1)
temp2 <- temp %>% filter(str_detect(Objects.and.Actions, char2) | str_detect(Character_2, char2))

#Plotting the evolution of emotions of Character 1 due to effect of Character2compound_score == 0
compound_score = as.double(temp2$compound_score)
#Removing the neutral part with assumption that history is followed
emotions = temp2$sentence.Emotion
while(length(which(compound_score == 0))>0){
  indexNeutral = which(compound_score == 0)
  compound_score[indexNeutral] = compound_score[indexNeutral-1]
  emotions[indexNeutral] = emotions[indexNeutral-1]}

plot(compound_score, type="l",
     main= c("Effect on sentiment score of",char1,"due to effect of",char2),
     xlab = "As story progresses",ylab = "Change of emotions",
     col= "blue",pch = 19, cex = 1, lty = "solid", lwd = 2)

text(compound_score, labels=emotions, cex= 0.7,pos = 4)

#Word Cloud of Two Characters
#Cleaning the text
temp3 <- Corpus(VectorSource(temp2$Sentence))
inspect(temp3)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
temp3 <- tm_map(temp3, toSpace, "/")
temp3 <- tm_map(temp3, toSpace, "@")
temp3 <- tm_map(temp3, toSpace, "\\|")
docs <- temp3
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
#docs <- tm_map(docs, stemDocument)
#Generate the word clowd
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Sentiment assignment to two characters

temp4 <- temp2$actionsAssociatedWithCharacter1
temp4 <- lapply(temp4 , function(x) (strsplit(toString(x),"\\[|,|\\(|\\]|\\)|'")))
temp4 <- lapply(temp4 , function(x) x[[1]][x[[1]]!=""])
temp4 <- lapply(temp4 , function(x) x[x!=" "])
y <- wordStem(temp4)
temp4 <- y
emotionNames <- list()
for(i in 1:(length(temp4))){
  if(length(temp4[i])>=1)
  emotionIndex = grep(temp4[i],sentiments$word)
  if(length(emotionIndex)>=1)
    emotionNames[i] <- sentiments$sentiment[emotionIndex]
  else if(length(emotionIndex)==0)
    emotionNames[i] <- "dictionary not enough"
  else
    emotionNames[i] <- "unknown emotion"
  }

#print(emotionNames)
#print(temp4)

plot(compound_score, type="l",
     main= c("Effect on emotions of",char1,"due to effect of",char2),
     xlab = "As story progresses",ylab = "Change of emotions",
     col= "blue",pch = 19, cex = 1, lty = "solid", lwd = 2)

text(compound_score, labels=emotionNames, cex= 0.7,pos =1)

#Average of the compound score
relation_score = mean(compound_score)
if(relation_score > 0){
  relation = "Friends"
} else if(relation_score < 0){
  relation = "Enemies"
} else {
  relation = "No interesting relation"
}
print(relation)

#Word cloud for the entire story

#Word Cloud of Two Characters
#Cleaning the text
temp5 <- Corpus(VectorSource(kb$Sentence))
inspect(temp5)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
temp5 <- tm_map(temp5, toSpace, "/")
temp5 <- tm_map(temp5, toSpace, "@")
temp5 <- tm_map(temp5, toSpace, "\\|")
docs <- temp5
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
#docs <- tm_map(docs, stemDocument)
#Generate the word clowd
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
#Story graph generation
temp6 <- kb
temp6 <- temp6[ which( ! temp6$sentence.Emotion %in% "neutral") , ]
storygraph <- data.frame(from=toString(temp6$Character_2),
                        to=toString(temp6$Character_1))
g <- graph_from_data_frame(storygraph, directed=TRUE, vertices=unique(c(toString(temp6$Character_1),toString(temp6$Character_2))))
plot(g, e=TRUE, v=TRUE)

#same.dept=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
#friendship=c(4,5,5,2,1,1), advice=c(4,5,5,4,2,3)









