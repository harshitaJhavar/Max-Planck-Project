#Experiment2
#Model: Emotion (Positive, Neutral, Negative) ~ nGrams(summary) + nGrams(sentiments)
# install.packages("devtools")
# Installing the tool from github
# devtools::install_github("sfeuerriegel/SentimentAnalysis")
library(stylo)
library(nnet)
library(SentimentAnalysis)
#Dealing with Train Data
#Reading Train File
trainData <- read.csv("trainFile.csv")

#Converting factor to string character for all entries in train Data
trainData$Emotion <- as.character(trainData$Emotion)
trainData$Character1 <- as.character(trainData$Character1)
trainData$Character2 <- as.character(trainData$Character2)
trainData$Summary <- as.character(trainData$Summary)

#Replacing char1 value in summary with char1 and char2 value in summary with char2 to 
#create a template out of summary text.

#For Train Data
for (i in 1:446){
  #Using regular expression to find the first name or last name in the text.
  separateFirstMiddleLastNameOfCharacter1 <- strsplit(trainData$Character1[i],"\\s+|-|\\.")
  separateFirstMiddleLastNameOfCharacter2 <- strsplit(trainData$Character2[i],"\\s+|-|\\.")
  #Replacing any of first name or last name with char1 or char2 respectively to create a template
  trainData$Summary[i]=gsub(separateFirstMiddleLastNameOfCharacter1, "Character1", trainData$Summary[i])
  trainData$Summary[i]=gsub(separateFirstMiddleLastNameOfCharacter2, "Character2", trainData$Summary[i])
  trainData$Summary[i]=gsub("\n", " ", trainData$Summary[i])
}

#Retrieving only those sentences in which both Character1 and Character2 are coming.
#For Train Data
for (i in 1:446){
  tempTxt<- strsplit(trainData$Summary[i],"\\.|\\?")
  #Looking for only those sentences which have character 1 and character 2 in them.
  b <-grep("Character1.*Character2|Character2.*Character1", tempTxt[[1]], ignore.case=TRUE, value= TRUE)
  #Assigning those relevant sentences in train Data Summary section
  trainData$Summary[i]=paste(b, collapse='. ' )
}

#Creating the model
#Computing ngrams of the summary 

#For Train Data
unigramsTrainList = list()
nanogramsTrainList = list()
septagramsTrainList  = list()
octagramsTrainList  = list()
hexagramsTrainList  = list()
pentagramsTrainList  = list()
quadragramsTrainList  = list()
trigramsTrainList  = list()
bigramsTrainList  = list()
emotionList = list()
sentimentList = list()
sentimentUnigramTrainList = list()
sentimentNanogramTrainList = list()
sentimentOctagramTrainList = list()
sentimentSeptagramTrainList = list()
sentimentHexagramTrainList = list()
sentimentPentagramTrainList = list()
sentimentQuadragramTrainList = list()
sentimentTrigramTrainList = list()
sentimentBigramTrainList = list()

j=0
for (i in 1:446){
  if(nchar(trainData$Summary[i])>0){
    j=j+1
    emotionList[j] <- trainData$Emotion[i]
    #nanogram
    train_nanogram_df = data.frame(make.ngrams((txt.to.words(trainData$Summary[i])), ngram.size = 9))
    nanogramsTrainList[j] <- train_nanogram_df
    sentiment <- analyzeSentiment(train_nanogram_df)
    sentimentNanogramTrainList[j] <- c(train_nanogram_df,convertToDirection(sentiment$SentimentGI))
    
    #octagram
    train_octagram_df = data.frame(make.ngrams((txt.to.words(trainData$Summary[i])), ngram.size = 8))
    octagramsTrainList[j] <- train_octagram_df
    sentiment <- analyzeSentiment(train_octagram_df)
    sentimentOctagramTrainList[j] <- c(train_octagram_df,convertToDirection(sentiment$SentimentGI))
    
    #septagram
    train_septagram_df = data.frame(make.ngrams((txt.to.words(trainData$Summary[i])), ngram.size = 7))
    septagramsTrainList[j] <- train_septagram_df
    sentiment <- analyzeSentiment(train_septagram_df)
    sentimentSeptagramTrainList[j] <- c(train_septagram_df,convertToDirection(sentiment$SentimentGI))
    
    #hexagram
    train_hexagram_df = data.frame(make.ngrams((txt.to.words(trainData$Summary[i])), ngram.size = 6))
    hexagramsTrainList[j] <- train_hexagram_df
    sentiment <- analyzeSentiment(train_hexagram_df)
    sentimentHexagramTrainList[j] <- c(train_hexagram_df,convertToDirection(sentiment$SentimentGI))
    
    #pentagram
    train_pentagram_df = data.frame(make.ngrams((txt.to.words(trainData$Summary[i])), ngram.size = 5))
    pentagramsTrainList[j] <- train_pentagram_df
    sentiment <- analyzeSentiment(train_pentagram_df)
    sentimentPentagramTrainList[j] <- c(train_pentagram_df,convertToDirection(sentiment$SentimentGI))
    
    #quadragram
    train_quadragram_df = data.frame(make.ngrams((txt.to.words(trainData$Summary[i])), ngram.size = 4))
    quadragramsTrainList[j] <- train_quadragram_df
    sentiment <- analyzeSentiment(train_quadragram_df)
    sentimentQuadragramTrainList[j] <- c(train_quadragram_df,convertToDirection(sentiment$SentimentGI))
    
    #trigram
    train_trigram_df = data.frame(make.ngrams((txt.to.words(trainData$Summary[i])), ngram.size = 3))
    trigramsTrainList[j] <- train_trigram_df
    sentiment <- analyzeSentiment(train_trigram_df)
    sentimentTrigramTrainList[j] <- c(train_trigram_df,convertToDirection(sentiment$SentimentGI))
    
    #bigram
    train_bigram_df = data.frame(make.ngrams((txt.to.words(trainData$Summary[i])), ngram.size = 2))
    bigramsTrainList[j] <- train_bigram_df
    sentiment <- analyzeSentiment(train_bigram_df)
    sentimentBigramTrainList[j] <- c(train_bigram_df,convertToDirection(sentiment$SentimentGI))
    
    #unigram
    train_unigram_df = data.frame(make.ngrams((txt.to.words(trainData$Summary[i])), ngram.size = 1))
    unigramsTrainList[j] <- train_unigram_df
    sentiment <- analyzeSentiment(train_unigram_df)
    sentimentUnigramTrainList[j] <- c(train_unigram_df,convertToDirection(sentiment$SentimentGI))
    
  }
}
#Binding all the n-grams together
unigramsTrainFeature = do.call(rbind, unigramsTrainList)
bigramsTrainFeature = do.call(rbind, bigramsTrainList)
trigramsTrainFeature = do.call(rbind, trigramsTrainList)
quadragramsTrainFeature = do.call(rbind, quadragramsTrainList)
pentagramsTrainFeature = do.call(rbind, pentagramsTrainList)
hexagramsTrainFeature = do.call(rbind, pentagramsTrainList)
septagramsTrainFeature = do.call(rbind, septagramsTrainList)
octagramsTrainFeature = do.call(rbind, pentagramsTrainList)
nanogramsTrainFeature = do.call(rbind, nanogramsTrainList)
unigramsSentimentTrainFeature = do.call(rbind, sentimentUnigramTrainList)
nanogramsSentimentTrainFeature = do.call(rbind, sentimentNanogramTrainList)
octagramsSentimentTrainFeature = do.call(rbind, sentimentOctagramTrainList)
septagramsSentimentTrainFeature = do.call(rbind, sentimentSeptagramTrainList)
hexagramsSentimentTrainFeature = do.call(rbind, sentimentHexagramTrainList)
pentagramsSentimentTrainFeature = do.call(rbind, sentimentPentagramTrainList)
quadragramsSentimentTrainFeature = do.call(rbind, sentimentQuadragramTrainList)
trigramsSentimentTrainFeature = do.call(rbind, sentimentTrigramTrainList)
bigramsSentimentTrainFeature = do.call(rbind, sentimentBigramTrainList)
#Reading the emotion lexicon list
#Binding all outcome variable - emotions together
emotionOutcomeList = do.call(rbind, emotionList) 
j=0
# Adding Sentiment Analysis Feature
for (i in 1:446){
  if(nchar(trainData$Summary[i])>0){
    j=j+1
    # Extract dictionary-based sentiment according to the Harvard-IV dictionary
    sentiment <- analyzeSentiment(trainData$Summary[i])
    # View sentiment direction (i.e. positive, neutral and negative)
    sentimentList[j] <- convertToDirection(sentiment$SentimentGI)
  }
} 
#Binding all outcome variable - summary sentiments together
summarySentimentOutcomeList = do.call(rbind, sentimentList) 

#Creating the final train table
trainTable <- table(emotionOutcomeList)
trainTable$summarySentiment <- summarySentimentOutcomeList
trainTable$unigram <- unigramsTrainFeature
trainTable$bigram <- bigramsTrainFeature
trainTable$trigram <- trigramsTrainFeature
trainTable$quadragram <- quadragramsTrainFeature
trainTable$pentagram <- pentagramsTrainFeature
trainTable$hexagram <- quadragramsTrainFeature
trainTable$septagram <- septagramsTrainFeature
trainTable$octagram <- quadragramsTrainFeature
trainTable$nanogram <- nanogramsTrainFeature
trainTable$unigramSentiment <- unigramsSentimentTrainFeature
trainTable$bigramSentiment <- bigramsSentimentTrainFeature
trainTable$trigramSentiment <- trigramsSentimentTrainFeature
trainTable$quadragramSentiment <- quadragramsSentimentTrainFeature
trainTable$pentagramSentiment <- pentagramsSentimentTrainFeature
trainTable$hexagramSentiment <- hexagramsSentimentTrainFeature
trainTable$septagramSentiment <- septagramsSentimentTrainFeature
trainTable$octagramSentiment <- octagramsSentimentTrainFeature
trainTable$nanogramSentiment <- nanogramsSentimentTrainFeature
#Incorporating the emotion lexicon list into the train table

#Model Building
trainedModelOnNGramsOfSummary <- multinom(emotionOutcomeList ~ trainTable$trigram + trainTable$trigramSentiment, MaxNWts = 25000)
#Looking for model details
#summary(trainedModelOnNGramsOfSummary)

#For Test Data
#Reading Test File
testData <- read.csv("testfile.csv")

#Converting factor to string character for all entries in test Data
testData$Emotion <- as.character(testData$Emotion)
testData$Character1 <- as.character(testData$Character1)
testData$Character2 <- as.character(testData$Character2)
testData$Summary <- as.character(testData$Summary)

#For Test Data, creating a template.
for (i in 1:115){
  #Using regular expression to find the first name or last name in the text.
  separateFirstMiddleLastNameOfCharacter1 <- strsplit(testData$Character1[i],"\\s+|-|\\.")
  separateFirstMiddleLastNameOfCharacter2 <- strsplit(testData$Character2[i],"\\s+|-|\\.")
  #Replacing any of first name or last name with char1 or char2 respectively to create a template
  testData$Summary[i]=gsub(separateFirstMiddleLastNameOfCharacter1, "Character1", testData$Summary[i])
  testData$Summary[i]=gsub(separateFirstMiddleLastNameOfCharacter2, "Character2", testData$Summary[i])
  testData$Summary[i]=gsub("\n", " ", testData$Summary[i])
}

#Computing ngrams of the summary 

#For Test Data
nanogramsTestList = list()
unigramsTestList = list()
septagramsTestList  = list()
octagramsTestList  = list()
hexagramsTestList  = list()
pentagramsTestList  = list()
quadragramsTestList  = list()
bigramsTestList  = list()
trigramsTestList  = list()
emotionTestList = list()
sentimentTestList = list()
sentimentNanogramTestList = list()
sentimentUnigramTestList = list()
sentimentOctagramTestList = list()
sentimentSeptagramTestList = list()
sentimentHexagramTestList = list()
sentimentPentagramTestList = list()
sentimentQuadragramTestList = list()
sentimentTrigramTestList = list()
sentimentBigramTestList = list()

j=0
for (i in 1:115){
  if(nchar(testData$Summary[i])>0){
    j=j+1
    emotionTestList[j] <- testData$Emotion[i]
    #nanogram
    test_nanogram_df = data.frame(make.ngrams((txt.to.words(testData$Summary[i])), ngram.size = 9))
    nanogramsTestList[j] <- test_nanogram_df
    sentiment <- analyzeSentiment(test_nanogram_df)
    sentimentNanogramTestList[j] <- c(test_nanogram_df,convertToDirection(sentiment$SentimentGI))
    
    #octagram
    test_octagram_df = data.frame(make.ngrams((txt.to.words(testData$Summary[i])), ngram.size = 8))
    octagramsTestList[j] <- test_octagram_df
    sentiment <- analyzeSentiment(test_octagram_df)
    sentimentOctagramTestList[j] <- c(test_octagram_df,convertToDirection(sentiment$SentimentGI))
    
    #septagram
    test_septagram_df = data.frame(make.ngrams((txt.to.words(testData$Summary[i])), ngram.size = 7))
    septagramsTestList[j] <- test_septagram_df
    sentiment <- analyzeSentiment(test_septagram_df)
    sentimentSeptagramTestList[j] <- c(test_septagram_df,convertToDirection(sentiment$SentimentGI))
    
    #hexagram
    test_hexagram_df = data.frame(make.ngrams((txt.to.words(testData$Summary[i])), ngram.size = 6))
    hexagramsTestList[j] <- test_hexagram_df
    sentiment <- analyzeSentiment(test_hexagram_df)
    sentimentHexagramTestList[j] <- c(test_hexagram_df,convertToDirection(sentiment$SentimentGI))
    
    #pentagram
    test_pentagram_df = data.frame(make.ngrams((txt.to.words(testData$Summary[i])), ngram.size = 5))
    pentagramsTestList[j] <- test_pentagram_df
    sentiment <- analyzeSentiment(test_pentagram_df)
    sentimentPentagramTestList[j] <- c(test_pentagram_df,convertToDirection(sentiment$SentimentGI))
    
    #quadragram
    test_quadragram_df = data.frame(make.ngrams((txt.to.words(testData$Summary[i])), ngram.size = 4))
    quadragramsTestList[j] <- test_quadragram_df
    sentiment <- analyzeSentiment(test_quadragram_df)
    sentimentQuadragramTestList[j] <- c(test_quadragram_df,convertToDirection(sentiment$SentimentGI))
    
    #trigram
    test_trigram_df = data.frame(make.ngrams((txt.to.words(testData$Summary[i])), ngram.size = 3))
    trigramsTestList[j] <- test_trigram_df
    sentiment <- analyzeSentiment(test_trigram_df)
    sentimentTrigramTestList[j] <- c(test_trigram_df,convertToDirection(sentiment$SentimentGI))
    
    #bigram
    test_bigram_df = data.frame(make.ngrams((txt.to.words(testData$Summary[i])), ngram.size = 2))
    bigramsTestList[j] <- test_bigram_df
    sentiment <- analyzeSentiment(test_bigram_df)
    sentimentBigramTestList[j] <- c(test_bigram_df,convertToDirection(sentiment$SentimentGI))
    
    #unigram
    test_unigram_df = data.frame(make.ngrams((txt.to.words(testData$Summary[i])), ngram.size = 1))
    unigramsTestList[j] <- test_unigram_df
    sentiment <- analyzeSentiment(test_unigram_df)
    sentimentUnigramTestList[j] <- c(test_unigram_df,convertToDirection(sentiment$SentimentGI))
  }
}
j=0
# Adding Sentiment Analysis Feature
for (i in 1:115){
  if(nchar(testData$Summary[i])>0){
    j=j+1
    # Extract dictionary-based sentiment according to the Harvard-IV dictionary
    sentiment <- analyzeSentiment(testData$Summary[i])
    # View sentiment direction (i.e. positive, neutral and negative)
    sentimentTestList[j] <- convertToDirection(sentiment$SentimentGI)
  }
} 
#Binding all the n-grams together
unigramsTestFeature = do.call(rbind, unigramsTestList)
bigramsTestFeature = do.call(rbind, bigramsTestList)
trigramsTestFeature = do.call(rbind, trigramsTestList)
quadragramsTestFeature = do.call(rbind, quadragramsTestList)
pentagramsTestFeature = do.call(rbind, pentagramsTestList)
hexagramsTestFeature = do.call(rbind, hexagramsTestList)
septagramsTestFeature = do.call(rbind, septagramsTestList)
octagramsTestFeature = do.call(rbind, octagramsTestList)
nanogramsTestFeature = do.call(rbind, nanogramsTestList)
unigramsSentimentTestFeature = do.call(rbind, sentimentUnigramTestList)
nanogramsSentimentTestFeature = do.call(rbind, sentimentNanogramTestList)
octagramsSentimentTestFeature = do.call(rbind, sentimentOctagramTestList)
septagramsSentimentTestFeature = do.call(rbind, sentimentSeptagramTestList)
hexagramsSentimentTestFeature = do.call(rbind, sentimentHexagramTestList)
pentagramsSentimentTestFeature = do.call(rbind, sentimentPentagramTestList)
quadragramsSentimentTestFeature = do.call(rbind, sentimentQuadragramTestList)
trigramsSentimentTestFeature = do.call(rbind, sentimentTrigramTestList)
bigramsSentimentTestFeature = do.call(rbind, sentimentBigramTestList)

#Binding all outcome variable - emotions together
emotionOutcomeTestList = do.call(rbind, emotionTestList) 

#Binding all sentiments of summary together
sentimentOutcomeTestList = do.call(rbind, sentimentTestList)
#Creating the final test table
testTable <- table(emotionOutcomeTestList)
testTable$summarySentiment <- sentimentOutcomeTestList
testTable$unigram <- unigramsTestFeature
testTable$bigram <- bigramsTestFeature
testTable$trigram <- trigramsTestFeature
testTable$quadragram <- quadragramsTestFeature
testTable$pentagram <- pentagramsTestFeature
testTable$hexagram <- hexagramsTestFeature
testTable$septagram <- septagramsTestFeature
testTable$octagram <- octagramsTestFeature
testTable$nanogram <- nanogramsTestFeature
testTable$unigramSentiment <- unigramsSentimentTestFeature
testTable$bigramSentiment <- bigramsSentimentTestFeature
testTable$trigramSentiment <- trigramsSentimentTestFeature
testTable$quadragramSentiment <- quadragramsSentimentTestFeature
testTable$pentagramSentiment <- pentagramsSentimentTestFeature
testTable$hexagramSentiment <- hexagramsSentimentTestFeature
testTable$septagramSentiment <- septagramsSentimentTestFeature
testTable$octagramSentiment <- octagramsSentimentTestFeature
testTable$nanogramSentiment <- nanogramsSentimentTestFeature
#Reading the emotion lexicon list

#Incorporating the emotion lexicon list into the test table

#Predicting the accuracy of the system
predictedEmotion <- predict(trainedModelOnNGramsOfSummary, newdata = testTable)
# load Caret package for computing Confusion matrix to calculate accuracy
library(caret) 
u = union(predictedEmotion, emotionOutcomeTestList)
t = table(factor(predictedEmotion, u), factor(emotionOutcomeTestList, u))
confusionMatrix(t)


