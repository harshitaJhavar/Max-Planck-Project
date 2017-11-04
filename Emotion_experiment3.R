#Emotion ~ nGrams

#Train Data

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

#Removing the entries which do not have any summaries in their entry
trainData<-trainData[!(trainData$Summary==""),]


#Creating the model
#Computing ngrams of the summary 

#For Train Data
septagramsTrainList  = list()
hexagramsTrainList  = list()
pentagramsTrainList  = list()
quadragramsTrainList  = list()
trigramsTrainList  = list()
bigramsTrainList  = list()
emotionList = list()
j=0
for (i in 1:254){
  if(nchar(trainData$Summary[i])>0){
    j=j+1
    emotionList[j] <- trainData$Emotion[i]
    
    #septagram
    train_septagram_df = data.frame(make.ngrams((txt.to.words(trainData$Summary[i])), ngram.size = 7))
    septagramsTrainList[j] <- train_septagram_df
    
    #hexagram
    train_hexagram_df = data.frame(make.ngrams((txt.to.words(trainData$Summary[i])), ngram.size = 6))
    hexagramsTrainList[j] <- train_hexagram_df
    
    #pentagram
    train_pentagram_df = data.frame(make.ngrams((txt.to.words(trainData$Summary[i])), ngram.size = 5))
    pentagramsTrainList[j] <- train_pentagram_df
    
    #quadragram
    train_quadragram_df = data.frame(make.ngrams((txt.to.words(trainData$Summary[i])), ngram.size = 4))
    quadragramsTrainList[j] <- train_quadragram_df
    
    #trigram
    train_trigram_df = data.frame(make.ngrams((txt.to.words(trainData$Summary[i])), ngram.size = 3))
    trigramsTrainList[j] <- train_trigram_df
    
    #bigram
    train_bigram_df = data.frame(make.ngrams((txt.to.words(trainData$Summary[i])), ngram.size = 2))
    bigramsTrainList[j] <- train_bigram_df
  }
}
#Binding all the n-grams together
bigramsTrainFeature = do.call(rbind, bigramsTrainList)
trigramsTrainFeature = do.call(rbind, trigramsTrainList)
quadragramsTrainFeature = do.call(rbind, quadragramsTrainList)
pentagramsTrainFeature = do.call(rbind, pentagramsTrainList)
hexagramsTrainFeature = do.call(rbind, pentagramsTrainList)
septagramsTrainFeature = do.call(rbind, septagramsTrainList)

#Binding all outcome variable - emotions together
emotionOutcomeList = do.call(rbind, emotionList) 

#Creating the final train table
trainTable <- table(emotionOutcomeList)
trainTable$septagram <- septagramsTrainFeature
trainTable$quadragram <- quadragramsTrainFeature
trainTable$hexagram <- quadragramsTrainFeature
trainTable$pentagram <- pentagramsTrainFeature
trainTable$bigram <- bigramsTrainFeature
trainTable$trigram <- trigramsTrainFeature

#Reading the emotion lexicon list

#Incorporating the emotion lexicon list into the train table

#Model Building
trainedModelOnNGramsOfSummary <- multinom(emotionOutcomeList ~ trainTable$nanogram + trainTable$septagram + trainTable$pentagram + trainTable$trigram, MaxNWts = 23000)
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

#Removing the entries which do not have any summaries in their entry
testData<-testData[!(testData$Summary==""),]

#Computing ngrams of the summary 

#For Test Data

septagramsTestList  = list()
hexagramsTestList  = list()
pentagramsTestList  = list()
quadragramsTestList  = list()
bigramsTestList  = list()
trigramsTestList  = list()
emotionTestList = list()
j=0
for (i in 1:115){
  if(nchar(testData$Summary[i])>0){
    j=j+1
    emotionTestList[j] <- testData$Emotion[i]

    #hexagram
    test_hexagram_df = data.frame(make.ngrams((txt.to.words(testData$Summary[i])), ngram.size = 6))
    hexagramsTestList[j] <- test_hexagram_df
    
    #septagram
    test_septagram_df = data.frame(make.ngrams((txt.to.words(testData$Summary[i])), ngram.size = 7))
    septagramsTestList[j] <- test_septagram_df
    
    #pentagram
    test_pentagram_df = data.frame(make.ngrams((txt.to.words(testData$Summary[i])), ngram.size = 5))
    pentagramsTestList[j] <- test_pentagram_df
    
    #quadragram
    test_quadragram_df = data.frame(make.ngrams((txt.to.words(testData$Summary[i])), ngram.size = 4))
    quadragramsTestList[j] <- test_quadragram_df
    
    #trigram
    test_trigram_df = data.frame(make.ngrams((txt.to.words(testData$Summary[i])), ngram.size = 3))
    trigramsTestList[j] <- test_trigram_df
    
    #bigram
    test_bigram_df = data.frame(make.ngrams((txt.to.words(testData$Summary[i])), ngram.size = 2))
    bigramsTestList[j] <- test_bigram_df
    
  }
}
#Binding all the n-grams together
trigramsTestFeature = do.call(rbind, trigramsTestList)
bigramsTestFeature = do.call(rbind, bigramsTestList)
quadragramsTestFeature = do.call(rbind, quadragramsTestList)
pentagramsTestFeature = do.call(rbind, pentagramsTestList)
septagramsTestFeature = do.call(rbind, septagramsTestList)
hexagramsTestFeature = do.call(rbind, hexagramsTestList)

#Binding all outcome variable - emotions together
emotionOutcomeTestList = do.call(rbind, emotionTestList) 

#Creating the final test table
testTable <- table(emotionOutcomeTestList)
testTable$septagram <- septagramsTestFeature
testTable$pentagram <- pentagramsTestFeature
testTable$trigram <- trigramsTestFeature
testTable$bigram <- bigramsTestFeature
testTable$quadragram <- quadragramsTestFeature
testTable$hexagram <- hexagramsTestFeature

#Reading the emotion lexicon list

#Incorporating the emotion lexicon list into the test table

#Predicting the accuracy of the system
predictedEmotion <- predict(trainedModelOnNGramsOfSummary, newdata = testTable)
# load Caret package for computing Confusion matrix to calculate accuracy
library(caret) 
u = union(predictedEmotion, emotionOutcomeTestList)
t = table(factor(predictedEmotion, u), factor(emotionOutcomeTestList, u))
confusionMatrix(t)
