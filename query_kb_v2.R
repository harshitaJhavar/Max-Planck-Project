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
kb = read.csv("storygraph_v2.csv")  # read csv file

#Preprocessing
#Cleaning Actions and Objects column
preProcess <- function(data){
  data <-  gsub('\\[', '', data)
  data <-  gsub('\\]', '', data)
  data <-  gsub("\\'", '', data)
  data <-  gsub("\\,", '', data)
  data <-  gsub("\\- |\\-| ", ' ', data)
  data <- sub("^$", "NA", data)
  return(data)
}

kb$Subject.Characters <- preProcess(kb$Subject.Characters)
kb$Actions.with.Subject.Charcater <- preProcess(kb$Actions.with.Subject.Charcater)
kb$Object.Characters <- preProcess(kb$Object.Characters)
kb$Objects.and.Actions <- preProcess(kb$Objects.and.Actions)

#Defining Event
event = list()
event_sentences = list()

j=0 #Event Index
for(i in 1:length(kb$Sentence)){
  if(length(event) == 0){
    event[[1]] <- 1
    print("Event Initialised")
    j=1 #j is the event id
  }
  else{
    #Search in previous event
    length_Last_Event = length(event[[j]])
    #Storing the sentence id from the last event
    temp <- event[[j]]
    #print(c("temp =", temp))
    
    previous_event_characters = list()
   for(k in 1:length_Last_Event){
    previous_event_characters <- 
      unique(
        c(previous_event_characters,
            strsplit(
                (kb$Subject.Characters[temp[[k]]])," ")
                  [[1:length(strsplit((kb$Subject.Characters[temp[[k]]])," ")[1])]],
                                   strsplit(
                                     (kb$Object.Characters[temp[[k]]])," ")
                                        [[1:length(strsplit((kb$Object.Characters[temp[[k]]])," ")[1])]]))
   }
    current_sentence_characters <- unique(c(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]],
                                            strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]]))
    #print(c("previous_event_characters = ",previous_event_characters))
    #print(c("current_sentence_characters = ",current_sentence_characters))
    #Checking if there is any common character between the two events
      if(length(setdiff(current_sentence_characters,previous_event_characters))<(length(current_sentence_characters))){
        #Checking for only match being NA
        if(length(setdiff(current_sentence_characters,previous_event_characters))==(length(current_sentence_characters))-1 
            & ('NA' %in% previous_event_characters) & ('NA' %in% current_sentence_characters)){
          #A new event defined
          j = j+1 #Increasing the event id
          event[[j]] <- i
          print("New event")
        }
        else{
        #The sentence belongs to the last event
            event[[j]] <- c(event[[j]],i)
            print("Same Event")
                }}
      else{
        #A new event defined
        j = j+1 #Increasing the event id
        event[[j]] <- i
        print("New event")
    }
   }
 
print(" ")  
}
print(c("There are total ", j, "events identified."))

#Write these event sentences in a csv

paragraph = 1:length(event)
export = data.frame(paragraph)
for(i in 1:length(event)){
  for(j in 1:length(event[[i]])){
  export$paragraph[i] = paste(export$paragraph[i],kb$Sentence[event[[i]][j]], sep = " ")
  }  
}
write.csv2(export$paragraph, file= "event_distribution.csv", row.names = FALSE)

#Emotion Labelling per event based on the sentences per event





























