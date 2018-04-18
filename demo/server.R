library(shiny)
library(ggplot2)
library(reshape2)
library(lubridate)
library(Cairo)
library(rAmCharts)
#For autocomplete

function(input, output) {
  ranges1 <- reactiveValues(x = NULL, y = NULL)
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  ranges3 <- reactiveValues(x = NULL, y = NULL)
  ranges4 <- reactiveValues(x = NULL, y = NULL)
  
  
  book_choice <- c('Harry Potter and the Philosopher\'s Stone',
                   'Harry Potter and the Chamber of Secrets',
                   'Harry Potter and the Prisoner of Azkaban',
                   'Harry Potter and the Goblet of Fire',
                   'Harry Potter and the Order of the Phoenix',
                   'Harry Potter and the Half-Blood Prince',
                   'Harry Potter and the Deathly Hallows: Part 1')
  book_kb_files <- c("storygraph_v1.csv","storygraph_v2.csv","storygraph_v3.csv","storygraph_v4.csv",
                     "storygraph_v5.csv","storygraph_v6.csv","storygraph_v7.csv")
  book_export_files <- c("event_distribution1.csv","event_distribution2.csv","event_distribution3.csv",
                         "event_distribution4.csv","event_distribution5.csv","event_distribution6.csv",
                         "event_distribution7.csv")
  
  df_book <- data.frame(book_choice,book_kb_files,book_export_files)
  
  
  generate_plot1 <- eventReactive(input$goButton0,{
    
    #################################
    if(input$x =='Harry Potter and the Philosopher\'s Stone'){
      kb = read.csv("storygraph_v1.csv")
      export = as.data.frame(read.csv("event_distribution1.csv"))
    }
    
    else if(input$x=='Harry Potter and the Chamber of Secrets'){
      kb = read.csv("storygraph_v2.csv")
      export = as.data.frame(read.csv("event_distribution2.csv"))
    }
    else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
      kb = read.csv("storygraph_v3.csv")
      export = as.data.frame(read.csv("event_distribution3.csv"))
    }
    else if(input$x=='Harry Potter and the Goblet of Fire'){
      kb = read.csv("storygraph_v4.csv")
      export = as.data.frame(read.csv("event_distribution4.csv"))
    }
    else if(input$x=='Harry Potter and the Order of the Phoenix'){
      kb = read.csv("storygraph_v5.csv")
      export = as.data.frame(read.csv("event_distribution5.csv"))
    }
    else if(input$x=='Harry Potter and the Half-Blood Prince'){
      kb = read.csv("storygraph_v6.csv")
      export = as.data.frame(read.csv("event_distribution6.csv"))
    }
    else{
      kb = read.csv("storygraph_v7.csv")
      export = as.data.frame(read.csv("event_distribution7.csv"))
    }
    
    
    
    # kb <- renderText({ 
    #  read.csv(kb_data())
    #})
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
    
    #Removing those rows in which subject and object both are NA
    kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
    
    #Defining Event
    event = list()
    event_sentences = list()
    j=0 #Event Index
    for(i in 1:length(kb$Sentence)){
      #print(i)
      if(length(event) == 0){
        currentObject <- strsplit(
          (kb$Object.Characters[1])," ")
        # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
        
        currentSubject <-   strsplit(
          (kb$Subject.Characters[1])," ")
        event[1] <- 1
        print("Event Initialised")
        j=1 #j is the event id
        previous_event_characters <- c(currentSubject,currentObject)
        
      }
      else{
        
        #Search in previous event
        length_Last_Event = length(event[[j]])
        #Storing the sentence id from the last event
        temp <- event[[j]]
        #print(c("temp =", temp))
        
        
        # for(k in 1:length_Last_Event){
        pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
        #New Algo for Scene Segmentation
        currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
        # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
        
        currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
        #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
        print(i)
        print("Previous Event")
        print(previous_event_characters)
        print("Current Subject")
        print(currentSubject)
        print("Current Object")
        print(currentObject)
        #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
        if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
            (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
          #A new event defined
          j = j+1 #Increasing the event id
          event[[j]] <- i
          #print("LALA")
          print("New eventCC")
          previous_event_characters <- c(currentSubject,currentObject)
        }
        else{
          if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
            if(length(setdiff(currentObject,pronounList))==0){
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
            else{
              j = j+1 #Increasing the event id
              event[[j]] <- i
              #print("LALA")
              print("New eventCC")
              previous_event_characters <- c(currentSubject,currentObject)}
          }
          else{
            event[[j]] <- c(event[[j]],i)
            print("Same Event")
            previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
          }
        }
      } 
    }
    ###############################
    
    ################################
    
    #Pairwise Interaction Analysis
    
    #Replacing NaN value from export to previous value
    
    index_replace_export <- which(is.nan(export$anger))
    for(h in 1:length(index_replace_export)){
      export$anger[index_replace_export[h]] <- 0
      export$anticipation[index_replace_export[h]] <- 0
      export$disgust[index_replace_export[h]] <- 0
      export$fear[index_replace_export[h]] <- 0
      export$joy[index_replace_export[h]] <- 0
      export$sadness[index_replace_export[h]] <- 0
      export$surprise[index_replace_export[h]] <- 0
      export$trust[index_replace_export[h]] <- 0
    }
    
    do_pairwise_interaction_analysis <- function(export, char1, char2){
      anger = 1:length(export$sentence_id) #Will be addressed in next function
      anticipation=1:length(export$sentence_id)
      disgust=1:length(export$sentence_id)
      fear=1:length(export$sentence_id)
      joy=1:length(export$sentence_id)
      sadness=1:length(export$sentence_id)
      surprise=1:length(export$sentence_id)
      trust=1:length(export$sentence_id)
      paragraph = 1:length(export$sentence_id)
      nrc_sentiment = 1:length(export$sentence_id)
      no_of_interactions = 1:length(export$sentence_id)
      Valence =  1:length(export$sentence_id)
      Arousal =  1:length(export$sentence_id)
      Dominance =  1:length(export$sentence_id)
      export_pair_emotions = data.frame(paragraph,no_of_interactions,nrc_sentiment,anger,anticipation,disgust,fear,joy,sadness,surprise,trust, Valence, Arousal, Dominance)
      for(event_index in 1:length(export$sentence_id)){
        
        #Selecting all the available information from kb for the current event
        current_event_kb <- kb[event[[event_index]][1]:event[[event_index]][length(event[[event_index]])],c(1,2,3,4,5,7),drop=F]
        #Condition for direction from character 2 to character 1
        interaction_in_current_event <- subset(current_event_kb, grepl(char2, Subject.Characters) & grepl(char1, dependency_tree))
        
        #Mapping emotion of character 1 towards character 2
        export_pair_emotions$paragraph[event_index] <- ""
        if(nrow(interaction_in_current_event)==0){
          export_pair_emotions$paragraph[event_index] <- paste(export_pair_emotions$paragraph[event_index],"NA", split = "")
        }
        else if(nrow(interaction_in_current_event)>1){
          
          export_pair_emotions$paragraph[event_index] <- paste(interaction_in_current_event$Sentence[1]:interaction_in_current_event$Sentence[nrow(interaction_in_current_event)],split=" ")
        }
        else if(nrow(interaction_in_current_event)==1){
          export_pair_emotions$paragraph[event_index] <- paste(export_pair_emotions$paragraph[event_index],interaction_in_current_event$Sentence[1], split = "")
        }
        else
          export_pair_emotions$paragraph[event_index] <- paste(export_pair_emotions$paragraph[event_index],"NA", split = "")
      }
      #########################################################
      #Emotion Labelling per event based on the sentences per event for each character pair
      
      for(event_index in 1:length(event)){
        
        
        if(export_pair_emotions$paragraph[event_index] != " NA "){
          print(event_index)
          #export_pair_emotions$anger[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[1]/length(event[[event_index]])
          export_pair_emotions$anger[event_index] <- export$anger[event_index]
          #export_pair_emotions$anticipation[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[2]/length(event[[event_index]])
          export_pair_emotions$anticipation[event_index] <- export$anticipation[event_index]
          #export_pair_emotions$disgust[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[3]/length(event[[event_index]])
          export_pair_emotions$disgust[event_index] <- export$disgust[event_index]
          #export_pair_emotions$fear[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[4]/length(event[[event_index]])
          export_pair_emotions$fear[event_index] <- export$fear[event_index]
          #export_pair_emotions$joy[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[5]/length(event[[event_index]])
          export_pair_emotions$joy[event_index] <- export$joy[event_index]
          #export_pair_emotions$sadness[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[6]/length(event[[event_index]])
          export_pair_emotions$sadness[event_index] <- export$sadness[event_index]
          #export_pair_emotions$surprise[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[7]/length(event[[event_index]])
          export_pair_emotions$surprise[event_index] <- export$surprise[event_index]
          #export_pair_emotions$trust[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[8]/length(event[[event_index]])
          export_pair_emotions$trust[event_index] <- export$trust[event_index]
          
          #Calculating Sentiment Score(Average of all emotions)
          #export_pair_emotions$nrc_sentiment[event_index] = mean(c(as.numeric(export_pair_emotions[event_index,3:10])))
          # if(length(get_sentiment(get_sentences(export_pair_emotions$paragraph[event_index])))>1)
          #   export_pair_emotions$nrc_sentiment[event_index] = mean(get_sentiment(get_sentences(export_pair_emotions$paragraph[event_index])))
          #  else
          export_pair_emotions$nrc_sentiment[event_index] =  export$nrc_sentiment[event_index]
          export_pair_emotions$no_of_interactions[event_index] = export$no_of_interactions[event_index]
          #fileConn<-file("input.txt")
          #write(export$paragraph[event_index], fileConn)
          #system("java -jar /home/jhavarharshita/PycharmProjects/MxP/JEmAS-master/JEmAS-v0.2-beta.jar /home/jhavarharshita/PycharmProjects/MxP/input.txt > output.csv", intern=TRUE)
          #close(fileConn)
          #temp_kb <- read.csv("output.csv")
          #temp_kb <- read.csv("output.csv", sep = "\t")
          #export_pair_emotions$Valence[event_index] = temp_kb$Valence[1]/length(event[[event_index]])
          export_pair_emotions$Valence[event_index] = export$Valence[event_index]
          #export_pair_emotions$Arousal[event_index] = temp_kb$Arousal[1]/length(event[[event_index]])
          export_pair_emotions$Arousal[event_index] = export$Arousal[event_index]
          #export_pair_emotions$Dominance[event_index] = temp_kb$Dominance[1]/length(event[[event_index]])
          export_pair_emotions$Dominance[event_index] = export$Dominance[event_index]
        }  
        else{
          if(event_index!=1){
            export_pair_emotions$anger[event_index] <- export_pair_emotions$anger[event_index-1]
            export_pair_emotions$anticipation[event_index] <- export_pair_emotions$anticipation[event_index-1]
            export_pair_emotions$disgust[event_index] <- export_pair_emotions$disgust[event_index-1]
            export_pair_emotions$fear[event_index] <- export_pair_emotions$fear[event_index-1]
            export_pair_emotions$joy[event_index] <-export_pair_emotions$joy[event_index-1] 
            export_pair_emotions$sadness[event_index] <- export_pair_emotions$sadness[event_index-1]
            export_pair_emotions$surprise[event_index] <- export_pair_emotions$surprise[event_index-1]
            export_pair_emotions$trust[event_index] <-  export_pair_emotions$trust[event_index-1]
            export_pair_emotions$nrc_sentiment[event_index] <- export_pair_emotions$nrc_sentiment[event_index-1]
            export_pair_emotions$no_of_interactions[event_index] <- 0
            export_pair_emotions$Valence[event_index] <- export_pair_emotions$Valence[event_index-1]
            export_pair_emotions$Arousal[event_index] <- export_pair_emotions$Arousal[event_index-1]
            export_pair_emotions$Dominance[event_index] <- export_pair_emotions$Dominance[event_index-1]
          }
          else{
            export_pair_emotions$anger[event_index] <- 0
            export_pair_emotions$anticipation[event_index] <- 0
            export_pair_emotions$disgust[event_index] <- 0
            export_pair_emotions$fear[event_index] <- 0
            export_pair_emotions$joy[event_index] <-0
            export_pair_emotions$sadness[event_index] <- 0
            export_pair_emotions$surprise[event_index] <- 0
            export_pair_emotions$trust[event_index] <-  0
            export_pair_emotions$nrc_sentiment[event_index]<- 0
            export_pair_emotions$Valence[event_index]<- 0
            export_pair_emotions$Arousal[event_index]<- 0
            export_pair_emotions$Dominance[event_index]<- 0
          }
        }}
      return(export_pair_emotions)
    }
    
    #Read the characters
    #char1 <- readline(prompt="Enter the first character name: ")
    #char2 <-  readline(prompt="Enter the second character name: ")
    char1 <- 'Other'
    char2 <- 'Other'
    character1 <- 'Character 1'
    character2 <- 'Character 2'
    if(input$user_char1input!="Enter your choice for character 1"){
      if(input$user_char1input=='Harry Potter'){
        char1 <- as.character('Harry|HARRY|Potter')
        character1 <- input$user_char1input
      }
      else if(input$user_char1input=='Ronald Weasley'){
        char1 <- as.character('Ron|Ronald|RON|RONALD|Weasley')
        character1 <- input$user_char1input
      }
      else if(input$user_char1input=='Hermione Granger'){
        char1 <- as.character('Hermione|Granger|HERMIONE|GRANGER')
        character1 <- input$user_char1input
      }
      else if(input$user_char1input=='Lord Voldemort'){
        char1 <- as.character('Voldemort|Dark Lord|Lord|LORD|YOU-KNOW-WHO')
        character1 <- input$user_char1input
      }
      else if(input$user_char1input=='Draco Malfoy'){
        char1 <- as.character('Malfoy|Draco|DRACO|MALFOY')
        character1 <- input$user_char1input
      }
      else if(input$user_char1input=='Dursleys'){
        char1 <- as.character('Dursleys|Dudely|Petunia|Vernon|DURSLEYS|DUDELY|PETUNIA|VERNON')
        character1 <- input$user_char1input
      }}
   # else if(input$user_char1input!="Enter your choice for character 1"){
    #  char1 <- input$user_char1input
     # char1 <- as.character(char1)
      #character1 <- char1
    #}
    #char1 <- as.character(input$user_char1input)
    #char2 <- as.character(input$user_char2input)
    if(input$user_char2input!="Enter your choice for character 2"){
      if(input$user_char2input=='Harry Potter'){
        char2 <- as.character('Harry|HARRY|Potter')
        character2 <- input$user_char2input
      }
      else if(input$user_char2input=='Ronald Weasley'){
        char2 <- as.character('Ron|Ronald|RON|RONALD|Weasley')
        character2 <- input$user_char2input
      }
      else if(input$user_char2input=='Hermione Granger'){
        char2 <- as.character('Hermione|Granger|HERMIONE|GRANGER')
        character2 <- input$user_char2input
      }
      else if(input$user_char2input=='Lord Voldemort'){
        char2 <- as.character('Voldemort|Dark Lord|Lord|LORD|YOU-KNOW-WHO')
        character2 <- input$user_char2input
      }
      else if(input$user_char2input=='Draco Malfoy'){
        char2 <- as.character('Malfoy|Draco|DRACO|MALFOY')
        character2 <- input$user_char2input
      }
      else if(input$user_char2input=='Dursleys'){
        char2 <- as.character('Dursleys|Dudely|Petunia|Vernon|DURSLEYS|DUDELY|PETUNIA|VERNON')
        character2 <- input$user_char2input
      }}
   # else if(input$user_char2input!="Enter your choice for character 2"){
    #  char2 <- input$user_char2input
     # char2 <- as.character(char2)
    #  character2 <- char2
    #}
    
    #export_backup <- export
    kb_pairwise_emotions = do_pairwise_interaction_analysis(export, char1, char2)
    ###############################################################################
    #Valence Plotting
    #plot(kb_pairwise_emotions$nrc_sentiment,xlab = "Event Indexes", ylab = "Emotion Valence between the pair", main = c("Emotion valence progress of ", char1, "towards ", char2),type = "l")
    #Emotions Plotting along different events for the character pairs
    #Emotions Plotting along different events for the character pairs
    
    library(ggplot2)
    library(reshape2)
    df_categorical <- data.frame(Scenes = 1:length(event),
                                 anger = kb_pairwise_emotions$anger,
                                 anticipation = kb_pairwise_emotions$anticipation,
                                 disgust = kb_pairwise_emotions$disgust,
                                 fear = kb_pairwise_emotions$fear,
                                 joy = kb_pairwise_emotions$joy,
                                 sadness = kb_pairwise_emotions$sadness,
                                 surprise = kb_pairwise_emotions$surprise,
                                 trust = kb_pairwise_emotions$trust)
    
    df_dimensional <- data.frame(Scenes = 1:length(event),
                                 valence = kb_pairwise_emotions$Valence,
                                 arousal = kb_pairwise_emotions$Arousal,
                                 dominance = kb_pairwise_emotions$Dominance)
    
   df_categorical <- melt(df_categorical ,  id.vars = 'Scenes', variable.name = 'Categorical_Emotions')
    df_dimensional <- melt(df_dimensional ,  id.vars = 'Scenes', variable.name = 'Dimensional_Emotions')
    # plot on same grid, each series colored differently -- 
    #plot_c <- amStockMultiSet('Categorical_Emotions')
    #plot_c <- 
      ggplot(df_categorical, aes(Scenes,value))+ geom_line(aes(colour = Categorical_Emotions),size=1) + scale_x_continuous(breaks=seq(1, length(event), 1))+
      labs(title=paste("Categorical emotions trajectory for", character1, "towards",character2), x = "Progress of Story", y= "Categorical emotions in each scene")+coord_cartesian(xlim = ranges1$x, ylim = ranges1$y, expand = FALSE)+ theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    
    #require(gridExtra)
    #grid.arrange(plot_c,nrow=1)
    #height=700
      })
  
  
  output$`Emotion Mapping1` <- renderPlot({ 
      generate_plot1() 
  })
 
 ##################################################################################################################################
  generate_plot2 <- eventReactive(input$goButton0,{
    
    #################################
    if(input$x =='Harry Potter and the Philosopher\'s Stone'){
      kb = read.csv("storygraph_v1.csv")
      export = as.data.frame(read.csv("event_distribution1.csv"))
    }
    
    else if(input$x=='Harry Potter and the Chamber of Secrets'){
      kb = read.csv("storygraph_v2.csv")
      export = as.data.frame(read.csv("event_distribution2.csv"))
    }
    else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
      kb = read.csv("storygraph_v3.csv")
      export = as.data.frame(read.csv("event_distribution3.csv"))
    }
    else if(input$x=='Harry Potter and the Goblet of Fire'){
      kb = read.csv("storygraph_v4.csv")
      export = as.data.frame(read.csv("event_distribution4.csv"))
    }
    else if(input$x=='Harry Potter and the Order of the Phoenix'){
      kb = read.csv("storygraph_v5.csv")
      export = as.data.frame(read.csv("event_distribution5.csv"))
    }
    else if(input$x=='Harry Potter and the Half-Blood Prince'){
      kb = read.csv("storygraph_v6.csv")
      export = as.data.frame(read.csv("event_distribution6.csv"))
    }
    else{
      kb = read.csv("storygraph_v7.csv")
      export = as.data.frame(read.csv("event_distribution7.csv"))
    }
    
    
    
    # kb <- renderText({ 
    #  read.csv(kb_data())
    #})
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
    
    #Removing those rows in which subject and object both are NA
    kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
    
    #Defining Event
    event = list()
    event_sentences = list()
    j=0 #Event Index
    for(i in 1:length(kb$Sentence)){
      #print(i)
      if(length(event) == 0){
        currentObject <- strsplit(
          (kb$Object.Characters[1])," ")
        # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
        
        currentSubject <-   strsplit(
          (kb$Subject.Characters[1])," ")
        event[1] <- 1
        print("Event Initialised")
        j=1 #j is the event id
        previous_event_characters <- c(currentSubject,currentObject)
        
      }
      else{
        
        #Search in previous event
        length_Last_Event = length(event[[j]])
        #Storing the sentence id from the last event
        temp <- event[[j]]
        #print(c("temp =", temp))
        
        
        # for(k in 1:length_Last_Event){
        pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
        #New Algo for Scene Segmentation
        currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
        # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
        
        currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
        #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
        print(i)
        print("Previous Event")
        print(previous_event_characters)
        print("Current Subject")
        print(currentSubject)
        print("Current Object")
        print(currentObject)
        #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
        if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
            (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
          #A new event defined
          j = j+1 #Increasing the event id
          event[[j]] <- i
          #print("LALA")
          print("New eventCC")
          previous_event_characters <- c(currentSubject,currentObject)
        }
        else{
          if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
            if(length(setdiff(currentObject,pronounList))==0){
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
            else{
              j = j+1 #Increasing the event id
              event[[j]] <- i
              #print("LALA")
              print("New eventCC")
              previous_event_characters <- c(currentSubject,currentObject)}
          }
          else{
            event[[j]] <- c(event[[j]],i)
            print("Same Event")
            previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
          }
        }
      } 
    }
    ###############################
    
    ################################
    
    #Pairwise Interaction Analysis
    
    #Replacing NaN value from export to previous value
    
    index_replace_export <- which(is.nan(export$anger))
    for(h in 1:length(index_replace_export)){
      export$anger[index_replace_export[h]] <- 0
      export$anticipation[index_replace_export[h]] <- 0
      export$disgust[index_replace_export[h]] <- 0
      export$fear[index_replace_export[h]] <- 0
      export$joy[index_replace_export[h]] <- 0
      export$sadness[index_replace_export[h]] <- 0
      export$surprise[index_replace_export[h]] <- 0
      export$trust[index_replace_export[h]] <- 0
    }
    
    do_pairwise_interaction_analysis <- function(export, char1, char2){
      anger = 1:length(export$sentence_id) #Will be addressed in next function
      anticipation=1:length(export$sentence_id)
      disgust=1:length(export$sentence_id)
      fear=1:length(export$sentence_id)
      joy=1:length(export$sentence_id)
      sadness=1:length(export$sentence_id)
      surprise=1:length(export$sentence_id)
      trust=1:length(export$sentence_id)
      paragraph = 1:length(export$sentence_id)
      nrc_sentiment = 1:length(export$sentence_id)
      no_of_interactions = 1:length(export$sentence_id)
      Valence =  1:length(export$sentence_id)
      Arousal =  1:length(export$sentence_id)
      Dominance =  1:length(export$sentence_id)
      export_pair_emotions = data.frame(paragraph,no_of_interactions,nrc_sentiment,anger,anticipation,disgust,fear,joy,sadness,surprise,trust, Valence, Arousal, Dominance)
      for(event_index in 1:length(export$sentence_id)){
        
        #Selecting all the available information from kb for the current event
        current_event_kb <- kb[event[[event_index]][1]:event[[event_index]][length(event[[event_index]])],c(1,2,3,4,5,7),drop=F]
        #Condition for direction from character 2 to character 1
        interaction_in_current_event <- subset(current_event_kb, grepl(char2, Subject.Characters) & grepl(char1, dependency_tree))
        
        #Mapping emotion of character 1 towards character 2
        export_pair_emotions$paragraph[event_index] <- ""
        if(nrow(interaction_in_current_event)==0){
          export_pair_emotions$paragraph[event_index] <- paste(export_pair_emotions$paragraph[event_index],"NA", split = "")
        }
        else if(nrow(interaction_in_current_event)>1){
          
          export_pair_emotions$paragraph[event_index] <- paste(interaction_in_current_event$Sentence[1]:interaction_in_current_event$Sentence[nrow(interaction_in_current_event)],split=" ")
        }
        else if(nrow(interaction_in_current_event)==1){
          export_pair_emotions$paragraph[event_index] <- paste(export_pair_emotions$paragraph[event_index],interaction_in_current_event$Sentence[1], split = "")
        }
        else
          export_pair_emotions$paragraph[event_index] <- paste(export_pair_emotions$paragraph[event_index],"NA", split = "")
      }
      #########################################################
      #Emotion Labelling per event based on the sentences per event for each character pair
      
      for(event_index in 1:length(event)){
        
        
        if(export_pair_emotions$paragraph[event_index] != " NA "){
          print(event_index)
          #export_pair_emotions$anger[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[1]/length(event[[event_index]])
          export_pair_emotions$anger[event_index] <- export$anger[event_index]
          #export_pair_emotions$anticipation[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[2]/length(event[[event_index]])
          export_pair_emotions$anticipation[event_index] <- export$anticipation[event_index]
          #export_pair_emotions$disgust[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[3]/length(event[[event_index]])
          export_pair_emotions$disgust[event_index] <- export$disgust[event_index]
          #export_pair_emotions$fear[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[4]/length(event[[event_index]])
          export_pair_emotions$fear[event_index] <- export$fear[event_index]
          #export_pair_emotions$joy[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[5]/length(event[[event_index]])
          export_pair_emotions$joy[event_index] <- export$joy[event_index]
          #export_pair_emotions$sadness[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[6]/length(event[[event_index]])
          export_pair_emotions$sadness[event_index] <- export$sadness[event_index]
          #export_pair_emotions$surprise[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[7]/length(event[[event_index]])
          export_pair_emotions$surprise[event_index] <- export$surprise[event_index]
          #export_pair_emotions$trust[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[8]/length(event[[event_index]])
          export_pair_emotions$trust[event_index] <- export$trust[event_index]
          
          #Calculating Sentiment Score(Average of all emotions)
          #export_pair_emotions$nrc_sentiment[event_index] = mean(c(as.numeric(export_pair_emotions[event_index,3:10])))
          # if(length(get_sentiment(get_sentences(export_pair_emotions$paragraph[event_index])))>1)
          #   export_pair_emotions$nrc_sentiment[event_index] = mean(get_sentiment(get_sentences(export_pair_emotions$paragraph[event_index])))
          #  else
          export_pair_emotions$nrc_sentiment[event_index] =  export$nrc_sentiment[event_index]
          export_pair_emotions$no_of_interactions[event_index] = export$no_of_interactions[event_index]
          #fileConn<-file("input.txt")
          #write(export$paragraph[event_index], fileConn)
          #system("java -jar /home/jhavarharshita/PycharmProjects/MxP/JEmAS-master/JEmAS-v0.2-beta.jar /home/jhavarharshita/PycharmProjects/MxP/input.txt > output.csv", intern=TRUE)
          #close(fileConn)
          #temp_kb <- read.csv("output.csv")
          #temp_kb <- read.csv("output.csv", sep = "\t")
          #export_pair_emotions$Valence[event_index] = temp_kb$Valence[1]/length(event[[event_index]])
          export_pair_emotions$Valence[event_index] = export$Valence[event_index]
          #export_pair_emotions$Arousal[event_index] = temp_kb$Arousal[1]/length(event[[event_index]])
          export_pair_emotions$Arousal[event_index] = export$Arousal[event_index]
          #export_pair_emotions$Dominance[event_index] = temp_kb$Dominance[1]/length(event[[event_index]])
          export_pair_emotions$Dominance[event_index] = export$Dominance[event_index]
        }  
        else{
          if(event_index!=1){
            export_pair_emotions$anger[event_index] <- export_pair_emotions$anger[event_index-1]
            export_pair_emotions$anticipation[event_index] <- export_pair_emotions$anticipation[event_index-1]
            export_pair_emotions$disgust[event_index] <- export_pair_emotions$disgust[event_index-1]
            export_pair_emotions$fear[event_index] <- export_pair_emotions$fear[event_index-1]
            export_pair_emotions$joy[event_index] <-export_pair_emotions$joy[event_index-1] 
            export_pair_emotions$sadness[event_index] <- export_pair_emotions$sadness[event_index-1]
            export_pair_emotions$surprise[event_index] <- export_pair_emotions$surprise[event_index-1]
            export_pair_emotions$trust[event_index] <-  export_pair_emotions$trust[event_index-1]
            export_pair_emotions$nrc_sentiment[event_index] <- export_pair_emotions$nrc_sentiment[event_index-1]
            export_pair_emotions$no_of_interactions[event_index] <- 0
            export_pair_emotions$Valence[event_index] <- export_pair_emotions$Valence[event_index-1]
            export_pair_emotions$Arousal[event_index] <- export_pair_emotions$Arousal[event_index-1]
            export_pair_emotions$Dominance[event_index] <- export_pair_emotions$Dominance[event_index-1]
          }
          else{
            export_pair_emotions$anger[event_index] <- 0
            export_pair_emotions$anticipation[event_index] <- 0
            export_pair_emotions$disgust[event_index] <- 0
            export_pair_emotions$fear[event_index] <- 0
            export_pair_emotions$joy[event_index] <-0
            export_pair_emotions$sadness[event_index] <- 0
            export_pair_emotions$surprise[event_index] <- 0
            export_pair_emotions$trust[event_index] <-  0
            export_pair_emotions$nrc_sentiment[event_index]<- 0
            export_pair_emotions$Valence[event_index]<- 0
            export_pair_emotions$Arousal[event_index]<- 0
            export_pair_emotions$Dominance[event_index]<- 0
          }
        }}
      return(export_pair_emotions)
    }
    
    #Read the characters
    #char1 <- readline(prompt="Enter the first character name: ")
    #char2 <-  readline(prompt="Enter the second character name: ")
    char1 <- 'Other'
    char2 <- 'Other'
    character1 <- 'Character 1'
    character2 <- 'Character 2'
    if(input$user_char1input!="Enter your choice for character 1"){
      if(input$user_char1input=='Harry Potter'){
        char1 <- as.character('Harry|HARRY|Potter')
        character1 <- input$user_char1input
      }
      else if(input$user_char1input=='Ronald Weasley'){
        char1 <- as.character('Ron|Ronald|RON|RONALD|Weasley')
        character1 <- input$user_char1input
      }
      else if(input$user_char1input=='Hermione Granger'){
        char1 <- as.character('Hermione|Granger|HERMIONE|GRANGER')
        character1 <- input$user_char1input
      }
      else if(input$user_char1input=='Lord Voldemort'){
        char1 <- as.character('Voldemort|Dark Lord|Lord|LORD|YOU-KNOW-WHO')
        character1 <- input$user_char1input
      }
      else if(input$user_char1input=='Draco Malfoy'){
        char1 <- as.character('Malfoy|Draco|DRACO|MALFOY')
        character1 <- input$user_char1input
      }
      else if(input$user_char1input=='Dursleys'){
        char1 <- as.character('Dursleys|Dudely|Petunia|Vernon|DURSLEYS|DUDELY|PETUNIA|VERNON')
        character1 <- input$user_char1input
      }}
    # else if(input$user_char1input!="Enter your choice for character 1"){
    #  char1 <- input$user_char1input
    # char1 <- as.character(char1)
    #character1 <- char1
    #}
    #char1 <- as.character(input$user_char1input)
    #char2 <- as.character(input$user_char2input)
    if(input$user_char2input!="Enter your choice for character 2"){
      if(input$user_char2input=='Harry Potter'){
        char2 <- as.character('Harry|HARRY|Potter')
        character2 <- input$user_char2input
      }
      else if(input$user_char2input=='Ronald Weasley'){
        char2 <- as.character('Ron|Ronald|RON|RONALD|Weasley')
        character2 <- input$user_char2input
      }
      else if(input$user_char2input=='Hermione Granger'){
        char2 <- as.character('Hermione|Granger|HERMIONE|GRANGER')
        character2 <- input$user_char2input
      }
      else if(input$user_char2input=='Lord Voldemort'){
        char2 <- as.character('Voldemort|Dark Lord|Lord|LORD|YOU-KNOW-WHO')
        character2 <- input$user_char2input
      }
      else if(input$user_char2input=='Draco Malfoy'){
        char2 <- as.character('Malfoy|Draco|DRACO|MALFOY')
        character2 <- input$user_char2input
      }
      else if(input$user_char2input=='Dursleys'){
        char2 <- as.character('Dursleys|Dudely|Petunia|Vernon|DURSLEYS|DUDELY|PETUNIA|VERNON')
        character2 <- input$user_char2input
      }}
    # else if(input$user_char2input!="Enter your choice for character 2"){
    #  char2 <- input$user_char2input
    # char2 <- as.character(char2)
    #  character2 <- char2
    #}
    
    #export_backup <- export
    kb_pairwise_emotions = do_pairwise_interaction_analysis(export, char1, char2)
    ###############################################################################
    #Valence Plotting
    #plot(kb_pairwise_emotions$nrc_sentiment,xlab = "Event Indexes", ylab = "Emotion Valence between the pair", main = c("Emotion valence progress of ", char1, "towards ", char2),type = "l")
    #Emotions Plotting along different events for the character pairs
    #Emotions Plotting along different events for the character pairs
    
    library(ggplot2)
    library(reshape2)
    df_categorical <- data.frame(Scenes = 1:length(event),
                                 anger = kb_pairwise_emotions$anger,
                                 anticipation = kb_pairwise_emotions$anticipation,
                                 disgust = kb_pairwise_emotions$disgust,
                                 fear = kb_pairwise_emotions$fear,
                                 joy = kb_pairwise_emotions$joy,
                                 sadness = kb_pairwise_emotions$sadness,
                                 surprise = kb_pairwise_emotions$surprise,
                                 trust = kb_pairwise_emotions$trust)
    
    df_dimensional <- data.frame(Scenes = 1:length(event),
                                 valence = kb_pairwise_emotions$Valence,
                                 arousal = kb_pairwise_emotions$Arousal,
                                 dominance = kb_pairwise_emotions$Dominance)
    
    df_categorical <- melt(df_categorical ,  id.vars = 'Scenes', variable.name = 'Categorical_Emotions')
    df_dimensional <- melt(df_dimensional ,  id.vars = 'Scenes', variable.name = 'Dimensional_Emotions')
    # plot on same grid, each series colored differently -- 
    #plot_c <- amStockMultiSet('Categorical_Emotions')
     #plot_d <- 
    ggplot(df_dimensional, aes(Scenes,value))+ geom_line(aes(colour = Dimensional_Emotions),size=1) + scale_x_continuous(breaks=seq(1, length(event), 1))+
      labs(title=paste("Dimensional emotions trajectory for", character1, "towards", character2),x = "Progress of Story", y = "Dimensional emotions in each scene")+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)+ theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) 
    
    #require(gridExtra)
    #grid.arrange(plot_d, nrow=1)
    #height=700
    })
  
  output$`Emotion Mapping2` <- renderPlot({ 
    generate_plot2() 
  })
  ############################################################################################################################
  output$Character_pairs <- renderText({
   generate_plot_title_d1()
  })
  output$Character_pairs_reverse <- renderText({
    generate_plot_title_d2()
  })
  
    generate_plot_title_d1 <- eventReactive(input$goButton0,{
      if(input$user_char1input!="Enter your choice for character 1" & 
         input$user_char2input!="Enter your choice for character 2"){
        paste(input$user_char1input,"->",input$user_char2input)
      } else if(input$user_char1input!="Enter your choice for character 1"){
        paste(input$user_char1input,"->",input$user_char2input)
      } else if(input$user_char2input!="Enter your choice for character 2"){
        paste(input$user_char1input,"->",input$user_char2input)
      } else {
        paste(input$user_char1input,"->",input$user_char2input)
      }
    }
    )

    generate_plot_title_d2 <- eventReactive(input$goButton0,{
      if(input$user_char1input!="Enter your choice for character 1" & 
         input$user_char2input!="Enter your choice for character 2"){
        paste(input$user_char2input,"->",input$user_char1input)
      } else if(input$user_char1input!="Enter your choice for character 1"){
        paste(input$user_char2input,"->",input$user_char1input)
      } else if(input$user_char2input!="Enter your choice for character 2"){
        paste(input$user_char2input,"->",input$user_char1input)
      } else {
        paste(input$user_char2input,"->",input$user_char1input)
      }
    }
    )
  
  ########################################################################################################
  generate_plot3 <- eventReactive(input$goButton0,{
    #kb = read.csv("storygraph_v2.csv")
    #export = as.data.frame(read.csv("event_distribution2.csv"))
    #################################
    if(input$x=='Harry Potter and the Philosopher\'s Stone'){
      kb = read.csv("storygraph_v1.csv")
      export = as.data.frame(read.csv("event_distribution1.csv"))
    }
    
    else if(input$x=='Harry Potter and the Chamber of Secrets'){
      kb = read.csv("storygraph_v2.csv")
      export = as.data.frame(read.csv("event_distribution2.csv"))
    }
    else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
      kb = read.csv("storygraph_v3.csv")
      export = as.data.frame(read.csv("event_distribution3.csv"))
    }
    else if(input$x=='Harry Potter and the Goblet of Fire'){
      kb = read.csv("storygraph_v4.csv")
      export = as.data.frame(read.csv("event_distribution4.csv"))
    }
    else if(input$x=='Harry Potter and the Order of the Phoenix'){
      kb = read.csv("storygraph_v5.csv")
      export = as.data.frame(read.csv("event_distribution5.csv"))
    }
    else if(input$x=='Harry Potter and the Half-Blood Prince'){
      kb = read.csv("storygraph_v6.csv")
      export = as.data.frame(read.csv("event_distribution6.csv"))
    }
    else{
      kb = read.csv("storygraph_v7.csv")
      export = as.data.frame(read.csv("event_distribution7.csv"))
    }
    
    
    
    # kb <- renderText({ 
    #  read.csv(kb_data())
    #})
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
    
    #Removing those rows in which subject and object both are NA
    kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
    
    #Defining Event
    event = list()
    event_sentences = list()
    j=0 #Event Index
    for(i in 1:length(kb$Sentence)){
      #print(i)
      if(length(event) == 0){
        currentObject <- strsplit(
          (kb$Object.Characters[1])," ")
        # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
        
        currentSubject <-   strsplit(
          (kb$Subject.Characters[1])," ")
        event[1] <- 1
        print("Event Initialised")
        j=1 #j is the event id
        previous_event_characters <- c(currentSubject,currentObject)
        
      }
      else{
        
        #Search in previous event
        length_Last_Event = length(event[[j]])
        #Storing the sentence id from the last event
        temp <- event[[j]]
        #print(c("temp =", temp))
        
        
        # for(k in 1:length_Last_Event){
        pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
        #New Algo for Scene Segmentation
        currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
        # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
        
        currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
        #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]

        #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
        if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
            (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
          #A new event defined
          j = j+1 #Increasing the event id
          event[[j]] <- i
          #print("LALA")
          print("New eventCC")
          previous_event_characters <- c(currentSubject,currentObject)
        }
        else{
          if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
            if(length(setdiff(currentObject,pronounList))==0){
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
            else{
              j = j+1 #Increasing the event id
              event[[j]] <- i
              #print("LALA")
              print("New eventCC")
              previous_event_characters <- c(currentSubject,currentObject)}
          }
          else{
            event[[j]] <- c(event[[j]],i)
            print("Same Event")
            previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
          }
        }
      } 
    }
    ###############################
    
    ################################
    
    #Pairwise Interaction Analysis
 
    #Replacing NaN value from export to previous value
    
    index_replace_export <- which(is.nan(export$anger))
    for(h in 1:length(index_replace_export)){
      export$anger[index_replace_export[h]] <- 0
      export$anticipation[index_replace_export[h]] <- 0
      export$disgust[index_replace_export[h]] <- 0
      export$fear[index_replace_export[h]] <- 0
      export$joy[index_replace_export[h]] <- 0
      export$sadness[index_replace_export[h]] <- 0
      export$surprise[index_replace_export[h]] <- 0
      export$trust[index_replace_export[h]] <- 0
    }
    
    do_pairwise_interaction_analysis <- function(export, char1, char2){
      anger = 1:length(export$sentence_id) #Will be addressed in next function
      anticipation=1:length(export$sentence_id)
      disgust=1:length(export$sentence_id)
      fear=1:length(export$sentence_id)
      joy=1:length(export$sentence_id)
      sadness=1:length(export$sentence_id)
      surprise=1:length(export$sentence_id)
      trust=1:length(export$sentence_id)
      paragraph = 1:length(export$sentence_id)
      nrc_sentiment = 1:length(export$sentence_id)
      no_of_interactions = 1:length(export$sentence_id)
      Valence =  1:length(export$sentence_id)
      Arousal =  1:length(export$sentence_id)
      Dominance =  1:length(export$sentence_id)
      export_pair_emotions = data.frame(paragraph,no_of_interactions,nrc_sentiment,anger,anticipation,disgust,fear,joy,sadness,surprise,trust, Valence, Arousal, Dominance)
      for(event_index in 1:length(export$sentence_id)){
        
        #Selecting all the available information from kb for the current event
        current_event_kb <- kb[event[[event_index]][1]:event[[event_index]][length(event[[event_index]])],c(1,2,3,4,5,7),drop=F]
        #Condition for direction from character 2 to character 1
        interaction_in_current_event <- subset(current_event_kb, grepl(char2, Subject.Characters) & grepl(char1, dependency_tree))
        
        #Mapping emotion of character 1 towards character 2
        export_pair_emotions$paragraph[event_index] <- ""
        if(nrow(interaction_in_current_event)==0){
          export_pair_emotions$paragraph[event_index] <- paste(export_pair_emotions$paragraph[event_index],"NA", split = "")
        }
        else if(nrow(interaction_in_current_event)>1){
          
          export_pair_emotions$paragraph[event_index] <- paste(interaction_in_current_event$Sentence[1]:interaction_in_current_event$Sentence[nrow(interaction_in_current_event)],split=" ")
        }
        else if(nrow(interaction_in_current_event)==1){
          export_pair_emotions$paragraph[event_index] <- paste(export_pair_emotions$paragraph[event_index],interaction_in_current_event$Sentence[1], split = "")
        }
        else
          export_pair_emotions$paragraph[event_index] <- paste(export_pair_emotions$paragraph[event_index],"NA", split = "")
      }
      #########################################################
      #Emotion Labelling per event based on the sentences per event for each character pair
      
      for(event_index in 1:length(event)){
        
      
        if(export_pair_emotions$paragraph[event_index] != " NA "){
          print(event_index)
          #export_pair_emotions$anger[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[1]/length(event[[event_index]])
          export_pair_emotions$anger[event_index] <- export$anger[event_index]
          #export_pair_emotions$anticipation[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[2]/length(event[[event_index]])
          export_pair_emotions$anticipation[event_index] <- export$anticipation[event_index]
          #export_pair_emotions$disgust[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[3]/length(event[[event_index]])
          export_pair_emotions$disgust[event_index] <- export$disgust[event_index]
          #export_pair_emotions$fear[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[4]/length(event[[event_index]])
          export_pair_emotions$fear[event_index] <- export$fear[event_index]
          #export_pair_emotions$joy[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[5]/length(event[[event_index]])
          export_pair_emotions$joy[event_index] <- export$joy[event_index]
          #export_pair_emotions$sadness[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[6]/length(event[[event_index]])
          export_pair_emotions$sadness[event_index] <- export$sadness[event_index]
          #export_pair_emotions$surprise[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[7]/length(event[[event_index]])
          export_pair_emotions$surprise[event_index] <- export$surprise[event_index]
          #export_pair_emotions$trust[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[8]/length(event[[event_index]])
          export_pair_emotions$trust[event_index] <- export$trust[event_index]
          
          #Calculating Sentiment Score(Average of all emotions)
          #export_pair_emotions$nrc_sentiment[event_index] = mean(c(as.numeric(export_pair_emotions[event_index,3:10])))
         # if(length(get_sentiment(get_sentences(export_pair_emotions$paragraph[event_index])))>1)
         #   export_pair_emotions$nrc_sentiment[event_index] = mean(get_sentiment(get_sentences(export_pair_emotions$paragraph[event_index])))
        #  else
            export_pair_emotions$nrc_sentiment[event_index] =  export$nrc_sentiment[event_index]
          export_pair_emotions$no_of_interactions[event_index] = export$no_of_interactions[event_index]
          #fileConn<-file("input.txt")
          #write(export$paragraph[event_index], fileConn)
          #system("java -jar /home/jhavarharshita/PycharmProjects/MxP/JEmAS-master/JEmAS-v0.2-beta.jar /home/jhavarharshita/PycharmProjects/MxP/input.txt > output.csv", intern=TRUE)
          #close(fileConn)
          #temp_kb <- read.csv("output.csv")
          #temp_kb <- read.csv("output.csv", sep = "\t")
          #export_pair_emotions$Valence[event_index] = temp_kb$Valence[1]/length(event[[event_index]])
          export_pair_emotions$Valence[event_index] = export$Valence[event_index]
          #export_pair_emotions$Arousal[event_index] = temp_kb$Arousal[1]/length(event[[event_index]])
          export_pair_emotions$Arousal[event_index] = export$Arousal[event_index]
          #export_pair_emotions$Dominance[event_index] = temp_kb$Dominance[1]/length(event[[event_index]])
          export_pair_emotions$Dominance[event_index] = export$Dominance[event_index]
        }  
        else{
          if(event_index!=1){
            export_pair_emotions$anger[event_index] <- export_pair_emotions$anger[event_index-1]
            export_pair_emotions$anticipation[event_index] <- export_pair_emotions$anticipation[event_index-1]
            export_pair_emotions$disgust[event_index] <- export_pair_emotions$disgust[event_index-1]
            export_pair_emotions$fear[event_index] <- export_pair_emotions$fear[event_index-1]
            export_pair_emotions$joy[event_index] <-export_pair_emotions$joy[event_index-1] 
            export_pair_emotions$sadness[event_index] <- export_pair_emotions$sadness[event_index-1]
            export_pair_emotions$surprise[event_index] <- export_pair_emotions$surprise[event_index-1]
            export_pair_emotions$trust[event_index] <-  export_pair_emotions$trust[event_index-1]
            export_pair_emotions$nrc_sentiment[event_index] <- export_pair_emotions$nrc_sentiment[event_index-1]
            export_pair_emotions$no_of_interactions[event_index] <- 0
            export_pair_emotions$Valence[event_index] <- export_pair_emotions$Valence[event_index-1]
            export_pair_emotions$Arousal[event_index] <- export_pair_emotions$Arousal[event_index-1]
            export_pair_emotions$Dominance[event_index] <- export_pair_emotions$Dominance[event_index-1]
          }
          else{
            export_pair_emotions$anger[event_index] <- 0
            export_pair_emotions$anticipation[event_index] <- 0
            export_pair_emotions$disgust[event_index] <- 0
            export_pair_emotions$fear[event_index] <- 0
            export_pair_emotions$joy[event_index] <-0
            export_pair_emotions$sadness[event_index] <- 0
            export_pair_emotions$surprise[event_index] <- 0
            export_pair_emotions$trust[event_index] <-  0
            export_pair_emotions$nrc_sentiment[event_index]<- 0
            export_pair_emotions$Valence[event_index]<- 0
            export_pair_emotions$Arousal[event_index]<- 0
            export_pair_emotions$Dominance[event_index]<- 0
          }
        }}
      return(export_pair_emotions)
    }
    
    #Read the characters
    #char1 <- readline(prompt="Enter the first character name: ")
    #char2 <-  readline(prompt="Enter the second character name: ")
    char1 <- 'Other'
    char2 <- 'Other'
    character1 <- 'Character 1'
    character2 <- 'Character 2'
    if(input$user_char1input!="Enter your choice for character 1"){
      if(input$user_char1input=='Harry Potter'){
        char1 <- as.character('Harry|HARRY|Potter')
        character1 <- input$user_char1input
      }
      else if(input$user_char1input=='Ronald Weasley'){
        char1 <- as.character('Ron|Ronald|RON|RONALD|Weasley')
        character1 <- input$user_char1input
      }
      else if(input$user_char1input=='Hermione Granger'){
        char1 <- as.character('Hermione|Granger|HERMIONE|GRANGER')
        character1 <- input$user_char1input
      }
      else if(input$user_char1input=='Lord Voldemort'){
        char1 <- as.character('Voldemort|Dark Lord|Lord|LORD|YOU-KNOW-WHO')
        character1 <- input$user_char1input
      }
      else if(input$user_char1input=='Draco Malfoy'){
        char1 <- as.character('Malfoy|Draco|DRACO|MALFOY')
        character1 <- input$user_char1input
      }
      else if(input$user_char1input=='Dursleys'){
        char1 <- as.character('Dursleys|Dudely|Petunia|Vernon|DURSLEYS|DUDELY|PETUNIA|VERNON')
        character1 <- input$user_char1input
      }}
   # else if(input$user_char1input!="Enter your choice for character 1"){
    #  char1 <- input$user_char1input
     # char1 <- as.character(char1)
      #character1 <- char1
  #  }
    #char1 <- as.character(input$user_char1input)
    #char2 <- as.character(input$user_char2input)
    if(input$user_char2input!="Enter your choice for character 2"){
      if(input$user_char2input=='Harry Potter'){
        char2 <- as.character('Harry|HARRY|Potter')
        character2 <- input$user_char2input
      }
      else if(input$user_char2input=='Ronald Weasley'){
        char2 <- as.character('Ron|Ronald|RON|RONALD|Weasley')
        character2 <- input$user_char2input
      }
      else if(input$user_char2input=='Hermione Granger'){
        char2 <- as.character('Hermione|Granger|HERMIONE|GRANGER')
        character2 <- input$user_char2input
      }
      else if(input$user_char2input=='Lord Voldemort'){
        char2 <- as.character('Voldemort|Dark Lord|Lord|LORD|YOU-KNOW-WHO')
        character2 <- input$user_char2input
      }
      else if(input$user_char2input=='Draco Malfoy'){
        char2 <- as.character('Malfoy|Draco|DRACO|MALFOY')
        character2 <- input$user_char2input
      }
      else if(input$user_char2input=='Dursleys'){
        char2 <- as.character('Dursleys|Dudely|Petunia|Vernon|DURSLEYS|DUDELY|PETUNIA|VERNON')
        character2 <- input$user_char2input
      }}
  #  else if(input$user_char2input!="Enter your choice for character 2"){
   #   char2 <- input$user_char2input
    #  char2 <- as.character(char2)
     # character2 <- char2
    #}
    
    #export_backup <- export
    kb_pairwise_emotions = do_pairwise_interaction_analysis(export, char2, char1)
    ###############################################################################
    #Valence Plotting
    #plot(kb_pairwise_emotions$nrc_sentiment,xlab = "Event Indexes", ylab = "Emotion Valence between the pair", main = c("Emotion valence progress of ", char1, "towards ", char2),type = "l")
    #Emotions Plotting along different events for the character pairs
    #Emotions Plotting along different events for the character pairs
    
    library(ggplot2)
    library(reshape2)
    df_categorical <- data.frame(Scenes = 1:length(event),
                                 anger = kb_pairwise_emotions$anger,
                                 anticipation = kb_pairwise_emotions$anticipation,
                                 disgust = kb_pairwise_emotions$disgust,
                                 fear = kb_pairwise_emotions$fear,
                                 joy = kb_pairwise_emotions$joy,
                                 sadness = kb_pairwise_emotions$sadness,
                                 surprise = kb_pairwise_emotions$surprise,
                                 trust = kb_pairwise_emotions$trust)
    df_dimensional <- data.frame(Scenes = 1:length(event),
                                 valence = kb_pairwise_emotions$Valence,
                                 arousal = kb_pairwise_emotions$Arousal,
                                 dominance = kb_pairwise_emotions$Dominance)
    df_categorical <- melt(df_categorical ,  id.vars = 'Scenes', variable.name = 'Categorical_Emotions')
    df_dimensional <- melt(df_dimensional ,  id.vars = 'Scenes', variable.name = 'Dimensional_Emotions')
    # plot on same grid, each series colored differently -- 
    
    #plot_c <- 
    ggplot(df_categorical, aes(Scenes,value))+ geom_line(aes(colour = Categorical_Emotions),size=1) + scale_x_continuous(breaks=seq(1, length(event), 1))+
      labs(title=paste("Categorical emotions trajectory for", character2, "towards",character1), x = "Progress of Story", y= "Categorical emotions in each scene")+coord_cartesian(xlim = ranges3$x, ylim = ranges3$y, expand = FALSE)+ theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    
  #require(gridExtra)
   #grid.arrange(plot_c, nrow=1)
    #height=700
    })
  
    output$`Emotion Mapping3` <- renderPlot({ 
      generate_plot3() 
    })
    
    generate_plot4 <- eventReactive(input$goButton0,{
      #kb = read.csv("storygraph_v2.csv")
      #export = as.data.frame(read.csv("event_distribution2.csv"))
      #################################
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      
      # kb <- renderText({ 
      #  read.csv(kb_data())
      #})
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
      
      #Removing those rows in which subject and object both are NA
      kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
      
      #Defining Event
      event = list()
      event_sentences = list()
      j=0 #Event Index
      for(i in 1:length(kb$Sentence)){
        #print(i)
        if(length(event) == 0){
          currentObject <- strsplit(
            (kb$Object.Characters[1])," ")
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   strsplit(
            (kb$Subject.Characters[1])," ")
          event[1] <- 1
          print("Event Initialised")
          j=1 #j is the event id
          previous_event_characters <- c(currentSubject,currentObject)
          
        }
        else{
          
          #Search in previous event
          length_Last_Event = length(event[[j]])
          #Storing the sentence id from the last event
          temp <- event[[j]]
          #print(c("temp =", temp))
          
          
          # for(k in 1:length_Last_Event){
          pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
          #New Algo for Scene Segmentation
          currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
          #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
          print(i)
          print("Previous Event")
          print(previous_event_characters)
          print("Current Subject")
          print(currentSubject)
          print("Current Object")
          print(currentObject)
          #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
          if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
              (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
            #A new event defined
            j = j+1 #Increasing the event id
            event[[j]] <- i
            #print("LALA")
            print("New eventCC")
            previous_event_characters <- c(currentSubject,currentObject)
          }
          else{
            if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
              if(length(setdiff(currentObject,pronounList))==0){
                event[[j]] <- c(event[[j]],i)
                print("Same Event")
                previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
              }
              else{
                j = j+1 #Increasing the event id
                event[[j]] <- i
                #print("LALA")
                print("New eventCC")
                previous_event_characters <- c(currentSubject,currentObject)}
            }
            else{
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
          }
        } 
      }
      ###############################
      
      ################################
      
      #Pairwise Interaction Analysis
      
      #Replacing NaN value from export to previous value
      
      index_replace_export <- which(is.nan(export$anger))
      for(h in 1:length(index_replace_export)){
        export$anger[index_replace_export[h]] <- 0
        export$anticipation[index_replace_export[h]] <- 0
        export$disgust[index_replace_export[h]] <- 0
        export$fear[index_replace_export[h]] <- 0
        export$joy[index_replace_export[h]] <- 0
        export$sadness[index_replace_export[h]] <- 0
        export$surprise[index_replace_export[h]] <- 0
        export$trust[index_replace_export[h]] <- 0
      }
      
      do_pairwise_interaction_analysis <- function(export, char1, char2){
        anger = 1:length(export$sentence_id) #Will be addressed in next function
        anticipation=1:length(export$sentence_id)
        disgust=1:length(export$sentence_id)
        fear=1:length(export$sentence_id)
        joy=1:length(export$sentence_id)
        sadness=1:length(export$sentence_id)
        surprise=1:length(export$sentence_id)
        trust=1:length(export$sentence_id)
        paragraph = 1:length(export$sentence_id)
        nrc_sentiment = 1:length(export$sentence_id)
        no_of_interactions = 1:length(export$sentence_id)
        Valence =  1:length(export$sentence_id)
        Arousal =  1:length(export$sentence_id)
        Dominance =  1:length(export$sentence_id)
        export_pair_emotions = data.frame(paragraph,no_of_interactions,nrc_sentiment,anger,anticipation,disgust,fear,joy,sadness,surprise,trust, Valence, Arousal, Dominance)
        for(event_index in 1:length(export$sentence_id)){
          
          #Selecting all the available information from kb for the current event
          current_event_kb <- kb[event[[event_index]][1]:event[[event_index]][length(event[[event_index]])],c(1,2,3,4,5,7),drop=F]
          #Condition for direction from character 2 to character 1
          interaction_in_current_event <- subset(current_event_kb, grepl(char2, Subject.Characters) & grepl(char1, dependency_tree))
          
          #Mapping emotion of character 1 towards character 2
          export_pair_emotions$paragraph[event_index] <- ""
          if(nrow(interaction_in_current_event)==0){
            export_pair_emotions$paragraph[event_index] <- paste(export_pair_emotions$paragraph[event_index],"NA", split = "")
          }
          else if(nrow(interaction_in_current_event)>1){
            
            export_pair_emotions$paragraph[event_index] <- paste(interaction_in_current_event$Sentence[1]:interaction_in_current_event$Sentence[nrow(interaction_in_current_event)],split=" ")
          }
          else if(nrow(interaction_in_current_event)==1){
            export_pair_emotions$paragraph[event_index] <- paste(export_pair_emotions$paragraph[event_index],interaction_in_current_event$Sentence[1], split = "")
          }
          else
            export_pair_emotions$paragraph[event_index] <- paste(export_pair_emotions$paragraph[event_index],"NA", split = "")
        }
        #########################################################
        #Emotion Labelling per event based on the sentences per event for each character pair
        
        for(event_index in 1:length(event)){
          
          
          if(export_pair_emotions$paragraph[event_index] != " NA "){
            print(event_index)
            #export_pair_emotions$anger[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[1]/length(event[[event_index]])
            export_pair_emotions$anger[event_index] <- export$anger[event_index]
            #export_pair_emotions$anticipation[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[2]/length(event[[event_index]])
            export_pair_emotions$anticipation[event_index] <- export$anticipation[event_index]
            #export_pair_emotions$disgust[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[3]/length(event[[event_index]])
            export_pair_emotions$disgust[event_index] <- export$disgust[event_index]
            #export_pair_emotions$fear[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[4]/length(event[[event_index]])
            export_pair_emotions$fear[event_index] <- export$fear[event_index]
            #export_pair_emotions$joy[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[5]/length(event[[event_index]])
            export_pair_emotions$joy[event_index] <- export$joy[event_index]
            #export_pair_emotions$sadness[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[6]/length(event[[event_index]])
            export_pair_emotions$sadness[event_index] <- export$sadness[event_index]
            #export_pair_emotions$surprise[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[7]/length(event[[event_index]])
            export_pair_emotions$surprise[event_index] <- export$surprise[event_index]
            #export_pair_emotions$trust[event_index] <- colSums(prop.table(nrc_data[, 1:8]))[8]/length(event[[event_index]])
            export_pair_emotions$trust[event_index] <- export$trust[event_index]
            
            #Calculating Sentiment Score(Average of all emotions)
            #export_pair_emotions$nrc_sentiment[event_index] = mean(c(as.numeric(export_pair_emotions[event_index,3:10])))
            # if(length(get_sentiment(get_sentences(export_pair_emotions$paragraph[event_index])))>1)
            #   export_pair_emotions$nrc_sentiment[event_index] = mean(get_sentiment(get_sentences(export_pair_emotions$paragraph[event_index])))
            #  else
            export_pair_emotions$nrc_sentiment[event_index] =  export$nrc_sentiment[event_index]
            export_pair_emotions$no_of_interactions[event_index] = export$no_of_interactions[event_index]
            #fileConn<-file("input.txt")
            #write(export$paragraph[event_index], fileConn)
            #system("java -jar /home/jhavarharshita/PycharmProjects/MxP/JEmAS-master/JEmAS-v0.2-beta.jar /home/jhavarharshita/PycharmProjects/MxP/input.txt > output.csv", intern=TRUE)
            #close(fileConn)
            #temp_kb <- read.csv("output.csv")
            #temp_kb <- read.csv("output.csv", sep = "\t")
            #export_pair_emotions$Valence[event_index] = temp_kb$Valence[1]/length(event[[event_index]])
            export_pair_emotions$Valence[event_index] = export$Valence[event_index]
            #export_pair_emotions$Arousal[event_index] = temp_kb$Arousal[1]/length(event[[event_index]])
            export_pair_emotions$Arousal[event_index] = export$Arousal[event_index]
            #export_pair_emotions$Dominance[event_index] = temp_kb$Dominance[1]/length(event[[event_index]])
            export_pair_emotions$Dominance[event_index] = export$Dominance[event_index]
          }  
          else{
            if(event_index!=1){
              export_pair_emotions$anger[event_index] <- export_pair_emotions$anger[event_index-1]
              export_pair_emotions$anticipation[event_index] <- export_pair_emotions$anticipation[event_index-1]
              export_pair_emotions$disgust[event_index] <- export_pair_emotions$disgust[event_index-1]
              export_pair_emotions$fear[event_index] <- export_pair_emotions$fear[event_index-1]
              export_pair_emotions$joy[event_index] <-export_pair_emotions$joy[event_index-1] 
              export_pair_emotions$sadness[event_index] <- export_pair_emotions$sadness[event_index-1]
              export_pair_emotions$surprise[event_index] <- export_pair_emotions$surprise[event_index-1]
              export_pair_emotions$trust[event_index] <-  export_pair_emotions$trust[event_index-1]
              export_pair_emotions$nrc_sentiment[event_index] <- export_pair_emotions$nrc_sentiment[event_index-1]
              export_pair_emotions$no_of_interactions[event_index] <- 0
              export_pair_emotions$Valence[event_index] <- export_pair_emotions$Valence[event_index-1]
              export_pair_emotions$Arousal[event_index] <- export_pair_emotions$Arousal[event_index-1]
              export_pair_emotions$Dominance[event_index] <- export_pair_emotions$Dominance[event_index-1]
            }
            else{
              export_pair_emotions$anger[event_index] <- 0
              export_pair_emotions$anticipation[event_index] <- 0
              export_pair_emotions$disgust[event_index] <- 0
              export_pair_emotions$fear[event_index] <- 0
              export_pair_emotions$joy[event_index] <-0
              export_pair_emotions$sadness[event_index] <- 0
              export_pair_emotions$surprise[event_index] <- 0
              export_pair_emotions$trust[event_index] <-  0
              export_pair_emotions$nrc_sentiment[event_index]<- 0
              export_pair_emotions$Valence[event_index]<- 0
              export_pair_emotions$Arousal[event_index]<- 0
              export_pair_emotions$Dominance[event_index]<- 0
            }
          }}
        return(export_pair_emotions)
      }
      
      #Read the characters
      #char1 <- readline(prompt="Enter the first character name: ")
      #char2 <-  readline(prompt="Enter the second character name: ")
      char1 <- 'Other'
      char2 <- 'Other'
      character1 <- 'Character 1'
      character2 <- 'Character 2'
      if(input$user_char1input!="Enter your choice for character 1"){
        if(input$user_char1input=='Harry Potter'){
          char1 <- as.character('Harry|HARRY|Potter')
          character1 <- input$user_char1input
        }
        else if(input$user_char1input=='Ronald Weasley'){
          char1 <- as.character('Ron|Ronald|RON|RONALD|Weasley')
          character1 <- input$user_char1input
        }
        else if(input$user_char1input=='Hermione Granger'){
          char1 <- as.character('Hermione|Granger|HERMIONE|GRANGER')
          character1 <- input$user_char1input
        }
        else if(input$user_char1input=='Lord Voldemort'){
          char1 <- as.character('Voldemort|Dark Lord|Lord|LORD|YOU-KNOW-WHO')
          character1 <- input$user_char1input
        }
        else if(input$user_char1input=='Draco Malfoy'){
          char1 <- as.character('Malfoy|Draco|DRACO|MALFOY')
          character1 <- input$user_char1input
        }
        else if(input$user_char1input=='Dursleys'){
          char1 <- as.character('Dursleys|Dudely|Petunia|Vernon|DURSLEYS|DUDELY|PETUNIA|VERNON')
          character1 <- input$user_char1input
        }}
      # else if(input$user_char1input!="Enter your choice for character 1"){
      #  char1 <- input$user_char1input
      # char1 <- as.character(char1)
      #character1 <- char1
      #  }
      #char1 <- as.character(input$user_char1input)
      #char2 <- as.character(input$user_char2input)
      if(input$user_char2input!="Enter your choice for character 2"){
        if(input$user_char2input=='Harry Potter'){
          char2 <- as.character('Harry|HARRY|Potter')
          character2 <- input$user_char2input
        }
        else if(input$user_char2input=='Ronald Weasley'){
          char2 <- as.character('Ron|Ronald|RON|RONALD|Weasley')
          character2 <- input$user_char2input
        }
        else if(input$user_char2input=='Hermione Granger'){
          char2 <- as.character('Hermione|Granger|HERMIONE|GRANGER')
          character2 <- input$user_char2input
        }
        else if(input$user_char2input=='Lord Voldemort'){
          char2 <- as.character('Voldemort|Dark Lord|Lord|LORD|YOU-KNOW-WHO')
          character2 <- input$user_char2input
        }
        else if(input$user_char2input=='Draco Malfoy'){
          char2 <- as.character('Malfoy|Draco|DRACO|MALFOY')
          character2 <- input$user_char2input
        }
        else if(input$user_char2input=='Dursleys'){
          char2 <- as.character('Dursleys|Dudely|Petunia|Vernon|DURSLEYS|DUDELY|PETUNIA|VERNON')
          character2 <- input$user_char2input
        }}
      #  else if(input$user_char2input!="Enter your choice for character 2"){
      #   char2 <- input$user_char2input
      #  char2 <- as.character(char2)
      # character2 <- char2
      #}
      
      #export_backup <- export
      kb_pairwise_emotions = do_pairwise_interaction_analysis(export, char2, char1)
      ###############################################################################
      #Valence Plotting
      #plot(kb_pairwise_emotions$nrc_sentiment,xlab = "Event Indexes", ylab = "Emotion Valence between the pair", main = c("Emotion valence progress of ", char1, "towards ", char2),type = "l")
      #Emotions Plotting along different events for the character pairs
      #Emotions Plotting along different events for the character pairs
      
      library(ggplot2)
      library(reshape2)
      df_categorical <- data.frame(Scenes = 1:length(event),
                                   anger = kb_pairwise_emotions$anger,
                                   anticipation = kb_pairwise_emotions$anticipation,
                                   disgust = kb_pairwise_emotions$disgust,
                                   fear = kb_pairwise_emotions$fear,
                                   joy = kb_pairwise_emotions$joy,
                                   sadness = kb_pairwise_emotions$sadness,
                                   surprise = kb_pairwise_emotions$surprise,
                                   trust = kb_pairwise_emotions$trust)
      df_dimensional <- data.frame(Scenes = 1:length(event),
                                   valence = kb_pairwise_emotions$Valence,
                                   arousal = kb_pairwise_emotions$Arousal,
                                   dominance = kb_pairwise_emotions$Dominance)
      df_categorical <- melt(df_categorical ,  id.vars = 'Scenes', variable.name = 'Categorical_Emotions')
      df_dimensional <- melt(df_dimensional ,  id.vars = 'Scenes', variable.name = 'Dimensional_Emotions')
      # plot on same grid, each series colored differently -- 
      
      #plot_d <- 
      ggplot(df_dimensional, aes(Scenes,value))+ geom_line(aes(colour = Dimensional_Emotions),size=1) + scale_x_continuous(breaks=seq(1, length(event), 1))+
        labs(title=paste("Dimensional emotions trajectory for", character2, "towards", character1),x = "Progress of Story", y = "Dimensional emotions in each scene")+coord_cartesian(xlim = ranges4$x, ylim = ranges4$y, expand = FALSE) + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      
      #require(gridExtra)
      #grid.arrange(plot_d, nrow=1)
      #height=700
      })
    
    output$`Emotion Mapping4` <- renderPlot({ 
      generate_plot4() 
    })
    observeEvent(input$plot1_dblclick, {
      brush <- input$plot1_brush
      if (!is.null(brush)) {
        ranges1$x <- c(brush$xmin, brush$xmax)
        ranges1$y <- c(brush$ymin, brush$ymax)
        
      } else {
        ranges1$x <- NULL
        ranges1$y <- NULL
      }
    })
    
    observeEvent(input$plot2_dblclick, {
      brush <- input$plot2_brush
      if (!is.null(brush)) {
        ranges2$x <- c(brush$xmin, brush$xmax)
        ranges2$y <- c(brush$ymin, brush$ymax)
        
      } else {
        ranges2$x <- NULL
        ranges2$y <- NULL
      }
    })
    
    observeEvent(input$plot3_dblclick, {
      brush <- input$plot3_dblclick
      if (!is.null(brush)) {
        ranges3$x <- c(brush$xmin, brush$xmax)
        ranges3$y <- c(brush$ymin, brush$ymax)
        
      } else {
        ranges3$x <- NULL
        ranges3$y <- NULL
      }
    })
    
    observeEvent(input$plot4_dblclick, {
      brush <- input$plot4_brush
      if (!is.null(brush)) {
        ranges4$x <- c(brush$xmin, brush$xmax)
        ranges4$y <- c(brush$ymin, brush$ymax)
        
      } else {
        ranges4$x <- NULL
        ranges4$y <- NULL
      }
    })
  ###########################################################################################################
    ##########################################################################################################
    text_emotion <- renderText({
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      paste("Categorical Emotion Mapping Values:",
            "  Anger = ", export$anger[floor(as.numeric(input$plot_click1$x))],
            "  Anticipation = ",export$anticipation[floor(as.numeric(input$plot_click1$x))],
            "  Disgust = ",export$disgust[as.integer(floor(as.numeric(input$plot_click1$x)))],
            "  Fear = ",export$fear[floor(as.numeric(input$plot_click1$x))],
            "  Joy = ",export$joy[floor(as.numeric(input$plot_click1$x))],
            "  Sadness = ",export$sadness[floor(as.numeric(input$plot_click1$x))],
            "  Surprise = ",export$surprise[floor(as.numeric(input$plot_click1$x))],
            "  Trust = ",export$trust[floor(as.numeric(input$plot_click1$x))],
            "  Dimensional Emotional Mapping Values",
            "  Valence = ",export$Valence[floor(as.numeric(input$plot_click1$x))],
            "  Arousal = ",export$Arousal[floor(as.numeric(input$plot_click1$x))],
            "  Dominance = ",export$Dominance[floor(as.numeric(input$plot_click1$x))],sep="\n")
    })
    output$`Selected Scene Emotion Analysis` <- eventReactive(input$plot_click1,{ 
      text_emotion()
    })
    
   
    #Scene Description
    text_d <- renderText({
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      paste("Scene",floor(as.numeric(input$plot_click1$x)),"Description:",export$paragraph[floor(as.numeric(input$plot_click1$x))])
    })
    
    text_sub <- renderText( {
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      
      # kb <- renderText({ 
      #  read.csv(kb_data())
      #})
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
      
      #Removing those rows in which subject and object both are NA
      kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
      
      #Defining Event
      event = list()
      event_sentences = list()
      j=0 #Event Index
      for(i in 1:length(kb$Sentence)){
        #print(i)
        if(length(event) == 0){
          currentObject <- strsplit(
            (kb$Object.Characters[1])," ")
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   strsplit(
            (kb$Subject.Characters[1])," ")
          event[1] <- 1
          print("Event Initialised")
          j=1 #j is the event id
          previous_event_characters <- c(currentSubject,currentObject)
          
        }
        else{
          
          #Search in previous event
          length_Last_Event = length(event[[j]])
          #Storing the sentence id from the last event
          temp <- event[[j]]
          #print(c("temp =", temp))
          
          
          # for(k in 1:length_Last_Event){
          pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
          #New Algo for Scene Segmentation
          currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
          #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
          print(i)
          print("Previous Event")
          print(previous_event_characters)
          print("Current Subject")
          print(currentSubject)
          print("Current Object")
          print(currentObject)
          #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
          if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
              (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
            #A new event defined
            j = j+1 #Increasing the event id
            event[[j]] <- i
            #print("LALA")
            print("New eventCC")
            previous_event_characters <- c(currentSubject,currentObject)
          }
          else{
            if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
              if(length(setdiff(currentObject,pronounList))==0){
                event[[j]] <- c(event[[j]],i)
                print("Same Event")
                previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
              }
              else{
                j = j+1 #Increasing the event id
                event[[j]] <- i
                #print("LALA")
                print("New eventCC")
                previous_event_characters <- c(currentSubject,currentObject)}
            }
            else{
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
          }
        } 
      }
      
      scene_kb <- kb[event[[floor(as.numeric(input$plot_click1$x))]][1]:event[[floor(as.numeric(input$plot_click1$x))]][length(event[[floor(as.numeric(input$plot_click1$x))]])],c(1,2,3,4,5),drop=F]
      c("Agents+associated actions:",scene_kb$Actions.with.Subject.Charcater)
    })
    
    
    text_obj <- renderText( {
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      
      # kb <- renderText({ 
      #  read.csv(kb_data())
      #})
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
      
      #Removing those rows in which subject and object both are NA
      kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
      
      #Defining Event
      event = list()
      event_sentences = list()
      j=0 #Event Index
      for(i in 1:length(kb$Sentence)){
        #print(i)
        if(length(event) == 0){
          currentObject <- strsplit(
            (kb$Object.Characters[1])," ")
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   strsplit(
            (kb$Subject.Characters[1])," ")
          event[1] <- 1
          print("Event Initialised")
          j=1 #j is the event id
          previous_event_characters <- c(currentSubject,currentObject)
          
        }
        else{
          
          #Search in previous event
          length_Last_Event = length(event[[j]])
          #Storing the sentence id from the last event
          temp <- event[[j]]
          #print(c("temp =", temp))
          
          
          # for(k in 1:length_Last_Event){
          pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
          #New Algo for Scene Segmentation
          currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
          #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
          print(i)
          print("Previous Event")
          print(previous_event_characters)
          print("Current Subject")
          print(currentSubject)
          print("Current Object")
          print(currentObject)
          #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
          if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
              (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
            #A new event defined
            j = j+1 #Increasing the event id
            event[[j]] <- i
            #print("LALA")
            print("New eventCC")
            previous_event_characters <- c(currentSubject,currentObject)
          }
          else{
            if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
              if(length(setdiff(currentObject,pronounList))==0){
                event[[j]] <- c(event[[j]],i)
                print("Same Event")
                previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
              }
              else{
                j = j+1 #Increasing the event id
                event[[j]] <- i
                #print("LALA")
                print("New eventCC")
                previous_event_characters <- c(currentSubject,currentObject)}
            }
            else{
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
          }
        } 
      }
      
      scene_kb <- kb[event[[floor(as.numeric(input$plot_click1$x))]][1]:event[[floor(as.numeric(input$plot_click1$x))]][length(event[[floor(as.numeric(input$plot_click1$x))]])],c(1,2,3,4,5),drop=F]
      c("Patients+associated Actions:",scene_kb$Objects.and.Actions)
    })
  
    
    bar_plot <- eventReactive( input$plot_click1,{
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      
      # kb <- renderText({ 
      #  read.csv(kb_data())
      #})
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
      
      #Removing those rows in which subject and object both are NA
      kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
      
      #Defining Event
      event = list()
      event_sentences = list()
      j=0 #Event Index
      for(i in 1:length(kb$Sentence)){
        #print(i)
        if(length(event) == 0){
          currentObject <- strsplit(
            (kb$Object.Characters[1])," ")
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   strsplit(
            (kb$Subject.Characters[1])," ")
          event[1] <- 1
          print("Event Initialised")
          j=1 #j is the event id
          previous_event_characters <- c(currentSubject,currentObject)
          
        }
        else{
          
          #Search in previous event
          length_Last_Event = length(event[[j]])
          #Storing the sentence id from the last event
          temp <- event[[j]]
          #print(c("temp =", temp))
          
          
          # for(k in 1:length_Last_Event){
          pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
          #New Algo for Scene Segmentation
          currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
          #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
         
          #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
          if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
              (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
            #A new event defined
            j = j+1 #Increasing the event id
            event[[j]] <- i
            #print("LALA")
            print("New eventCC")
            previous_event_characters <- c(currentSubject,currentObject)
          }
          else{
            if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
              if(length(setdiff(currentObject,pronounList))==0){
                event[[j]] <- c(event[[j]],i)
                print("Same Event")
                previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
              }
              else{
                j = j+1 #Increasing the event id
                event[[j]] <- i
                #print("LALA")
                print("New eventCC")
                previous_event_characters <- c(currentSubject,currentObject)}
            }
            else{
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
          }
        } 
      }
      barplot(as.matrix(export[floor(as.numeric(input$plot_click1$x)),5:12]), 
              main=paste("Categorical Emotion Mapping of Scene",floor(as.numeric(input$plot_click1$x))),
              ylab="Emotion Score",
              xlab="Categorical Emotions")
      
    })
    output$`Plot Generated` <- renderPlot({ 
      bar_plot()
    }) 
    
    
    d_barplot <- eventReactive(input$plot_click1, {
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      
      # kb <- renderText({ 
      #  read.csv(kb_data())
      #})
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
      
      #Removing those rows in which subject and object both are NA
      kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
      
      #Defining Event
      event = list()
      event_sentences = list()
      j=0 #Event Index
      for(i in 1:length(kb$Sentence)){
        #print(i)
        if(length(event) == 0){
          currentObject <- strsplit(
            (kb$Object.Characters[1])," ")
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   strsplit(
            (kb$Subject.Characters[1])," ")
          event[1] <- 1
          print("Event Initialised")
          j=1 #j is the event id
          previous_event_characters <- c(currentSubject,currentObject)
          
        }
        else{
          
          #Search in previous event
          length_Last_Event = length(event[[j]])
          #Storing the sentence id from the last event
          temp <- event[[j]]
          #print(c("temp =", temp))
          
          
          # for(k in 1:length_Last_Event){
          pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
          #New Algo for Scene Segmentation
          currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
          #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
       
          #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
          if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
              (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
            #A new event defined
            j = j+1 #Increasing the event id
            event[[j]] <- i
            #print("LALA")
            print("New eventCC")
            previous_event_characters <- c(currentSubject,currentObject)
          }
          else{
            if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
              if(length(setdiff(currentObject,pronounList))==0){
                event[[j]] <- c(event[[j]],i)
                print("Same Event")
                previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
              }
              else{
                j = j+1 #Increasing the event id
                event[[j]] <- i
                #print("LALA")
                print("New eventCC")
                previous_event_characters <- c(currentSubject,currentObject)}
            }
            else{
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
          }
        } 
      }
      barplot(as.matrix(export[floor(as.numeric(input$plot_click1$x)),5:12]), 
              main=paste("Categorical Emotion Mapping of Scene",floor(as.numeric(input$plot_click1$x))),
              ylab="Emotion Score",
              xlab="Categorical Emotions")
    
      #  floor(as.numeric(input$plot_click1$x))
    
     # amRadar(data.frame(label = c("A", "Z", "E", "R", "T", "AW", "ZW", "EW"),
      #                          Product1 = export[ floor(as.numeric(input$plot_click1$x)),5:12]),legend=TRUE,ylim=c(0,1))
    })
    output$`Dimensional Mapping` <- renderPlot({ 
      d_barplot()
    }) 
    #########################################################################################
    text_emotion2 <- renderText({
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      paste("Categorical Emotion Mapping Values:",
            "  Anger = ", export$anger[floor(as.numeric(input$plot_click2$x))],
            "  Anticipation = ",export$anticipation[floor(as.numeric(input$plot_click2$x))],
            "  Disgust = ",export$disgust[as.integer(floor(as.numeric(input$plot_click2$x)))],
            "  Fear = ",export$fear[floor(as.numeric(input$plot_click2$x))],
            "  Joy = ",export$joy[floor(as.numeric(input$plot_click2$x))],
            "  Sadness = ",export$sadness[floor(as.numeric(input$plot_click2$x))],
            "  Surprise = ",export$surprise[floor(as.numeric(input$plot_click2$x))],
            "  Trust = ",export$trust[floor(as.numeric(input$plot_click2$x))],
            "  Dimensional Emotional Mapping Values",
            "  Valence = ",export$Valence[floor(as.numeric(input$plot_click2$x))],
            "  Arousal = ",export$Arousal[floor(as.numeric(input$plot_click2$x))],
            "  Dominance = ",export$Dominance[floor(as.numeric(input$plot_click2$x))],sep="\n")
    })
    output$`Selected Scene Emotion Analysis2` <- eventReactive(input$plot_click2,{ 
      text_emotion2()
    })
    
    
    #Scene Description
    text_d2 <- renderText({
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      paste("Scene",floor(as.numeric(input$plot_click2$x)),"Description:",export$paragraph[floor(as.numeric(input$plot_click2$x))])
    })
    
    text_sub2 <- renderText( {
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      
      # kb <- renderText({ 
      #  read.csv(kb_data())
      #})
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
      
      #Removing those rows in which subject and object both are NA
      kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
      
      #Defining Event
      event = list()
      event_sentences = list()
      j=0 #Event Index
      for(i in 1:length(kb$Sentence)){
        #print(i)
        if(length(event) == 0){
          currentObject <- strsplit(
            (kb$Object.Characters[1])," ")
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   strsplit(
            (kb$Subject.Characters[1])," ")
          event[1] <- 1
          print("Event Initialised")
          j=1 #j is the event id
          previous_event_characters <- c(currentSubject,currentObject)
          
        }
        else{
          
          #Search in previous event
          length_Last_Event = length(event[[j]])
          #Storing the sentence id from the last event
          temp <- event[[j]]
          #print(c("temp =", temp))
          
          
          # for(k in 1:length_Last_Event){
          pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
          #New Algo for Scene Segmentation
          currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
          #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
          print(i)
          print("Previous Event")
          print(previous_event_characters)
          print("Current Subject")
          print(currentSubject)
          print("Current Object")
          print(currentObject)
          #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
          if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
              (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
            #A new event defined
            j = j+1 #Increasing the event id
            event[[j]] <- i
            #print("LALA")
            print("New eventCC")
            previous_event_characters <- c(currentSubject,currentObject)
          }
          else{
            if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
              if(length(setdiff(currentObject,pronounList))==0){
                event[[j]] <- c(event[[j]],i)
                print("Same Event")
                previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
              }
              else{
                j = j+1 #Increasing the event id
                event[[j]] <- i
                #print("LALA")
                print("New eventCC")
                previous_event_characters <- c(currentSubject,currentObject)}
            }
            else{
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
          }
        } 
      }
      
      scene_kb <- kb[event[[floor(as.numeric(input$plot_click2$x))]][1]:event[[floor(as.numeric(input$plot_click2$x))]][length(event[[floor(as.numeric(input$plot_click2$x))]])],c(1,2,3,4,5),drop=F]
      c("Agents+associated actions:",scene_kb$Actions.with.Subject.Charcater)
    })
    
    
    text_obj2 <- renderText( {
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      
      # kb <- renderText({ 
      #  read.csv(kb_data())
      #})
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
      
      #Removing those rows in which subject and object both are NA
      kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
      
      #Defining Event
      event = list()
      event_sentences = list()
      j=0 #Event Index
      for(i in 1:length(kb$Sentence)){
        #print(i)
        if(length(event) == 0){
          currentObject <- strsplit(
            (kb$Object.Characters[1])," ")
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   strsplit(
            (kb$Subject.Characters[1])," ")
          event[1] <- 1
          print("Event Initialised")
          j=1 #j is the event id
          previous_event_characters <- c(currentSubject,currentObject)
          
        }
        else{
          
          #Search in previous event
          length_Last_Event = length(event[[j]])
          #Storing the sentence id from the last event
          temp <- event[[j]]
          #print(c("temp =", temp))
          
          
          # for(k in 1:length_Last_Event){
          pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
          #New Algo for Scene Segmentation
          currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
          #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
          print(i)
          print("Previous Event")
          print(previous_event_characters)
          print("Current Subject")
          print(currentSubject)
          print("Current Object")
          print(currentObject)
          #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
          if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
              (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
            #A new event defined
            j = j+1 #Increasing the event id
            event[[j]] <- i
            #print("LALA")
            print("New eventCC")
            previous_event_characters <- c(currentSubject,currentObject)
          }
          else{
            if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
              if(length(setdiff(currentObject,pronounList))==0){
                event[[j]] <- c(event[[j]],i)
                print("Same Event")
                previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
              }
              else{
                j = j+1 #Increasing the event id
                event[[j]] <- i
                #print("LALA")
                print("New eventCC")
                previous_event_characters <- c(currentSubject,currentObject)}
            }
            else{
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
          }
        } 
      }
      
      scene_kb <- kb[event[[floor(as.numeric(input$plot_click2$x))]][1]:event[[floor(as.numeric(input$plot_click2$x))]][length(event[[floor(as.numeric(input$plot_click2$x))]])],c(1,2,3,4,5),drop=F]
      c("Patients+associated Actions:",scene_kb$Objects.and.Actions)
    })
    
    
    bar_plot2 <- eventReactive( input$plot_click2,{
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      
      # kb <- renderText({ 
      #  read.csv(kb_data())
      #})
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
      
      #Removing those rows in which subject and object both are NA
      kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
      
      #Defining Event
      event = list()
      event_sentences = list()
      j=0 #Event Index
      for(i in 1:length(kb$Sentence)){
        #print(i)
        if(length(event) == 0){
          currentObject <- strsplit(
            (kb$Object.Characters[1])," ")
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   strsplit(
            (kb$Subject.Characters[1])," ")
          event[1] <- 1
          print("Event Initialised")
          j=1 #j is the event id
          previous_event_characters <- c(currentSubject,currentObject)
          
        }
        else{
          
          #Search in previous event
          length_Last_Event = length(event[[j]])
          #Storing the sentence id from the last event
          temp <- event[[j]]
          #print(c("temp =", temp))
          
          
          # for(k in 1:length_Last_Event){
          pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
          #New Algo for Scene Segmentation
          currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
          #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
          
          #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
          if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
              (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
            #A new event defined
            j = j+1 #Increasing the event id
            event[[j]] <- i
            #print("LALA")
            print("New eventCC")
            previous_event_characters <- c(currentSubject,currentObject)
          }
          else{
            if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
              if(length(setdiff(currentObject,pronounList))==0){
                event[[j]] <- c(event[[j]],i)
                print("Same Event")
                previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
              }
              else{
                j = j+1 #Increasing the event id
                event[[j]] <- i
                #print("LALA")
                print("New eventCC")
                previous_event_characters <- c(currentSubject,currentObject)}
            }
            else{
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
          }
        } 
      }
      barplot(as.matrix(export[floor(as.numeric(input$plot_click2$x)),5:12]), 
              main=paste("Categorical Emotion Mapping of Scene",floor(as.numeric(input$plot_click2$x))),
              ylab="Emotion Score",
              xlab="Categorical Emotions")
      
    })
    output$`Plot Generated2` <- renderPlot({ 
      bar_plot2()
    }) 
    
    
    d_barplot2 <- eventReactive(input$plot_click2, {
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      
      # kb <- renderText({ 
      #  read.csv(kb_data())
      #})
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
      
      #Removing those rows in which subject and object both are NA
      kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
      
      #Defining Event
      event = list()
      event_sentences = list()
      j=0 #Event Index
      for(i in 1:length(kb$Sentence)){
        #print(i)
        if(length(event) == 0){
          currentObject <- strsplit(
            (kb$Object.Characters[1])," ")
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   strsplit(
            (kb$Subject.Characters[1])," ")
          event[1] <- 1
          print("Event Initialised")
          j=1 #j is the event id
          previous_event_characters <- c(currentSubject,currentObject)
          
        }
        else{
          
          #Search in previous event
          length_Last_Event = length(event[[j]])
          #Storing the sentence id from the last event
          temp <- event[[j]]
          #print(c("temp =", temp))
          
          
          # for(k in 1:length_Last_Event){
          pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
          #New Algo for Scene Segmentation
          currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
          #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
          
          #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
          if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
              (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
            #A new event defined
            j = j+1 #Increasing the event id
            event[[j]] <- i
            #print("LALA")
            print("New eventCC")
            previous_event_characters <- c(currentSubject,currentObject)
          }
          else{
            if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
              if(length(setdiff(currentObject,pronounList))==0){
                event[[j]] <- c(event[[j]],i)
                print("Same Event")
                previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
              }
              else{
                j = j+1 #Increasing the event id
                event[[j]] <- i
                #print("LALA")
                print("New eventCC")
                previous_event_characters <- c(currentSubject,currentObject)}
            }
            else{
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
          }
        } 
      }
      barplot(as.matrix(export[floor(as.numeric(input$plot_click2$x)),13:15]), 
              main=paste("Dimensional Emotion Mapping of Scene",floor(as.numeric(input$plot_click2$x))),
              ylab="Emotion Score",
              xlab="Dimensional Emotions")
      
      #  floor(as.numeric(input$plot_click1$x))
      
      # amRadar(data.frame(label = c("A", "Z", "E", "R", "T", "AW", "ZW", "EW"),
      #                          Product1 = export[ floor(as.numeric(input$plot_click1$x)),5:12]),legend=TRUE,ylim=c(0,1))
    })
    output$`Dimensional Mapping2` <- renderPlot({ 
      bar_plot2()
    }) 
    
    ######################################################################################
    text_emotion2 <- renderText({
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      paste("Categorical Emotion Mapping Values:",
            "  Anger = ", export$anger[floor(as.numeric(input$plot_click2$x))],
            "  Anticipation = ",export$anticipation[floor(as.numeric(input$plot_click2$x))],
            "  Disgust = ",export$disgust[as.integer(floor(as.numeric(input$plot_click2$x)))],
            "  Fear = ",export$fear[floor(as.numeric(input$plot_click2$x))],
            "  Joy = ",export$joy[floor(as.numeric(input$plot_click2$x))],
            "  Sadness = ",export$sadness[floor(as.numeric(input$plot_click2$x))],
            "  Surprise = ",export$surprise[floor(as.numeric(input$plot_click2$x))],
            "  Trust = ",export$trust[floor(as.numeric(input$plot_click2$x))],
            "  Dimensional Emotional Mapping Values",
            "  Valence = ",export$Valence[floor(as.numeric(input$plot_click2$x))],
            "  Arousal = ",export$Arousal[floor(as.numeric(input$plot_click2$x))],
            "  Dominance = ",export$Dominance[floor(as.numeric(input$plot_click2$x))],sep="\n")
    })
    output$`Selected Scene Emotion Analysis2` <- eventReactive(input$plot_click2,{ 
      text_emotion2()
    })
    
    
    #Scene Description
    text_d2 <- renderText({
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      paste("Scene",floor(as.numeric(input$plot_click2$x)),"Description:",export$paragraph[floor(as.numeric(input$plot_click2$x))])
    })
    
    text_sub2 <- renderText( {
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      
      # kb <- renderText({ 
      #  read.csv(kb_data())
      #})
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
      
      #Removing those rows in which subject and object both are NA
      kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
      
      #Defining Event
      event = list()
      event_sentences = list()
      j=0 #Event Index
      for(i in 1:length(kb$Sentence)){
        #print(i)
        if(length(event) == 0){
          currentObject <- strsplit(
            (kb$Object.Characters[1])," ")
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   strsplit(
            (kb$Subject.Characters[1])," ")
          event[1] <- 1
          print("Event Initialised")
          j=1 #j is the event id
          previous_event_characters <- c(currentSubject,currentObject)
          
        }
        else{
          
          #Search in previous event
          length_Last_Event = length(event[[j]])
          #Storing the sentence id from the last event
          temp <- event[[j]]
          #print(c("temp =", temp))
          
          
          # for(k in 1:length_Last_Event){
          pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
          #New Algo for Scene Segmentation
          currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
          #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
          print(i)
          print("Previous Event")
          print(previous_event_characters)
          print("Current Subject")
          print(currentSubject)
          print("Current Object")
          print(currentObject)
          #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
          if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
              (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
            #A new event defined
            j = j+1 #Increasing the event id
            event[[j]] <- i
            #print("LALA")
            print("New eventCC")
            previous_event_characters <- c(currentSubject,currentObject)
          }
          else{
            if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
              if(length(setdiff(currentObject,pronounList))==0){
                event[[j]] <- c(event[[j]],i)
                print("Same Event")
                previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
              }
              else{
                j = j+1 #Increasing the event id
                event[[j]] <- i
                #print("LALA")
                print("New eventCC")
                previous_event_characters <- c(currentSubject,currentObject)}
            }
            else{
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
          }
        } 
      }
      
      scene_kb <- kb[event[[floor(as.numeric(input$plot_click2$x))]][1]:event[[floor(as.numeric(input$plot_click2$x))]][length(event[[floor(as.numeric(input$plot_click2$x))]])],c(1,2,3,4,5),drop=F]
      c("Agents+associated actions:",scene_kb$Actions.with.Subject.Charcater)
    })
    
    
    text_obj2 <- renderText( {
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      
      # kb <- renderText({ 
      #  read.csv(kb_data())
      #})
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
      
      #Removing those rows in which subject and object both are NA
      kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
      
      #Defining Event
      event = list()
      event_sentences = list()
      j=0 #Event Index
      for(i in 1:length(kb$Sentence)){
        #print(i)
        if(length(event) == 0){
          currentObject <- strsplit(
            (kb$Object.Characters[1])," ")
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   strsplit(
            (kb$Subject.Characters[1])," ")
          event[1] <- 1
          print("Event Initialised")
          j=1 #j is the event id
          previous_event_characters <- c(currentSubject,currentObject)
          
        }
        else{
          
          #Search in previous event
          length_Last_Event = length(event[[j]])
          #Storing the sentence id from the last event
          temp <- event[[j]]
          #print(c("temp =", temp))
          
          
          # for(k in 1:length_Last_Event){
          pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
          #New Algo for Scene Segmentation
          currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
          #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
          print(i)
          print("Previous Event")
          print(previous_event_characters)
          print("Current Subject")
          print(currentSubject)
          print("Current Object")
          print(currentObject)
          #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
          if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
              (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
            #A new event defined
            j = j+1 #Increasing the event id
            event[[j]] <- i
            #print("LALA")
            print("New eventCC")
            previous_event_characters <- c(currentSubject,currentObject)
          }
          else{
            if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
              if(length(setdiff(currentObject,pronounList))==0){
                event[[j]] <- c(event[[j]],i)
                print("Same Event")
                previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
              }
              else{
                j = j+1 #Increasing the event id
                event[[j]] <- i
                #print("LALA")
                print("New eventCC")
                previous_event_characters <- c(currentSubject,currentObject)}
            }
            else{
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
          }
        } 
      }
      
      scene_kb <- kb[event[[floor(as.numeric(input$plot_click2$x))]][1]:event[[floor(as.numeric(input$plot_click2$x))]][length(event[[floor(as.numeric(input$plot_click2$x))]])],c(1,2,3,4,5),drop=F]
      c("Patients+associated Actions:",scene_kb$Objects.and.Actions)
    })
    
    
    bar_plot2 <- eventReactive( input$plot_click2,{
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      
      # kb <- renderText({ 
      #  read.csv(kb_data())
      #})
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
      
      #Removing those rows in which subject and object both are NA
      kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
      
      #Defining Event
      event = list()
      event_sentences = list()
      j=0 #Event Index
      for(i in 1:length(kb$Sentence)){
        #print(i)
        if(length(event) == 0){
          currentObject <- strsplit(
            (kb$Object.Characters[1])," ")
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   strsplit(
            (kb$Subject.Characters[1])," ")
          event[1] <- 1
          print("Event Initialised")
          j=1 #j is the event id
          previous_event_characters <- c(currentSubject,currentObject)
          
        }
        else{
          
          #Search in previous event
          length_Last_Event = length(event[[j]])
          #Storing the sentence id from the last event
          temp <- event[[j]]
          #print(c("temp =", temp))
          
          
          # for(k in 1:length_Last_Event){
          pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
          #New Algo for Scene Segmentation
          currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
          #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
          
          #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
          if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
              (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
            #A new event defined
            j = j+1 #Increasing the event id
            event[[j]] <- i
            #print("LALA")
            print("New eventCC")
            previous_event_characters <- c(currentSubject,currentObject)
          }
          else{
            if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
              if(length(setdiff(currentObject,pronounList))==0){
                event[[j]] <- c(event[[j]],i)
                print("Same Event")
                previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
              }
              else{
                j = j+1 #Increasing the event id
                event[[j]] <- i
                #print("LALA")
                print("New eventCC")
                previous_event_characters <- c(currentSubject,currentObject)}
            }
            else{
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
          }
        } 
      }
      barplot(as.matrix(export[floor(as.numeric(input$plot_click2$x)),13:15]), 
              main=paste("Dimensional Emotion Mapping of Scene",floor(as.numeric(input$plot_click2$x))),
              ylab="Emotion Score",
              xlab="Dimensional Emotions")
      
      
    })
    output$`Plot Generated2` <- renderPlot({ 
      bar_plot2()
    }) 
    
    
    d_barplot2 <- eventReactive(input$plot_click2, {
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      
      # kb <- renderText({ 
      #  read.csv(kb_data())
      #})
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
      
      #Removing those rows in which subject and object both are NA
      kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
      
      #Defining Event
      event = list()
      event_sentences = list()
      j=0 #Event Index
      for(i in 1:length(kb$Sentence)){
        #print(i)
        if(length(event) == 0){
          currentObject <- strsplit(
            (kb$Object.Characters[1])," ")
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   strsplit(
            (kb$Subject.Characters[1])," ")
          event[1] <- 1
          print("Event Initialised")
          j=1 #j is the event id
          previous_event_characters <- c(currentSubject,currentObject)
          
        }
        else{
          
          #Search in previous event
          length_Last_Event = length(event[[j]])
          #Storing the sentence id from the last event
          temp <- event[[j]]
          #print(c("temp =", temp))
          
          
          # for(k in 1:length_Last_Event){
          pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
          #New Algo for Scene Segmentation
          currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
          #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
          
          #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
          if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
              (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
            #A new event defined
            j = j+1 #Increasing the event id
            event[[j]] <- i
            #print("LALA")
            print("New eventCC")
            previous_event_characters <- c(currentSubject,currentObject)
          }
          else{
            if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
              if(length(setdiff(currentObject,pronounList))==0){
                event[[j]] <- c(event[[j]],i)
                print("Same Event")
                previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
              }
              else{
                j = j+1 #Increasing the event id
                event[[j]] <- i
                #print("LALA")
                print("New eventCC")
                previous_event_characters <- c(currentSubject,currentObject)}
            }
            else{
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
          }
        } 
      }
      barplot(as.matrix(export[floor(as.numeric(input$plot_click2$x)),13:15]), 
              main=paste("Dimensional Emotion Mapping of Scene",floor(as.numeric(input$plot_click2$x))),
              ylab="Emotion Score",
              xlab="Dimensional Emotions")
      
      #  floor(as.numeric(input$plot_click1$x))
      
      # amRadar(data.frame(label = c("A", "Z", "E", "R", "T", "AW", "ZW", "EW"),
      #                          Product1 = export[ floor(as.numeric(input$plot_click1$x)),5:12]),legend=TRUE,ylim=c(0,1))
    })
    output$`Dimensional Mapping2` <- renderPlot({ 
      bar_plot2()
      
    }) 
    ##################################################################################################
    text_emotion4 <- renderText({
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      paste("Categorical Emotion Mapping Values:",
            "  Anger = ", export$anger[floor(as.numeric(input$plot_click4$x))],
            "  Anticipation = ",export$anticipation[floor(as.numeric(input$plot_click4$x))],
            "  Disgust = ",export$disgust[as.integer(floor(as.numeric(input$plot_click4$x)))],
            "  Fear = ",export$fear[floor(as.numeric(input$plot_click4$x))],
            "  Joy = ",export$joy[floor(as.numeric(input$plot_click4$x))],
            "  Sadness = ",export$sadness[floor(as.numeric(input$plot_click4$x))],
            "  Surprise = ",export$surprise[floor(as.numeric(input$plot_click4$x))],
            "  Trust = ",export$trust[floor(as.numeric(input$plot_click4$x))],
            "  Dimensional Emotional Mapping Values",
            "  Valence = ",export$Valence[floor(as.numeric(input$plot_click4$x))],
            "  Arousal = ",export$Arousal[floor(as.numeric(input$plot_click4$x))],
            "  Dominance = ",export$Dominance[floor(as.numeric(input$plot_click4$x))],sep="\n")
    })
    output$`Selected Scene Emotion Analysis4` <- eventReactive(input$plot_click4,{ 
      text_emotion4()
    })
    
    
    #Scene Description
    text_d4 <- renderText({
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      paste("Scene",floor(as.numeric(input$plot_click4$x)),"Description:",export$paragraph[floor(as.numeric(input$plot_click4$x))])
    })
    
  
    text_sub4 <- renderText( {
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      
      # kb <- renderText({ 
      #  read.csv(kb_data())
      #})
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
      
      #Removing those rows in which subject and object both are NA
      kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
      
      #Defining Event
      event = list()
      event_sentences = list()
      j=0 #Event Index
      for(i in 1:length(kb$Sentence)){
        #print(i)
        if(length(event) == 0){
          currentObject <- strsplit(
            (kb$Object.Characters[1])," ")
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   strsplit(
            (kb$Subject.Characters[1])," ")
          event[1] <- 1
          print("Event Initialised")
          j=1 #j is the event id
          previous_event_characters <- c(currentSubject,currentObject)
          
        }
        else{
          
          #Search in previous event
          length_Last_Event = length(event[[j]])
          #Storing the sentence id from the last event
          temp <- event[[j]]
          #print(c("temp =", temp))
          
          
          # for(k in 1:length_Last_Event){
          pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
          #New Algo for Scene Segmentation
          currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
          #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
          print(i)
          print("Previous Event")
          print(previous_event_characters)
          print("Current Subject")
          print(currentSubject)
          print("Current Object")
          print(currentObject)
          #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
          if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
              (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
            #A new event defined
            j = j+1 #Increasing the event id
            event[[j]] <- i
            #print("LALA")
            print("New eventCC")
            previous_event_characters <- c(currentSubject,currentObject)
          }
          else{
            if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
              if(length(setdiff(currentObject,pronounList))==0){
                event[[j]] <- c(event[[j]],i)
                print("Same Event")
                previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
              }
              else{
                j = j+1 #Increasing the event id
                event[[j]] <- i
                #print("LALA")
                print("New eventCC")
                previous_event_characters <- c(currentSubject,currentObject)}
            }
            else{
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
          }
        } 
      }
      
      scene_kb <- kb[event[[floor(as.numeric(input$plot_click4$x))]][1]:event[[floor(as.numeric(input$plot_click4$x))]][length(event[[floor(as.numeric(input$plot_click4$x))]])],c(1,2,3,4,5),drop=F]
      c("Agents+associated actions:",scene_kb$Actions.with.Subject.Charcater)
    })
    
    
    text_obj4 <- renderText( {
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      
      # kb <- renderText({ 
      #  read.csv(kb_data())
      #})
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
      
      #Removing those rows in which subject and object both are NA
      kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
      
      #Defining Event
      event = list()
      event_sentences = list()
      j=0 #Event Index
      for(i in 1:length(kb$Sentence)){
        #print(i)
        if(length(event) == 0){
          currentObject <- strsplit(
            (kb$Object.Characters[1])," ")
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   strsplit(
            (kb$Subject.Characters[1])," ")
          event[1] <- 1
          print("Event Initialised")
          j=1 #j is the event id
          previous_event_characters <- c(currentSubject,currentObject)
          
        }
        else{
          
          #Search in previous event
          length_Last_Event = length(event[[j]])
          #Storing the sentence id from the last event
          temp <- event[[j]]
          #print(c("temp =", temp))
          
          
          # for(k in 1:length_Last_Event){
          pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
          #New Algo for Scene Segmentation
          currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
          #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
          print(i)
          print("Previous Event")
          print(previous_event_characters)
          print("Current Subject")
          print(currentSubject)
          print("Current Object")
          print(currentObject)
          #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
          if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
              (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
            #A new event defined
            j = j+1 #Increasing the event id
            event[[j]] <- i
            #print("LALA")
            print("New eventCC")
            previous_event_characters <- c(currentSubject,currentObject)
          }
          else{
            if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
              if(length(setdiff(currentObject,pronounList))==0){
                event[[j]] <- c(event[[j]],i)
                print("Same Event")
                previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
              }
              else{
                j = j+1 #Increasing the event id
                event[[j]] <- i
                #print("LALA")
                print("New eventCC")
                previous_event_characters <- c(currentSubject,currentObject)}
            }
            else{
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
          }
        } 
      }
      
      scene_kb <- kb[event[[floor(as.numeric(input$plot_click4$x))]][1]:event[[floor(as.numeric(input$plot_click4$x))]][length(event[[floor(as.numeric(input$plot_click4$x))]])],c(1,2,3,4,5),drop=F]
      c("Patients+associated Actions:",scene_kb$Objects.and.Actions)
    })
    
    
    bar_plot4 <- eventReactive( input$plot_click4,{
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      
      # kb <- renderText({ 
      #  read.csv(kb_data())
      #})
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
      
      #Removing those rows in which subject and object both are NA
      kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
      
      #Defining Event
      event = list()
      event_sentences = list()
      j=0 #Event Index
      for(i in 1:length(kb$Sentence)){
        #print(i)
        if(length(event) == 0){
          currentObject <- strsplit(
            (kb$Object.Characters[1])," ")
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   strsplit(
            (kb$Subject.Characters[1])," ")
          event[1] <- 1
          print("Event Initialised")
          j=1 #j is the event id
          previous_event_characters <- c(currentSubject,currentObject)
          
        }
        else{
          
          #Search in previous event
          length_Last_Event = length(event[[j]])
          #Storing the sentence id from the last event
          temp <- event[[j]]
          #print(c("temp =", temp))
          
          
          # for(k in 1:length_Last_Event){
          pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
          #New Algo for Scene Segmentation
          currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
          #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
          
          #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
          if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
              (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
            #A new event defined
            j = j+1 #Increasing the event id
            event[[j]] <- i
            #print("LALA")
            print("New eventCC")
            previous_event_characters <- c(currentSubject,currentObject)
          }
          else{
            if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
              if(length(setdiff(currentObject,pronounList))==0){
                event[[j]] <- c(event[[j]],i)
                print("Same Event")
                previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
              }
              else{
                j = j+1 #Increasing the event id
                event[[j]] <- i
                #print("LALA")
                print("New eventCC")
                previous_event_characters <- c(currentSubject,currentObject)}
            }
            else{
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
          }
        } 
      }
      barplot(as.matrix(export[floor(as.numeric(input$plot_click4$x)),13:15]), 
              main=paste("Dimensional Emotion Mapping of Scene",floor(as.numeric(input$plot_click4$x))),
              ylab="Emotion Score",
              xlab="Dimensional Emotions")
      
    })
    output$`Plot Generated4` <- renderPlot({ 
      bar_plot4()
    }) 
    
    
    d_barplot4 <- eventReactive(input$plot_click4, {
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      
      # kb <- renderText({ 
      #  read.csv(kb_data())
      #})
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
      
      #Removing those rows in which subject and object both are NA
      kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
      
      #Defining Event
      event = list()
      event_sentences = list()
      j=0 #Event Index
      for(i in 1:length(kb$Sentence)){
        #print(i)
        if(length(event) == 0){
          currentObject <- strsplit(
            (kb$Object.Characters[1])," ")
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   strsplit(
            (kb$Subject.Characters[1])," ")
          event[1] <- 1
          print("Event Initialised")
          j=1 #j is the event id
          previous_event_characters <- c(currentSubject,currentObject)
          
        }
        else{
          
          #Search in previous event
          length_Last_Event = length(event[[j]])
          #Storing the sentence id from the last event
          temp <- event[[j]]
          #print(c("temp =", temp))
          
          
          # for(k in 1:length_Last_Event){
          pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
          #New Algo for Scene Segmentation
          currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
          #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
          
          #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
          if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
              (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
            #A new event defined
            j = j+1 #Increasing the event id
            event[[j]] <- i
            #print("LALA")
            print("New eventCC")
            previous_event_characters <- c(currentSubject,currentObject)
          }
          else{
            if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
              if(length(setdiff(currentObject,pronounList))==0){
                event[[j]] <- c(event[[j]],i)
                print("Same Event")
                previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
              }
              else{
                j = j+1 #Increasing the event id
                event[[j]] <- i
                #print("LALA")
                print("New eventCC")
                previous_event_characters <- c(currentSubject,currentObject)}
            }
            else{
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
          }
        } 
      }
      barplot(as.matrix(export[floor(as.numeric(input$plot_click4$x)),13:15]), 
              main=paste("Dimensional Emotion Mapping of Scene",floor(as.numeric(input$plot_click4$x))),
              ylab="Emotion Score",
              xlab="Dimensional Emotions")
      
      #  floor(as.numeric(input$plot_click1$x))
      
      # amRadar(data.frame(label = c("A", "Z", "E", "R", "T", "AW", "ZW", "EW"),
      #                          Product1 = export[ floor(as.numeric(input$plot_click1$x)),5:12]),legend=TRUE,ylim=c(0,1))
    })
    output$`Dimensional Mapping4` <- renderPlot({ 
      bar_plot4()
    }) 
    ######################################################################################
    text_emotion3 <- renderText({
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      paste("Categorical Emotion Mapping Values:",
            "  Anger = ", export$anger[floor(as.numeric(input$plot_click3$x))],
            "  Anticipation = ",export$anticipation[floor(as.numeric(input$plot_click3$x))],
            "  Disgust = ",export$disgust[as.integer(floor(as.numeric(input$plot_click3$x)))],
            "  Fear = ",export$fear[floor(as.numeric(input$plot_click3$x))],
            "  Joy = ",export$joy[floor(as.numeric(input$plot_click3$x))],
            "  Sadness = ",export$sadness[floor(as.numeric(input$plot_click3$x))],
            "  Surprise = ",export$surprise[floor(as.numeric(input$plot_click3$x))],
            "  Trust = ",export$trust[floor(as.numeric(input$plot_click3$x))],
            "  Dimensional Emotional Mapping Values",
            "  Valence = ",export$Valence[floor(as.numeric(input$plot_click3$x))],
            "  Arousal = ",export$Arousal[floor(as.numeric(input$plot_click3$x))],
            "  Dominance = ",export$Dominance[floor(as.numeric(input$plot_click3$x))],sep="\n")
    })
    output$`Selected Scene Emotion Analysis3` <- eventReactive(input$plot_click3,{ 
      text_emotion3()
    })
    
    
    #Scene Description
    text_d3 <- renderText({
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      paste("Scene",floor(as.numeric(input$plot_click3$x)),"Description:",export$paragraph[floor(as.numeric(input$plot_click3$x))])
    })
    
    
    text_sub3 <- renderText( {
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      
      # kb <- renderText({ 
      #  read.csv(kb_data())
      #})
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
      
      #Removing those rows in which subject and object both are NA
      kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
      
      #Defining Event
      event = list()
      event_sentences = list()
      j=0 #Event Index
      for(i in 1:length(kb$Sentence)){
        #print(i)
        if(length(event) == 0){
          currentObject <- strsplit(
            (kb$Object.Characters[1])," ")
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   strsplit(
            (kb$Subject.Characters[1])," ")
          event[1] <- 1
          print("Event Initialised")
          j=1 #j is the event id
          previous_event_characters <- c(currentSubject,currentObject)
          
        }
        else{
          
          #Search in previous event
          length_Last_Event = length(event[[j]])
          #Storing the sentence id from the last event
          temp <- event[[j]]
          #print(c("temp =", temp))
          
          
          # for(k in 1:length_Last_Event){
          pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
          #New Algo for Scene Segmentation
          currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
          #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
          print(i)
          print("Previous Event")
          print(previous_event_characters)
          print("Current Subject")
          print(currentSubject)
          print("Current Object")
          print(currentObject)
          #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
          if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
              (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
            #A new event defined
            j = j+1 #Increasing the event id
            event[[j]] <- i
            #print("LALA")
            print("New eventCC")
            previous_event_characters <- c(currentSubject,currentObject)
          }
          else{
            if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
              if(length(setdiff(currentObject,pronounList))==0){
                event[[j]] <- c(event[[j]],i)
                print("Same Event")
                previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
              }
              else{
                j = j+1 #Increasing the event id
                event[[j]] <- i
                #print("LALA")
                print("New eventCC")
                previous_event_characters <- c(currentSubject,currentObject)}
            }
            else{
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
          }
        } 
      }
      
      scene_kb <- kb[event[[floor(as.numeric(input$plot_click3$x))]][1]:event[[floor(as.numeric(input$plot_click3$x))]][length(event[[floor(as.numeric(input$plot_click3$x))]])],c(1,2,3,4,5),drop=F]
      c("Agents+associated actions:",scene_kb$Actions.with.Subject.Charcater)
    })
    
    
    text_obj3 <- renderText( {
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      
      # kb <- renderText({ 
      #  read.csv(kb_data())
      #})
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
      
      #Removing those rows in which subject and object both are NA
      kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
      
      #Defining Event
      event = list()
      event_sentences = list()
      j=0 #Event Index
      for(i in 1:length(kb$Sentence)){
        #print(i)
        if(length(event) == 0){
          currentObject <- strsplit(
            (kb$Object.Characters[1])," ")
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   strsplit(
            (kb$Subject.Characters[1])," ")
          event[1] <- 1
          print("Event Initialised")
          j=1 #j is the event id
          previous_event_characters <- c(currentSubject,currentObject)
          
        }
        else{
          
          #Search in previous event
          length_Last_Event = length(event[[j]])
          #Storing the sentence id from the last event
          temp <- event[[j]]
          #print(c("temp =", temp))
          
          
          # for(k in 1:length_Last_Event){
          pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
          #New Algo for Scene Segmentation
          currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
          #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
          print(i)
          print("Previous Event")
          print(previous_event_characters)
          print("Current Subject")
          print(currentSubject)
          print("Current Object")
          print(currentObject)
          #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
          if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
              (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
            #A new event defined
            j = j+1 #Increasing the event id
            event[[j]] <- i
            #print("LALA")
            print("New eventCC")
            previous_event_characters <- c(currentSubject,currentObject)
          }
          else{
            if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
              if(length(setdiff(currentObject,pronounList))==0){
                event[[j]] <- c(event[[j]],i)
                print("Same Event")
                previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
              }
              else{
                j = j+1 #Increasing the event id
                event[[j]] <- i
                #print("LALA")
                print("New eventCC")
                previous_event_characters <- c(currentSubject,currentObject)}
            }
            else{
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
          }
        } 
      }
      
      scene_kb <- kb[event[[floor(as.numeric(input$plot_click3$x))]][1]:event[[floor(as.numeric(input$plot_click3$x))]][length(event[[floor(as.numeric(input$plot_click3$x))]])],c(1,2,3,4,5),drop=F]
      c("Patients+associated Actions:",scene_kb$Objects.and.Actions)
    })
    
    
    bar_plot3 <- eventReactive( input$plot_click3,{
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      
      # kb <- renderText({ 
      #  read.csv(kb_data())
      #})
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
      
      #Removing those rows in which subject and object both are NA
      kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
      
      #Defining Event
      event = list()
      event_sentences = list()
      j=0 #Event Index
      for(i in 1:length(kb$Sentence)){
        #print(i)
        if(length(event) == 0){
          currentObject <- strsplit(
            (kb$Object.Characters[1])," ")
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   strsplit(
            (kb$Subject.Characters[1])," ")
          event[1] <- 1
          print("Event Initialised")
          j=1 #j is the event id
          previous_event_characters <- c(currentSubject,currentObject)
          
        }
        else{
          
          #Search in previous event
          length_Last_Event = length(event[[j]])
          #Storing the sentence id from the last event
          temp <- event[[j]]
          #print(c("temp =", temp))
          
          
          # for(k in 1:length_Last_Event){
          pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
          #New Algo for Scene Segmentation
          currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
          #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
          
          #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
          if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
              (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
            #A new event defined
            j = j+1 #Increasing the event id
            event[[j]] <- i
            #print("LALA")
            print("New eventCC")
            previous_event_characters <- c(currentSubject,currentObject)
          }
          else{
            if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
              if(length(setdiff(currentObject,pronounList))==0){
                event[[j]] <- c(event[[j]],i)
                print("Same Event")
                previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
              }
              else{
                j = j+1 #Increasing the event id
                event[[j]] <- i
                #print("LALA")
                print("New eventCC")
                previous_event_characters <- c(currentSubject,currentObject)}
            }
            else{
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
          }
        } 
      }
      barplot(as.matrix(export[floor(as.numeric(input$plot_click3$x)),5:12]), 
              main=paste("Categorical Emotion Mapping of Scene",floor(as.numeric(input$plot_click3$x))),
              ylab="Emotion Score",
              xlab="Categorical Emotions")
      
    })
    output$`Plot Generated3` <- renderPlot({ 
      bar_plot3()
    })
      
      text_emotion3 <- renderText({
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      paste("Categorical Emotion Mapping Values:",
            "  Anger = ", export$anger[floor(as.numeric(input$plot_click3$x))],
            "  Anticipation = ",export$anticipation[floor(as.numeric(input$plot_click3$x))],
            "  Disgust = ",export$disgust[as.integer(floor(as.numeric(input$plot_click3$x)))],
            "  Fear = ",export$fear[floor(as.numeric(input$plot_click3$x))],
            "  Joy = ",export$joy[floor(as.numeric(input$plot_click3$x))],
            "  Sadness = ",export$sadness[floor(as.numeric(input$plot_click3$x))],
            "  Surprise = ",export$surprise[floor(as.numeric(input$plot_click3$x))],
            "  Trust = ",export$trust[floor(as.numeric(input$plot_click3$x))],
            "  Dimensional Emotional Mapping Values",
            "  Valence = ",export$Valence[floor(as.numeric(input$plot_click3$x))],
            "  Arousal = ",export$Arousal[floor(as.numeric(input$plot_click3$x))],
            "  Dominance = ",export$Dominance[floor(as.numeric(input$plot_click3$x))],sep="\n")
    })
    output$`Selected Scene Emotion Analysis3` <- eventReactive(input$plot_click3,{ 
      text_emotion3()
    })
    
    
    #Scene Description
    text_d3 <- renderText({
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      paste("Scene",floor(as.numeric(input$plot_click3$x)),"Description:",export$paragraph[floor(as.numeric(input$plot_click3$x))])
    })
    
    
    text_sub3 <- renderText( {
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      
      # kb <- renderText({ 
      #  read.csv(kb_data())
      #})
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
      
      #Removing those rows in which subject and object both are NA
      kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
      
      #Defining Event
      event = list()
      event_sentences = list()
      j=0 #Event Index
      for(i in 1:length(kb$Sentence)){
        #print(i)
        if(length(event) == 0){
          currentObject <- strsplit(
            (kb$Object.Characters[1])," ")
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   strsplit(
            (kb$Subject.Characters[1])," ")
          event[1] <- 1
          print("Event Initialised")
          j=1 #j is the event id
          previous_event_characters <- c(currentSubject,currentObject)
          
        }
        else{
          
          #Search in previous event
          length_Last_Event = length(event[[j]])
          #Storing the sentence id from the last event
          temp <- event[[j]]
          #print(c("temp =", temp))
          
          
          # for(k in 1:length_Last_Event){
          pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
          #New Algo for Scene Segmentation
          currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
          #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
          print(i)
          print("Previous Event")
          print(previous_event_characters)
          print("Current Subject")
          print(currentSubject)
          print("Current Object")
          print(currentObject)
          #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
          if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
              (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
            #A new event defined
            j = j+1 #Increasing the event id
            event[[j]] <- i
            #print("LALA")
            print("New eventCC")
            previous_event_characters <- c(currentSubject,currentObject)
          }
          else{
            if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
              if(length(setdiff(currentObject,pronounList))==0){
                event[[j]] <- c(event[[j]],i)
                print("Same Event")
                previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
              }
              else{
                j = j+1 #Increasing the event id
                event[[j]] <- i
                #print("LALA")
                print("New eventCC")
                previous_event_characters <- c(currentSubject,currentObject)}
            }
            else{
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
          }
        } 
      }
      
      scene_kb <- kb[event[[floor(as.numeric(input$plot_click3$x))]][1]:event[[floor(as.numeric(input$plot_click3$x))]][length(event[[floor(as.numeric(input$plot_click3$x))]])],c(1,2,3,4,5),drop=F]
      c("Agents+associated actions:",scene_kb$Actions.with.Subject.Charcater)
    })
    
    
    text_obj3 <- renderText( {
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      
      # kb <- renderText({ 
      #  read.csv(kb_data())
      #})
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
      
      #Removing those rows in which subject and object both are NA
      kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
      
      #Defining Event
      event = list()
      event_sentences = list()
      j=0 #Event Index
      for(i in 1:length(kb$Sentence)){
        #print(i)
        if(length(event) == 0){
          currentObject <- strsplit(
            (kb$Object.Characters[1])," ")
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   strsplit(
            (kb$Subject.Characters[1])," ")
          event[1] <- 1
          print("Event Initialised")
          j=1 #j is the event id
          previous_event_characters <- c(currentSubject,currentObject)
          
        }
        else{
          
          #Search in previous event
          length_Last_Event = length(event[[j]])
          #Storing the sentence id from the last event
          temp <- event[[j]]
          #print(c("temp =", temp))
          
          
          # for(k in 1:length_Last_Event){
          pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
          #New Algo for Scene Segmentation
          currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
          #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
          print(i)
          print("Previous Event")
          print(previous_event_characters)
          print("Current Subject")
          print(currentSubject)
          print("Current Object")
          print(currentObject)
          #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
          if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
              (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
            #A new event defined
            j = j+1 #Increasing the event id
            event[[j]] <- i
            #print("LALA")
            print("New eventCC")
            previous_event_characters <- c(currentSubject,currentObject)
          }
          else{
            if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
              if(length(setdiff(currentObject,pronounList))==0){
                event[[j]] <- c(event[[j]],i)
                print("Same Event")
                previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
              }
              else{
                j = j+1 #Increasing the event id
                event[[j]] <- i
                #print("LALA")
                print("New eventCC")
                previous_event_characters <- c(currentSubject,currentObject)}
            }
            else{
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
          }
        } 
      }
      
      scene_kb <- kb[event[[floor(as.numeric(input$plot_click3$x))]][1]:event[[floor(as.numeric(input$plot_click3$x))]][length(event[[floor(as.numeric(input$plot_click3$x))]])],c(1,2,3,4,5),drop=F]
      c("Patients+associated Actions:",scene_kb$Objects.and.Actions)
    })
    
    
    bar_plot3 <- eventReactive( input$plot_click3,{
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      
      # kb <- renderText({ 
      #  read.csv(kb_data())
      #})
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
      
      #Removing those rows in which subject and object both are NA
      kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
      
      #Defining Event
      event = list()
      event_sentences = list()
      j=0 #Event Index
      for(i in 1:length(kb$Sentence)){
        #print(i)
        if(length(event) == 0){
          currentObject <- strsplit(
            (kb$Object.Characters[1])," ")
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   strsplit(
            (kb$Subject.Characters[1])," ")
          event[1] <- 1
          print("Event Initialised")
          j=1 #j is the event id
          previous_event_characters <- c(currentSubject,currentObject)
          
        }
        else{
          
          #Search in previous event
          length_Last_Event = length(event[[j]])
          #Storing the sentence id from the last event
          temp <- event[[j]]
          #print(c("temp =", temp))
          
          
          # for(k in 1:length_Last_Event){
          pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
          #New Algo for Scene Segmentation
          currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
          #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
          
          #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
          if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
              (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
            #A new event defined
            j = j+1 #Increasing the event id
            event[[j]] <- i
            #print("LALA")
            print("New eventCC")
            previous_event_characters <- c(currentSubject,currentObject)
          }
          else{
            if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
              if(length(setdiff(currentObject,pronounList))==0){
                event[[j]] <- c(event[[j]],i)
                print("Same Event")
                previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
              }
              else{
                j = j+1 #Increasing the event id
                event[[j]] <- i
                #print("LALA")
                print("New eventCC")
                previous_event_characters <- c(currentSubject,currentObject)}
            }
            else{
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
          }
        } 
      }
      barplot(as.matrix(export[floor(as.numeric(input$plot_click3$x)),5:12]), 
              main=paste("Categorical Emotion Mapping of Scene",floor(as.numeric(input$plot_click3$x))),
              ylab="Emotion Score",
              xlab="Categorical Emotions")
      
    })
    output$`Plot Generated3` <- renderPlot({ 
      bar_plot3()
    }) 
    
    
    d_barplot3 <- eventReactive(input$plot_click3, {
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      
      # kb <- renderText({ 
      #  read.csv(kb_data())
      #})
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
      
      #Removing those rows in which subject and object both are NA
      kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
      
      #Defining Event
      event = list()
      event_sentences = list()
      j=0 #Event Index
      for(i in 1:length(kb$Sentence)){
        #print(i)
        if(length(event) == 0){
          currentObject <- strsplit(
            (kb$Object.Characters[1])," ")
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   strsplit(
            (kb$Subject.Characters[1])," ")
          event[1] <- 1
          print("Event Initialised")
          j=1 #j is the event id
          previous_event_characters <- c(currentSubject,currentObject)
          
        }
        else{
          
          #Search in previous event
          length_Last_Event = length(event[[j]])
          #Storing the sentence id from the last event
          temp <- event[[j]]
          #print(c("temp =", temp))
          
          
          # for(k in 1:length_Last_Event){
          pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
          #New Algo for Scene Segmentation
          currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
          #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
          
          #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
          if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
              (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
            #A new event defined
            j = j+1 #Increasing the event id
            event[[j]] <- i
            #print("LALA")
            print("New eventCC")
            previous_event_characters <- c(currentSubject,currentObject)
          }
          else{
            if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
              if(length(setdiff(currentObject,pronounList))==0){
                event[[j]] <- c(event[[j]],i)
                print("Same Event")
                previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
              }
              else{
                j = j+1 #Increasing the event id
                event[[j]] <- i
                #print("LALA")
                print("New eventCC")
                previous_event_characters <- c(currentSubject,currentObject)}
            }
            else{
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
          }
        } 
      }
      barplot(as.matrix(export[floor(as.numeric(input$plot_click3$x)),5:12]), 
              main=paste("Categorical Emotion Mapping of Scene",floor(as.numeric(input$plot_click3$x))),
              ylab="Emotion Score",
              xlab="Categorical Emotions")
      
      #  floor(as.numeric(input$plot_click1$x))
      
      amRadar(data.frame(label = c("A", "Z", "E", "R", "T", "AW", "ZW", "EW"),
                                Product1 = export[ floor(as.numeric(input$plot_click3$x)),5:12]),legend=TRUE,ylim=c(0,1))
    })
  
    output$`Dimensional Mapping3` <- renderPlot({ 
     
      bar_plot3()
    }) 
    
    
    d_barplot3 <- eventReactive(input$plot_click3, {
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      
      
      
      # kb <- renderText({ 
      #  read.csv(kb_data())
      #})
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
      
      #Removing those rows in which subject and object both are NA
      kb <- subset(kb, !(grepl("NA",kb[[2]]) & grepl("NA",kb[[4]])))
      
      #Defining Event
      event = list()
      event_sentences = list()
      j=0 #Event Index
      for(i in 1:length(kb$Sentence)){
        #print(i)
        if(length(event) == 0){
          currentObject <- strsplit(
            (kb$Object.Characters[1])," ")
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   strsplit(
            (kb$Subject.Characters[1])," ")
          event[1] <- 1
          print("Event Initialised")
          j=1 #j is the event id
          previous_event_characters <- c(currentSubject,currentObject)
          
        }
        else{
          
          #Search in previous event
          length_Last_Event = length(event[[j]])
          #Storing the sentence id from the last event
          temp <- event[[j]]
          #print(c("temp =", temp))
          
          
          # for(k in 1:length_Last_Event){
          pronounList <- c("he","He","She","she","it","It","They","they","We","we","I","You","you","him","Him","Her","her","Them","them","Us","us","Me","me")
          #New Algo for Scene Segmentation
          currentObject <- unique(strsplit(kb$Object.Characters[i]," ")[[1:length(strsplit(kb$Object.Characters[i]," ")[1])]])
          # currentObject<- currentObject[[1:length(strsplit((kb$Object.Characters[k])," ")[1])]]
          
          currentSubject <-   unique(strsplit(kb$Subject.Characters[i]," ")[[1:length(strsplit(kb$Subject.Characters[i]," ")[1])]])
          #currentSubject<- currentSubject[[1:length(strsplit((kb$Subject.Characters[k])," ")[1])]]
          
          #  if((length(Reduce(intersect,list(currentObject,previous_event_characters)))==0) & (length(currentObject)>0)){
          if(((length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0) &
              (length(intersect(strsplit(unlist(currentSubject)," "),strsplit(unlist(previous_event_characters)," ")))==0))){
            #A new event defined
            j = j+1 #Increasing the event id
            event[[j]] <- i
            #print("LALA")
            print("New eventCC")
            previous_event_characters <- c(currentSubject,currentObject)
          }
          else{
            if(length(intersect(strsplit(unlist(currentObject)," "),strsplit(unlist(previous_event_characters)," ")))==0){
              if(length(setdiff(currentObject,pronounList))==0){
                event[[j]] <- c(event[[j]],i)
                print("Same Event")
                previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
              }
              else{
                j = j+1 #Increasing the event id
                event[[j]] <- i
                #print("LALA")
                print("New eventCC")
                previous_event_characters <- c(currentSubject,currentObject)}
            }
            else{
              event[[j]] <- c(event[[j]],i)
              print("Same Event")
              previous_event_characters <- c(previous_event_characters,currentSubject,currentObject)
            }
          }
        } 
      }
      barplot(as.matrix(export[floor(as.numeric(input$plot_click3$x)),5:12]), 
              main=paste("Categorical Emotion Mapping of Scene",floor(as.numeric(input$plot_click3$x))),
              ylab="Emotion Score",
              xlab="Categorical Emotions")
      
      #  floor(as.numeric(input$plot_click1$x))
      
      # amRadar(data.frame(label = c("A", "Z", "E", "R", "T", "AW", "ZW", "EW"),
      #                          Product1 = export[ floor(as.numeric(input$plot_click1$x)),5:12]),legend=TRUE,ylim=c(0,1))
    })
    output$`Dimensional Mapping3` <- renderPlot({ 
      d_barplot3()
    })
    ######################################################################################
    output$info1 <- renderText({
      paste0("Please wait for atleast 5 seconds untill the plot is generated. Click on the plot to get the scene emotion analysis.")
    })
    output$info2 <- renderText({
      paste0("Please wait for atleast 5 seconds untill the plot is generated. Click on the plot to get the scene emotion analysis.")
    })
    output$info3 <- renderText({
      paste0("Please wait for atleast 5 seconds untill the plot is generated. Click on the plot to get the scene emotion analysis.")
    })
    output$info4 <- renderText({
      paste0("Please wait for atleast 5 seconds untill the plot is generated. Click on the plot to get the scene emotion analysis.")
    })
    text_scene_generated <- eventReactive(input$x, {
      if(input$x=='Harry Potter and the Philosopher\'s Stone'){
        kb = read.csv("storygraph_v1.csv")
        export = as.data.frame(read.csv("event_distribution1.csv"))
      }
      
      else if(input$x=='Harry Potter and the Chamber of Secrets'){
        kb = read.csv("storygraph_v2.csv")
        export = as.data.frame(read.csv("event_distribution2.csv"))
      }
      else if(input$x=='Harry Potter and the Prisoner of Azkaban'){
        kb = read.csv("storygraph_v3.csv")
        export = as.data.frame(read.csv("event_distribution3.csv"))
      }
      else if(input$x=='Harry Potter and the Goblet of Fire'){
        kb = read.csv("storygraph_v4.csv")
        export = as.data.frame(read.csv("event_distribution4.csv"))
      }
      else if(input$x=='Harry Potter and the Order of the Phoenix'){
        kb = read.csv("storygraph_v5.csv")
        export = as.data.frame(read.csv("event_distribution5.csv"))
      }
      else if(input$x=='Harry Potter and the Half-Blood Prince'){
        kb = read.csv("storygraph_v6.csv")
        export = as.data.frame(read.csv("event_distribution6.csv"))
      }
      else{
        kb = read.csv("storygraph_v7.csv")
        export = as.data.frame(read.csv("event_distribution7.csv"))
      }
      paste("Total number of scenes in this story are", length(export$paragraph))
    })
    output$`Heading for Scene Info` <- eventReactive(input$plot_click1$x,{
      paste("Clicked Scene Analysis")
    })
    output$`Total Number of Scenes in the Story` <- eventReactive(input$plot_click1$x,{ 
      text_scene_generated()
    })
    output$`Selected Scene ID` <- eventReactive(input$plot_click1$x,{
      paste("Scene #",floor(as.numeric(input$plot_click1$x)))
    })
    output$`Selected Scene ID2` <- eventReactive(input$plot_click2$x,{
      paste("Scene #",floor(as.numeric(input$plot_click2$x)))
    })
    output$`Selected Scene ID3` <- eventReactive(input$plot_click3$x,{
      paste("Scene #",floor(as.numeric(input$plot_click3$x)))
    })
    output$`Selected Scene ID4` <- eventReactive(input$plot_click4$x,{
      paste("Scene #",floor(as.numeric(input$plot_click4$x)))
    })
    output$`Selected Scene Description` <- eventReactive(input$plot_click1$x,{
      #paste("Description of Scene:",floor(as.numeric(input$plot_click1$x)))
      text_d()
    })
    output$`Selected Scene Description2` <- eventReactive(input$plot_click2$x,{
      #paste("Description of Scene:",floor(as.numeric(input$plot_click1$x)))
      text_d2()
    })
    output$`Selected Scene Description3` <- eventReactive(input$plot_click3$x,{
      #paste("Description of Scene:",floor(as.numeric(input$plot_click1$x)))
      text_d3()
    })
    output$`Selected Scene Description4` <- eventReactive(input$plot_click4$x,{
      #paste("Description of Scene:",floor(as.numeric(input$plot_click1$x)))
      text_d4()
    })
    output$`Selected Scene Agents and Actions` <- eventReactive(input$plot_click1$x,{
      text_sub()
    })
    output$`Selected Scene Agents and Actions2` <- eventReactive(input$plot_click2$x,{
      text_sub2()
    })
    output$`Selected Scene Agents and Actions3` <- eventReactive(input$plot_click3$x,{
      text_sub3()
    })
    output$`Selected Scene Agents and Actions4` <- eventReactive(input$plot_click4$x,{
      text_sub4()
    })
    output$`Selected Scene Objects and Actions` <- eventReactive(input$plot_click1$x,{
      text_obj()
    })
    output$`Selected Scene Objects and Actions2` <- eventReactive(input$plot_click2$x,{
      text_obj2()
    })
    output$`Selected Scene Objects and Actions3` <- eventReactive(input$plot_click3$x,{
      text_obj3()
    })
    output$`Selected Scene Objects and Actions4` <- eventReactive(input$plot_click4$x,{
      text_obj4()
    })
    output$About <- renderUI(
      HTML( 
      paste("We present EMOFIEL, a system that identifies characters and scenes
in a story from a fictional narrative summary, generates appropri-
             ate scene descriptions, identifies the emotion flow between a given
             directed pair of story characters in each interaction, and organizes
             them along the story timeline to populate a knowledge base about
             stories. These emotions are identified using two emotion modelling
             approaches: categorical and dimensional emotion models. The gen-
             erated plots show that in a particular scene, two characters can
             share multiple emotions together with different intensity. Further-
             more, the directionality of the emotion can be captured as well,
             depending on which character is more dominant in each interaction.
             EMOFIEL provides a web-based GUI that allows users to query the
             constructed knowledge base to explore the emotion mapping of
             a given character pair throughout a given story, and to explore
             scenes for which a certain emotion peaks.","
             
             This work is published at WWW 2018 as:

             Harshita Jhavar and Paramita Mirza. 2018. EMOFIEL: Mapping Emotions of
             Relationships in a Story. In WWW ’18 Companion: The 2018 Web Conference
             Companion, April 23–27, 2018, Lyon, France. ACM", sep = "<br/>")
      
            
    )
    
 #output$Copyright <- renderText(
  # paste("© Database and Information Systems Group, Max Planck Institute for Informatics. 2018 ")
 #)  
)  }
