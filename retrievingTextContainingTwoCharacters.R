#Reading the csv file
file <- read.csv("onlyn_grams_of_text.csv",header=TRUE)

#Adding txt extension to the list of different files available
a <- rep(".txt",583)
file$title <- paste(file$title,a,sep="")
write.csv(file$title)
#Creating an empty data frame
x <- data.frame()

#Finding sentences containing both characters line by line
for(i in 1:583){
  #Reading the original text of the book
  para <- read.table(file$title[i], header=T, sep="\n")
 
  #Passing the two characters we are interested in
  toMatch <- paste0("(?=.*", file$character_2[i], ")(?=.*", file$character_1[i], ")")
  
  #Retreiving the sentences from text
  sentences<-unlist(strsplit(para,split="/\r?\n/"))
  foo<-function(Match){c(Match,sentences[grep(Match,sentences,perl = T)])}
  a <- lapply(toMatch,foo)
  append(x,a)
  }
