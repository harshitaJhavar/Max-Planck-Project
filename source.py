#Source: Harry Potter and Philosopher's Stone
import nltk
import re
import matplotlib.pyplot as plt
#Reading the chapter wise summary
file = open('HP_Part1_ChapterwiseSummary_tagged.txt', 'r')

#Keeping only those sentences which have two propernouns in them
target = open('Sentences_With_Only_Character_Names.txt', 'w')
for line in file.read().split('\n'):
    if re.match(r'(.*) (.*Ron_NNP) (.*) (.*Hermione_NNP) (.*)', line) is not None:
        target.write(re.sub(r'(_.*?\s)', " ", line)) #Removing the other part of speech tags for different sentences
        target.write("\n")
 #   elif re.match(r'(.*?) (.*_NNP) (.*?) (.*_PRP) (.*?)', line) is not None:
 #       print(line)
 #   elif re.match(r'(.*?) (.*_PRP) (.*?) (.*_NNP) (.*?)', line) is not None:
 #       print(line)
 #   elif re.match(r'(.*?) (.*_PRP) (.*?) (.*_PRP) (.*?)', line) is not None:
 #       print(line)

target.close()
text = open('Sentences_With_Only_Character_Names.txt', 'r').read()
results_Story_Sentiment = open('Results_Story_Sentiment.txt', 'w')
#Sentiment Analysis
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
analyzer = SentimentIntensityAnalyzer()
sentiment_score = []
for sentence in text.split('\n'):
    print(sentence)
    vs = analyzer.polarity_scores(sentence)
    print(str(vs))
    print(str(vs["compound"]))
    results_Story_Sentiment.write(sentence)
    results_Story_Sentiment.write("\n")
    results_Story_Sentiment.write(str(vs["compound"]))
    sentiment_score.append(str(vs["compound"]))
#Plotting the sentiment
plt.plot(sentiment_score)
plt.show()
