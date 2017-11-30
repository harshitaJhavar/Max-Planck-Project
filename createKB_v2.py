#Making Knowledge Base Out of Resolved Text
import nltk
import os
import matplotlib.pyplot as plt
import matplotlib.image as mpimg
import re
import csv
from nltk.parse.stanford import StanfordDependencyParser
import re
import matplotlib.pyplot as plt
from matplotlib.ticker import FormatStrFormatter
import numpy as np
import simplejson
from wnaffect import *
from emotion import *
from nltk.stem import WordNetLemmatizer
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer

path_to_jar = './stanford-parser-full-2017-06-09/stanford-parser.jar'
path_to_models_jar = './stanford-parser-full-2017-06-09/stanford-parser-3.8.0-models.jar'
dependency_parser = StanfordDependencyParser(path_to_jar=path_to_jar, path_to_models_jar=path_to_models_jar)

#Retrieve sentences
file = open('Harry_Potter_and_the_Chamber_of_Secrets.resolved.txt', 'r')
storygraph = []
with open("storygraph_v2.csv",'w') as resultFile:
    wr = csv.writer(resultFile, dialect='excel')

    wr.writerow(["Sentence","Subject Characters", "Actions with Subject Charcater","Object Characters", "Objects and Actions", "sentence sentiment score","dependency_tree"])
    for sentence in file.read().split('\n'):
        sentence = ''.join([i for i in sentence if not i.isdigit()])
        print(sentence)
        #Dependency Parsing
        result = dependency_parser.raw_parse(sentence)
        dep = result.__next__()
        dependencyList = list(dep.triples())
        lengthDependencyList = len(dependencyList)
        print(dependencyList)
        actionsAssociatedWithObjects = []
        actionsAssociatedWithCharacter1 = []
        actionsAssociatedWithCharacter2 = []
        objects = []

        #Finding Subject Characters
        Character_1 = []

        for i in range(lengthDependencyList):
            if (dependencyList[i][1] == 'nsubj'or dependencyList[i][1] == 'nmod' ):
                if(dependencyList[i][2][1] == 'NNP' or dependencyList[i][2][1] == 'NNS' or dependencyList[i][2][1] == 'NN'):
                    #Removed PRP here
                    Character_1.append(dependencyList[i][2][0])
                    actionsAssociatedWithCharacter1.append(dependencyList[i][2][0] + ":" + dependencyList[i][0][0])

        #Finding Object Characters
        for i in range(lengthDependencyList):
            #Finding Associated objects and their actions
            if (dependencyList[i][1] == 'dobj' or dependencyList[i][1] == 'nsubjpass'):
                if(dependencyList[i][2][1] == 'NNP' or dependencyList[i][2][1] == 'NNS' or dependencyList[i][2][1] == 'NN'):
                    #Removed PRP here
                    actionsAssociatedWithObjects.append(dependencyList[i][2][0] + ":" + dependencyList[i][0][0])
                    objects.append(dependencyList[i][2][0])

    # Check emotion for overall sentence
        analyzer = SentimentIntensityAnalyzer()
        vs = analyzer.polarity_scores(sentence)

        print(Character_1)
        print(actionsAssociatedWithCharacter1)

        print(objects)
        print(actionsAssociatedWithObjects)

        print(str(vs["compound"]))

        storygraph.append((sentence,Character_1,actionsAssociatedWithCharacter1,objects,actionsAssociatedWithObjects,vs["compound"],dependencyList))
        temp = [sentence,Character_1,actionsAssociatedWithCharacter1,objects,actionsAssociatedWithObjects,vs["compound"],dependencyList]
        print(temp)
        wr.writerow(temp)