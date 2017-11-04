import re

#Reading the entry in the given csv file
f = open("A Portrait of the Artist as a Young Man.txt", encoding="utf8")
for line in f:
    if re.match("(?=*Simon)(?=*Stephen)", line):
        print(line)
