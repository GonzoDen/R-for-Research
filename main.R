response <- read.csv("~/.../mentalh_ugrad.csv", header = T)

#summary(response)
#head(response$C4)
#names(response)

#subset(response, Status=="IP Address", select=
#head(response, 2) #prints first 3 rows
#length(response) #number of variables (questions)
#names(response) #names of the vars

#load only real responders who completed the survey
#df <- subset (response, Status == "IP Address" & Progress == 100, select = c("A3", "A4")) 
df <- subset (response, Status == "IP Address" & Progress == 100) 

##df's index = /csv index - 1 #'cause count starts from 0

#load only those of df who did not report on troubles with mental health
mn <- subset(df, B1 == "No", select = c("B1"))
print(mn)

#load only those of df who reported on troubles with mental health
mi <- subset(df, B1 != "No", select = c("B1"))
print(mi)

#create comparison to those who reported "No" vs. !"No"
#?????

#create a piechart on symptoms distribution
slices <- c(10, 12, 4, 16, 8)
lbls <- c("Sleeping too much or too little", 
          "Poor concentration",
          "Feeling restless, irritable, or tense", 
          "Thinking or moving slower than usual",
          "Decrease of immune system")

pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Spread of Depression among majors")

