response <- read.csv("~/.../mentalh_ugrad.csv", header = T)

#summary(response)
#head(response$C4)
#names(response)

#subset(response, Status=="IP Address", select=
#head(response, 2) #prints first 3 rows
#length(response) #number of variables (questions)
#names(response) #names of the vars

#loaded only real responders who completed the survey
df <- subset (response, Status == "IP Address" & Progress == 100, select = c("A3", "A4")) 

data(df)
mosaicplot(df)
#print(df)

