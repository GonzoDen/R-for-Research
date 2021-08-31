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
mn <- subset(df, B1 == "No")

#load only those of df who reported on troubles with mental health
mi <- subset(df, B1 != "No")

#load only those of df who did not report on troubles with mental health
mn_men <- subset(df, B2 == "No")

#load only those of df who reported on troubles with mental health
mi_men <- subset(df, B2 != "No")

#GET LIST OF ALL MAJORS CONTAINING MIed STUDENTS
mi_maj <- subset(mi, select = c("A2"))
print(mi_maj)

#create comparison to those who reported "No" vs. !"No"
#?????

#db <- subset (response, Status == "IP Address" & Progress == 100, select = c("A3", "A4")) 
#data(db)
#mosaicplot(db)

#CALCULATE THE NUMBER OF MENTALLY ISSUED STUDENTS FOR EACH MAJOR
#create queries fot
mi_maj_eco <- subset(mi_maj,A2 == "Economics")
#print(nrow(mi_maj_eco))
mi_maj_las <- subset(mi_maj, A2 == "Liberal Arts and Science")
mi_maj_psy <- subset(mi_maj, A2 == "Psychology")
mi_maj_ibl <- subset(mi_maj, A2 == "International Business Law")
mi_maj_soc <- subset(mi_maj, A2 == "Sociology")
mi_maj_ba <- subset(mi_maj, A2 == "Business Administration")

#create a piechart on symptoms distribution
#is it possible to do via if
slices <- c(nrow(mi_maj_eco), nrow(mi_maj_las), nrow(mi_maj_psy), nrow(mi_maj_ibl), nrow(mi_maj_soc), nrow(mi_maj_ba))
lbls <- c("Economics", 
          "Liberal Arts and Science",
          "Psychology", 
          "International Business Law",
          "Sociology",
          "Business Administration")

pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Spread of mental complaint among surveyed")


slices_yn <- c(nrow(mi), nrow(mn))
lbls_yn <- c("Yes", "No")
pct_yn <- round(slices_yn/sum(slices_yn)*100)
lbls_yn <- paste(lbls_yn, pct_yn) # add percents to labels
lbls_yn <- paste(lbls_yn,"%",sep="") # ad % to labels
pie(slices_yn,labels = lbls_yn, col=rainbow(length(lbls_yn)),
    main="Spread of mental complaint among surveyed")

slices_yn <- c(nrow(mi), nrow(mn))
lbls_yn <- c("Yes", "No")
pct_yn <- round(slices_yn/sum(slices_yn)*100)
lbls_yn <- paste(lbls_yn, pct_yn) # add percents to labels
lbls_yn <- paste(lbls_yn,"%",sep="") # ad % to labels
pie(slices_yn,labels = lbls_yn, col=rainbow(length(lbls_yn)),
    main="Spread of mental complaint among surveyed")

#TODO YEAR OF STUDY
#TODO PHYSICAL & EMOTIONAL DISTRIBUTION (contains)
