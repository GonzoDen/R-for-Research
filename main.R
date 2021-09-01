response <- read.csv("~/.../mentalh_ugrad.csv", header = T)

#load only real respondents who completed the survey
df <- subset (response, Status == "IP Address" & Progress == 100) 

#load only those of df who did not report on troubles with mental health
mn <- subset(df, B1 == "No" & B2 == "No")

#load only those of df who reported on troubles with mental health
mi <- subset(df, B1 != "No" & B2 != "No")


#create a pie chart demonstrating ratio mental issues vs. no mental issues
slices_yn <- c(nrow(mi), nrow(mn))
lbls_yn <- c("Have troubles", 
             "Do not have")
pct_yn <- round(slices_yn/sum(slices_yn)*100)
lbls_yn <- paste(lbls_yn, pct_yn) # add percents to labels
lbls_yn <- paste(lbls_yn,"%",sep="") # ad % to labels
pie(slices_yn,labels = lbls_yn, col=cm.colors(length(lbls_yn)),
    main="Ratio of 
    students having any troubles related to mental health 
    to students who do not have any complaints about it")


#get list of all majors containing students with mental issues
mi_maj <- subset(mi, select = c("A2"))
print(mi_maj)

#calculate the dataframe of students with mental issues for each major
mi_maj_eco <- subset(mi_maj,A2 == "Economics")
mi_maj_las <- subset(mi_maj, A2 == "Liberal Arts and Science")
mi_maj_psy <- subset(mi_maj, A2 == "Psychology")
mi_maj_ibl <- subset(mi_maj, A2 == "International Business Law")
mi_maj_soc <- subset(mi_maj, A2 == "Sociology")
mi_maj_ba <- subset(mi_maj, A2 == "Business Administration")

#create a pie chart with distribuition of students with mental issues among majors
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


#create distribution of physical complaints 
mi_ph <- subset(mi, select = c("B1"))
print(mi_ph)

#function "+="
`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))

vec_sym_ph = c ("Significant loss/gain of weight",
            "Sleeping too much or too little",
            "Poor concentration",
            "Feeling restless, irritable, or tense",
            "Fatigue",
            "Thinking or moving slower than usual",
            "Increased muscle aches or soreness",
            "Decrease of immune system")

vec_ph = c(0, 0, 0, 0, 0, 0, 0, 0)

for(i in 1:nrow(mi_ph)) {
  if(grepl(vec_sym_ph[1], mi_ph[i, "B1"], fixed = TRUE)){
    vec_ph[1] %+=% 1
  }
  if(grepl(vec_sym_ph[2], mi_ph[i, "B1"], fixed = TRUE)){
    vec_ph[2] %+=% 1
  }
  if(grepl(vec_sym_ph[3], mi_ph[i, "B1"], fixed = TRUE)){
    vec_ph[3] %+=% 1
  }
  if(grepl(vec_sym_ph[4], mi_ph[i, "B1"], fixed = TRUE)){
    vec_ph[4] %+=% 1
  }
  if(grepl(vec_sym_ph[5], mi_ph[i, "B1"], fixed = TRUE)){
    vec_ph[5] %+=% 1
  }
  if(grepl(vec_sym_ph[6], mi_ph[i, "B1"], fixed = TRUE)){
    vec_ph[6] %+=% 1
  }
  if(grepl(vec_sym_ph[7], mi_ph[i, "B1"], fixed = TRUE)){
    vec_ph[7] %+=% 1
  }
  if(grepl(vec_sym_ph[8], mi_ph[i, "B1"], fixed = TRUE)){
    vec_ph[8] %+=% 1
  }
}

slices_ph <- vec_ph
lbls_ph <- vec_sym_ph
pct_ph <- round(slices_ph/sum(slices_ph)*100)
lbls_ph <- paste(lbls_ph, pct_ph) # add percents to labels
lbls_ph <- paste(lbls_ph,"%",sep="") # ad % to labels
pie(slices_ph,labels = lbls_ph, col=rainbow(length(lbls_ph)),
    main="Spread of complaints related to physical health")

#TODO YEAR OF STUDY
#TODO PHYSICAL & EMOTIONAL DISTRIBUTION (contains)
