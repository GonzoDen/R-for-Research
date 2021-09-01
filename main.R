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


#get a ratio of students who had it prior to AUCA vs have it after attending AUCA
mi_prior <- subset(mi, B1.1 == "Yes")
mi_after <- subset(mi, B1.1 == "No")

#create a pie chart demonstrating ratio mental issues vs. no mental issues
slices_pr <- c(nrow(mi_prior), nrow(mi_after))
lbls_pr <- c("Before AUCA", 
             "After AUCA")
pct_pr <- round(slices_pr/sum(slices_pr)*100)
lbls_pr <- paste(lbls_pr, pct_pr) # add percents to labels
lbls_pr <- paste(lbls_pr,"%",sep="") # ad % to labels
pie(slices_pr,labels = lbls_pr, col=rainbow(length(lbls_pr)),
    main="Ratio of 
    students experienced mental issues before attending AUCA
    to students who got them after attending AUCA")


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

#create a pie chart with distribution of students with mental issues among majors
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
    main="Spread of complaints related to mental health across majors")


#get list of all years of study containing students with mental issues
mi_ys <- subset(mi, select = c("A1"))
print(mi_ys)

#calculate the dataframe of students with mental issues for each major
mi_ys_soph <- subset(mi_ys,A1 == "Sophomore")
mi_ys_jun <- subset(mi_ys,A1 == "Junior")
mi_ys_sen <- subset(mi_ys,A1 == "Senior")
mi_ys_grad <- subset(mi_ys,A1 == "Graduate")

#create a pie chart with distribution of students with mental issues among majors
slices_ys <- c(nrow(mi_ys_soph), nrow(mi_ys_jun), nrow(mi_ys_sen), nrow(mi_ys_grad))
lbls_ys <- c("Sophomore", 
          "Junior",
          "Senior", 
          "Graduate")

pct_ys <- round(slices_ys/sum(slices_ys)*100)
lbls_ys <- paste(lbls_ys, pct_ys) # add percents to labels
lbls_ys <- paste(lbls_ys,"%",sep="") # ad % to labels
pie(slices_ys,labels = lbls_ys, col=topo.colors(length(lbls_ys)),
    main="Spread of complaints related to mental health across years of study")


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
lbls_ph <- paste(lbls_ph,"%",sep="") # add % to labels
pie(slices_ph,labels = lbls_ph,
    main="Spread of complaints related to physical health")


#create distribution of mental complaints 
mi_men <- subset(mi, select = c("B2"))

vec_sym_men = c ("excessive worry",
                "feeling sad or low",
                "difficulty to control the worry",
                "loss of pleasure or interest",
                "feelings of worthlessness or guilt",
                "withdrawal from social interactions",
                "desire to \"put life on pause\"",
                "thoughts on ending own life")

vec_men = c(0, 0, 0, 0, 0, 0, 0, 0)

for(i in 1:nrow(mi_men)) {
  if(grepl(vec_sym_men[1], mi_men[i, "B2"], fixed = TRUE)){
    vec_men[1] %+=% 1
  }
  if(grepl(vec_sym_men[2], mi_men[i, "B2"], fixed = TRUE)){
    vec_men[2] %+=% 1
  }
  if(grepl(vec_sym_men[3], mi_men[i, "B2"], fixed = TRUE)){
    vec_men[3] %+=% 1
  }
  if(grepl(vec_sym_men[4], mi_men[i, "B2"], fixed = TRUE)){
    vec_men[4] %+=% 1
  }
  if(grepl(vec_sym_men[5], mi_men[i, "B2"], fixed = TRUE)){
    vec_men[5] %+=% 1
  }
  if(grepl(vec_sym_men[6], mi_men[i, "B2"], fixed = TRUE)){
    vec_men[6] %+=% 1
  }
  if(grepl(vec_sym_men[7], mi_men[i, "B2"], fixed = TRUE)){
    vec_men[7] %+=% 1
  }
  if(grepl(vec_sym_men[8], mi_men[i, "B2"], fixed = TRUE)){
    vec_men[8] %+=% 1
  }
}

slices_men <- vec_men
lbls_men <- vec_sym_men
pct_men <- round(slices_men/sum(slices_men)*100)
lbls_men <- paste(lbls_men, pct_men) # add percents to labels
lbls_men <- paste(lbls_men,"%",sep="") # add % to labels
pie(slices_men,labels = lbls_men, col=rainbow(length(lbls_men)),
    main="Spread of complaints related to mental health")


#create distribution of academic complaints 
mi_ac <- subset(mi, select = c("C3"))

vec_sym_ac = c ("They are scheduled too tightly to each other",
                 "Distribution of workload is poorly designed",
                 "Inadequate deadlines",
                 "None")

vec_ac = c(0, 0, 0, 0)

for(i in 1:nrow(mi_ac)) {
  if(grepl(vec_sym_ac[1], mi_ac[i, "C3"], fixed = TRUE)){
    vec_ac[1] %+=% 1
  }
  if(grepl(vec_sym_ac[2], mi_ac[i, "C3"], fixed = TRUE)){
    vec_ac[2] %+=% 1
  }
  if(grepl(vec_sym_ac[3], mi_ac[i, "C3"], fixed = TRUE)){
    vec_ac[3] %+=% 1
  }
  if(grepl(vec_sym_ac[4], mi_ac[i, "C3"], fixed = TRUE)){
    vec_ac[4] %+=% 1
  }
}

slices_ac <- vec_ac
lbls_ac <- vec_sym_ac
pct_ac <- round(slices_ac/sum(slices_ac)*100)
lbls_ac <- paste(lbls_ac, pct_ac) # add percents to labels
lbls_ac <- paste(lbls_ac,"%",sep="") # add % to labels
pie(slices_ac,labels = lbls_ac, col=cm.colors(length(lbls_ac)),
    main="Discontent with academic scheduling")

