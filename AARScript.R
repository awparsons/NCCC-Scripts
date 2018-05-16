require(dplyr)
require(plyr)
require(reshape2)
require(lubridate)
require(data.table)
require(plotrix)
require(ggplot2)
require(survminer)
require(survival)

### Set Time Zone

ols.sys.timezone <- Sys.timezone()
Sys.setenv(TZ = 'GMT')

########Load data that contains all species########

setwd("C:/Users/Arielle/Desktop/NRC/eMammal/Hunting and Hiking/Dog Paper/AAR Scripts")

df <- read.csv("AARtest.csv")

########Format dates and times of sequences########

# Remove data with do dates
df[df$begin_date_time=="",] <- NA
df <- df[!is.na(df$begin_date_time),] # Remove data with do dates

# format with date/time
df$Begin.Time <- as.POSIXct(df$begin_date_time, format = "%m/%d/%Y %H:%M")
df$End.Time <- as.POSIXct(df$end_date_time, format = "%m/%d/%Y %H:%M")
str(df$Begin.Time)
str(df$End.Time)

#Subset by species (all preds and 1 prey)
Prey<-"Sciurus carolinensis"
Pred1<-"Canis familiaris"
Pred2<-"Vulpes vulpes"
species <- list(Prey, Pred1, Pred2) # The species the user wants to focus on, in character and scientific name.

#Define any other predators in the system that could confound 
#results, we'll deal with them later
other_pred<-("Canis latrans|Ursus Americanus|Procyon lotor")
  
#Sort by deployment and begin_date_time
df.sp<-df
df.sp<-df.sp[with(df.sp, order(title, Begin.Time)), ]

#######################################################################
#              Mark the elements of the T ratios                      #
# These "Marks" denote the elements of the predator/prey "sanwich"    #
#                                                                     #
#         See Parsons et al. 2016 for more details                    # 
#                                                                     #
#              T1=Prey followed by a predator                         #
#                                                                     #
#       T2=predator followed by a prey, following a T1 and without    #
#                 another predator species in between                 #
#                                                                     #
#               T3=successive prey detections                         #
#                                                                     #              
#       T4=The total time between successive prey detections with a   #
#                 predator species detection in between               #
#       (may be many predator detection but of only one species)       #
#######################################################################

#First split by deployment
df.sp$title<-droplevels(df.sp$title)
out <- split(df.sp, f = df.sp$title)

#Remove any elements with just one row
for(i in names(out)){
  if (nrow(out[[i]]) < 2) {out[[i]]<-NULL}
}

#See how many detections of each species at each site
t<-list()
Count<-data.frame(Site=character(),
                  Prey=numeric(),
                  Pred1=numeric(),
                  Pred2=numeric())
for(i in names(out)){
  t[[i]]<-table(out[[i]]$sn)
  Count[i, 2]<-t[[i]][names(t[[i]])==Prey]
  Count[i, 3]<-t[[i]][names(t[[i]])==Pred1]
  Count[i, 4]<-t[[i]][names(t[[i]])==Pred2]
}

Count

#Now add the marks
sn<-list()
title<-list()
for(i in names(out)){
  out[[i]]$Tmark<-rep(NA, nrow(out[[i]]))
  c<-which(colnames(out[[i]])=="Tmark")
  sn[[i]]<-as.data.frame(out[[i]]$sn)
  names(sn[[i]])<-c("sn")
  for(j in 2:nrow(out[[i]])){
    if(as.character(sn[[i]][j,])==as.character(Prey) & as.character(sn[[i]][j-1,])==as.character(Prey)){out[[i]][j, c]<-("T3")} 
    else if (as.character(sn[[i]][j,])==as.character(Pred1) & as.character(sn[[i]][j-1,])==as.character(Prey)) {out[[i]][j, c]<-("Pred1T1")}
    else if (as.character(sn[[i]][j,])==as.character(Pred2) & as.character(sn[[i]][j-1,])==as.character(Prey)) {out[[i]][j, c]<-("Pred2T1")}
    else if (as.character(sn[[i]][j,])==as.character(Pred1) & as.character(sn[[i]][j-1,])==as.character(Pred1)) {out[[i]][j, c]<-(NA)}
    else if (as.character(sn[[i]][j,])==as.character(Pred2) & as.character(sn[[i]][j-1,])==as.character(Pred1)) {out[[i]][j, c]<-("PP")}
    else if (as.character(sn[[i]][j,])==as.character(Pred1) & as.character(sn[[i]][j-1,])==as.character(Pred2)) {out[[i]][j, c]<-("PP")}
    else if (as.character(sn[[i]][j,])==as.character(Pred2) & as.character(sn[[i]][j-1,])==as.character(other_pred)) {out[[i]][j, c]<-("PP")}
    else if (as.character(sn[[i]][j,])==as.character(Pred1) & as.character(sn[[i]][j-1,])==as.character(other_pred)) {out[[i]][j, c]<-("PP")}
    else if (as.character(sn[[i]][j,])==as.character(Prey) & as.character(sn[[i]][j-1,])==as.character(other_pred)) {out[[i]][j, c]<-("PP")}
    else if (as.character(sn[[i]][j,])==as.character(Pred2) & as.character(sn[[i]][j-1,])==as.character(Pred2)) {out[[i]][j, c]<-(NA)}
    else if (as.character(sn[[i]][j,])==as.character(Prey) & as.character(sn[[i]][j-1,])==as.character(Pred1)) {out[[i]][j, c]<-("Pred1T2")}
    else if (as.character(sn[[i]][j,])==as.character(Prey) & as.character(sn[[i]][j-1,])==as.character(Pred2)) {out[[i]][j, c]<-("Pred2T2")}
    }
}

########################## Clean the Marks ###########################
#We need to make sure all Pred1T1 are 
#followed by Pred1T2 with nothing between and same for Pred2
#the last T1 needs a T2 or to be removed

#First remove all the NAs and add 3 final rows, calling them 
#"Nothing".  These are place holders to allow the loops to work.
non_na<-list()
newc<-list()
newc2<-list()
newc3<-list()
df2<-list()
bad1<-list()
bad2<-list()
bad3<-list()
bad4<-list()
bad5<-list()
r<-list()
for(i in names(out)){
d<-which(colnames(out[[i]])=="Tmark")
non_na[[i]]<-out[[i]][complete.cases(out[[i]][d]),]
c<-ncol(non_na[[1]])
newc[[i]]<-rep(NA, c)
newc2[[i]]<-rep(NA, c)
newc3[[i]]<-rep(NA, c)
newc[[i]][length(newc[[i]])]<-"Nothing"
newc2[[i]][length(newc2[[i]])]<-"Nothing"
newc3[[i]][length(newc3[[i]])]<-"Nothing"
df2[[i]]<-as.data.frame(rbind(non_na[[i]], newc[[i]],
                              newc2[[i]], newc3[[i]]))
r[[i]]<-nrow(df2[[i]])
bad1[[i]]<-rep(NA, nrow(df2[[i]]))
bad2[[i]]<-rep(NA, nrow(df2[[i]]))
bad3[[i]]<-rep(NA, nrow(df2[[i]]))
bad4[[i]]<-rep(NA, nrow(df2[[i]]))
bad5[[i]]<-rep(NA, nrow(df2[[i]]))
}

#Loop through all Tmark records at each site, starting with the
#second one.  If a mark is a T2 then see if the one coming before 
#it is a T1 for the appropriate predator and mark it "bad" if the
#sandwich is not intact to remove in the next step
#Add in the first row
one<-list()
for(i in names(df2)){
  one[[i]]<-out[[i]][1,]
  one[[i]]$bad<-"Nothing"
  one[[i]]$Tmark<-"Nothing"
  df2[[i]]$bad<-"NA"
  names(df2[[i]])<-names(one[[i]])
  df2[[i]]<-as.data.frame(rbind(one[[i]], df2[[i]]))
}

r<-lapply(df2, nrow)

for(i in names(df2)){
for(j in 1:r[[i]]){
  c<-which(colnames(out[[i]])=="Tmark")
  if(as.character(df2[[i]][j, c])==as.character("Pred1T1") & as.character(df2[[i]][j+1, c])!=as.character("Pred1T2")){bad1[[i]][j]<-("Bad")}
  else if(as.character(df2[[i]][j, c])==as.character("Pred2T1") & as.character(df2[[i]][j+1, c])!=as.character("Pred2T2")){bad2[[i]][j]<-("Bad")}
}
}

for(i in names(df2)){
for(j in 2:r[[i]]){
  c<-which(colnames(out[[i]])=="Tmark")
  if(as.character(df2[[i]][j, c])==as.character("Pred1T2") & as.character(df2[[i]][j-1, c])!=as.character("Pred1T1")){bad3[[i]][j]<-("Bad")}
  else if(as.character(df2[[i]][j, c])==as.character("Pred2T2") & as.character(df2[[i]][j-1, c])!=as.character("Pred2T1")){bad4[[i]][j]<-("Bad")}
  }
}

for(i in names(df2)){
  for(j in 1:r[[i]]){
    c<-which(colnames(out[[i]])=="Tmark")
    if(as.character(df2[[i]][j, c])==as.character("PP")){bad5[[i]][j]<-("Bad")}
    df2[[i]]$bad<-rep(NA, nrow(df2[[i]]))
  }
}

for(i in names(df2)){
for(j in 1:r[[i]]){
c<-which(colnames(out[[i]])=="bad")
if(!is.na(bad1[[i]][j])|!is.na(bad2[[i]][j])|!is.na(bad3[[i]][j])|!is.na(bad4[[i]][j])|!is.na(bad5[[i]][j])){df2[[i]][j, c]<-("Bad")}
}
}

#Remove the bad rows 
df3<-list()
for(i in names(df2)){
df3[[i]]<-df2[[i]]%>%filter(is.na(df2[[i]]$bad))
}

################## Add times for each Tmark ##########################
#Remove any elements with only the first row and no others
df4<-df3
for(i in names(df4)){
  if (nrow(df4[[i]]) < 2) {df4[[i]]<-NULL}
}

#Sort by deployment and begin_date_time
#Then Add column: 
#Time_from = Begin.Time-End.Time(of row above)
for(i in names(df4)){
  c<-which(colnames(df4[[i]])=="Begin.Time")
  d<-which(colnames(df4[[i]])=="End.Time")
  df4[[i]]<-df4[[i]][with(df4[[i]], order(title, Begin.Time)), ]
  df4[[i]]$Time_from_min <- c(NA, difftime((df4[[i]][2:nrow(df4[[i]]), c]), (df4[[i]][1:(nrow(df4[[i]])-1), d]), unit="min"))
  df4[[i]]$Time_from_day <- df4[[i]]$Time_from_min/1440 
  df4[[i]]$title<-droplevels(df4[[i]]$title)
}

################### Calculate T4s ##################################
#Make a list to hold the output and set contents to numeric
#You will need to make a separate list for any additional predators
#and run an additional loop for each below
Pred1T4<-list()
Pred2T4<-list()
for(i in names(Pred1T4)){
  Pred1T4[[i]]<-as.numeric(Pred1T4[[i]])
  Pred2T4[[i]]<-as.numeric(Pred2T4[[i]])
}

#Define the number of rows that should be in each list element
n<-lapply(df4, nrow)

#For each site, sum the times associated with each T1 and T2 in a 
#sandwich and save in the appropriate T4 list
for(i in names(df4)){
  for(j in 2:n[[i]]){
    c<-which(colnames(df4[[i]])=="Tmark")
    d<-which(colnames(df4[[i]])=="Time_from_day")
    if(as.character(df4[[i]][j, c])==as.character("Pred1T2") & as.character(df4[[i]][j-1, c])==as.character("Pred1T1")){Pred1T4[[i]][j]<-sum(df4[[i]][j, d], df4[[i]][j-1, d])}
    else {Pred1T4[[i]][j]<-NA}
  }
}

for(i in names(df4)){
  for(j in 2:n[[i]]){
    c<-which(colnames(df4[[i]])=="Tmark")
    d<-which(colnames(df4[[i]])=="Time_from_day")
    if(as.character(df4[[i]][j, c])==as.character("Pred2T2") & as.character(df4[[i]][j-1, c])==as.character("Pred2T1")){Pred2T4[[i]][j]<-sum(df4[[i]][j, d], df4[[i]][j-1, d])}
    else {Pred2T4[[i]][j]<-NA}
  }
}

#################### Calculate T3s, T1s and T2s #################### 
#Make lists to hold the output and set the values to numeric
T3<-list()
Pred1T1T2<-list()
Pred2T1T2<-list()
for(i in names(T3)){
  T3[[i]]<-as.numeric(T3[[i]])
  Pred1T1T2[[i]]<-as.numeric(Pred1T1T2[[i]])
  Pred2T1T2[[i]]<-as.numeric(Pred2T1T2[[i]])
}

#Set the length of the output for each site (number of rows)
n<-lapply(df4, nrow)

#Calculate the T3s and save to be compared to the T4s in a later step
for(i in names(df4)){
  for(j in 2:n[[i]]){
    c<-which(colnames(df4[[i]])=="Tmark")
    d<-which(colnames(df4[[i]])=="Time_from_day")
    if(as.character(df4[[i]][j, c])==as.character("T3")){T3[[i]][j]<-df4[[i]][j, d]}
    else {T3[[i]][j]<-NA}
  }
}

########## Calculate the T2/T1 ratios for each site/predator ##########
#Note, if dealing with more than two predators you will need to add
#a line for each one in the loop below and add a new output list
for(i in names(df4)){
  for(j in 2:n[[i]]){
    c<-which(colnames(df4[[i]])=="Tmark")
    d<-which(colnames(df4[[i]])=="Time_from_day")
    if(as.character(df4[[i]][j, c])==as.character("Pred1T2") & as.character(df4[[i]][j-1, c])==as.character("Pred1T1")){Pred1T1T2[[i]][j]<-(df4[[i]][j, d]/df4[[i]][j-1, d])}
    else if(as.character(df4[[i]][j, c])==as.character("Pred2T2") & as.character(df4[[i]][j-1, c])==as.character("Pred2T1")){Pred2T1T2[[i]][j]<-(df4[[i]][j, d]/df4[[i]][j-1, d])}
  }
}

#Convert the lists to data frames, adding a column for each
#Predator and then combine together into one data frame.
Pred1_z <- data.frame(Title = rep(names(Pred1T1T2), sapply(Pred1T1T2, length)),
                 T2T1Ratio = unlist(Pred1T1T2, use.names=FALSE))
Pred1_z<-as.data.frame(Pred1_z[complete.cases(Pred1_z),])
Pred1_z$Pred<-rep("Pred1", nrow(Pred1_z))
Pred2_z <- data.frame(Title = rep(names(Pred2T1T2), sapply(Pred2T1T2, length)),
                      T2T1Ratio = unlist(Pred2T1T2, use.names=FALSE))
Pred2_z<-as.data.frame(Pred1_z[complete.cases(Pred2_z),])
Pred2_z$Pred<-rep("Pred2", nrow(Pred2_z))
Preds_t<-as.data.frame(rbind(Pred1_z, Pred2_z))

library(dplyr)
library(plotrix)
m<-as.data.frame(Preds_t%>%group_by(Pred)%>%summarize(mean=mean(T2T1Ratio), sd=sd(T2T1Ratio), se=std.error(T2T1Ratio)))
m

#Check how many T2/T1 ratios you have
nrow(Pred1_z)
nrow(Pred2_z)

################# Compute T4/T3 ratio for each site/predator #######################
#Calculate means
Pred1T4_mean<-lapply(Pred1T4, mean, na.rm=TRUE)
Pred2T4_mean<-lapply(Pred2T4, mean, na.rm=TRUE)
T3_mean<-lapply(T3, mean, na.rm=TRUE)

#Make new lists to store the T4/T3 ratios for each site
#Then calculate the ratios
T4_T3_Pred1<-NA
T4_T3_Pred2<-NA
for (i in names(T3_mean)){
T4_T3_Pred1[i]<-Pred1T4_mean[[i]]/T3_mean[[i]]
T4_T3_Pred2[i]<-Pred2T4_mean[[i]]/T3_mean[[i]]
}

#Clean up the list, add a column to label the predators and combine
#together
T4_T3_Pred1<-as.data.frame(T4_T3_Pred1)
T4_T3_Pred2<-as.data.frame(T4_T3_Pred2)
T4_T3_Pred1<-as.data.frame(T4_T3_Pred1[complete.cases(T4_T3_Pred1),])
T4_T3_Pred1$Pred<-rep("Pred1", nrow(T4_T3_Pred1))
names(T4_T3_Pred1)<-c("T4T3Ratio", "Pred")
T4_T3_Pred2<-as.data.frame(T4_T3_Pred2[complete.cases(T4_T3_Pred2),])
T4_T3_Pred2$Pred<-rep("Pred2", nrow(T4_T3_Pred2))
names(T4_T3_Pred2)<-c("T4T3Ratio", "Pred")
Preds_z<-as.data.frame(rbind(T4_T3_Pred1, T4_T3_Pred2))

############## Test the relative strength of each predator ############
################# in terms of provoking avoidance ####################
#Perform a wilcoxon signed-rank test (nonparametric) to see if 
#T2/T1 and T4/T3 ratios are significantly different in response to different
#predators

#Significant p.values indicate the predators differ in their ability
#to provoke avoidance
test_T2T1<-wilcox.test(T2T1Ratio~Pred, data=Preds_t)
test_T2T1$p.value
test_T3T4<-wilcox.test(T4T3Ratio~Pred, data=Preds_z)
test_T3T4$p.value

#Make a graph, the red dot shows the mean and dotted line the 1/1
#ratio.  Those with ratios larger than one show avoidance and <1
#show attraction
ggplot(data=Preds_t, aes(x=Pred, y=log(T2T1Ratio))) +
  geom_boxplot(fill="steelblue")+
  stat_summary(fun.y = mean, geom="point",colour="darkred", size=2) +
  scale_x_discrete(labels=c(Pred1, Pred2))+
  theme_classic() + 
  labs(x="Predator", y = "Log T2/T1 Ratio")+
  theme(axis.text = element_text(color="black", size=12))+
  geom_hline(yintercept=1, linetype=2)+
  annotate("text", x = 1.5, y = 10, label = "Avoidance", color="red")+
  annotate("text", x = 1.5, y = -5, label = "Attraction", color="blue")+
  ggtitle("T2/T1 Ratio")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data=Preds_z, aes(x=Pred, y=log(T4T3Ratio))) +
  geom_boxplot(fill="steelblue")+
  stat_summary(fun.y = mean, geom="point",colour="darkred", size=2) +
  scale_x_discrete(labels=c(Pred1, Pred2))+
  theme_classic() + 
  labs(x="Predator", y = "Log T4/T3 Ratio")+
  theme(axis.text = element_text(color="black", size=12))+
  geom_hline(yintercept=1, linetype=2)+
  annotate("text", x = 1.5, y = 10, label = "Avoidance", color="red")+
  annotate("text", x = 1.5, y = -3, label = "Attraction", color="blue")+
  ggtitle("T4/T3 Ratio")+
  theme(plot.title = element_text(hjust = 0.5))

############ Test Significance with Exponential Model ################
#This is an absolute test of significant avoidance of a predator
#by prey
#It is different from the test above which compares the relative 
#strength of avoidance/attraction of different predators on a prey 
#species

#T4 and T3 are most appropriate for this
#First unlist the T4s and T3 into data frames and clean them up
#Make separate data frames for each predator and combine with
#T3.  Then mark a 0 for T3 and 1 for T4s in a new Preds column
T4_Pred1 <- data.frame(Title = rep(names(Pred1T4), sapply(Pred1T4, length)),
                      T4 = unlist(Pred1T4, use.names=FALSE))
T4_Pred1<-as.data.frame(T4_Pred1[complete.cases(T4_Pred1),])
T4_Pred2 <- data.frame(Title = rep(names(Pred2T4), sapply(Pred2T4, length)),
                       T4 = unlist(Pred2T4, use.names=FALSE))
T4_Pred2<-as.data.frame(T4_Pred2[complete.cases(T4_Pred2),])
T3_z <- data.frame(Title = rep(names(T3), sapply(T3, length)),
                       T4 = unlist(T3, use.names=FALSE))
T3_z<-as.data.frame(T3_z[complete.cases(T3_z),])

#Get the mean T3 and T4 for each site
detach(package:plyr)
library(dplyr)
T4_P1<-T4_Pred1%>%group_by(Title)%>%summarize(mean=mean(T4))
T4_P1$Pred<-rep(1, nrow(T4_P1))
T4_P2<-T4_Pred2%>%group_by(Title)%>%summarize(mean=mean(T4))
T4_P2$Pred<-rep(1, nrow(T4_P2))
T3_s<-T3_z%>%group_by(Title)%>%summarize(mean=mean(T4))
T3_s$Pred<-rep(0, nrow(T3_s))

#Then make sure we have mean T3 and T4 for each site, otherwise
#Remove the sites
`%notin%` <- function(x,y) !(x %in% y) 
d<-setdiff(as.character(T4_P1$Title), as.character(T3_s$Title))
d2<-setdiff(T3_s$Title, T4_P1$Title)
dat_P1_z<-T3_s[T3_s$Title %notin% d2,]
dat_P1_q<-T4_P1[T4_P1$Title %notin% d,]
dat_P1<-as.data.frame(rbind(dat_P1_z, dat_P1_q))

dn<-setdiff(as.character(T4_P2$Title), as.character(T3_s$Title))
d2n<-setdiff(T3_s$Title, T4_P2$Title)
dat_P2_z<-T3_s[T3_s$Title %notin% d2n,]
dat_P2_q<-T4_P2[T4_P2$Title %notin% dn,]
dat_P2<-as.data.frame(rbind(dat_P2_z, dat_P2_q))

#Fit a Cox proportional hazard model to see if the time between
#successive prey detections is significantly different if a predator
#passes in the middle
cox_P1 <- coxph(Surv(mean) ~ Pred, data = dat_P1)
cox_P2 <- coxph(Surv(mean) ~ Pred, data = dat_P2)

#A significant p.value indicates predators have an influence on the
#time to the next prey detection (i.e. provoke avoidance)
summary(cox_P1)
summary(cox_P2)
#The hazard ratios exp(coef) are interpretable as multiplicative 
#effects on the hazard. Having a predator in between (Pred=1) reduces 
#the hazard by a factor of 0.34, or 66%. 
#Having a predator in between is associated with a lower probability 
#of a second prey passage (i.e. longer time lag).

##################### Plot the results ################################
#The code below makes a plot for each predator and shows the 
#Time-to-detection of the next prey species at sites where the 
#predator is present (pred=1) and absent (pred=0)

fit_P1 <- survfit(Surv(mean) ~ Pred, data = dat_P1)
fit_P2 <- survfit(Surv(mean) ~ Pred, data = dat_P2)

ggsurvplot(fit_P1, data = dat_P1, conf.int = TRUE,
           ggtheme = theme_minimal(), 
           xlab=c("Days to next prey passage"), ylab=c("Probability of second prey passage"),
           xlim=c(0, max(dat_P1$mean)), fun="event", title=Pred1)

ggsurvplot(fit_P2, data = dat_P2, conf.int = TRUE,
           ggtheme = theme_minimal(), 
           xlab=c("Days to next prey passage"), ylab=c("Probability of second prey passage"),
           xlim=c(0, max(dat_P2$T4)), fun="event", title=Pred2)
