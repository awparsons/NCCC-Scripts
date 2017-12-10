###########################################################################
########    Using RMark to build single-season occupancy models    ########
###########################################################################

#Program Mark must be installed on your computer to use RMark
#http://www.phidot.org/software/mark/downloads/

#Install the RMark package and load it into the workspace
install.packages("RMark")
library(RMark)

#Set the working directory where the dataset is saved
#setwd("..")

#########################Load and prepare the data#######################

#Load your data.  
#Data take the form of a comma separated text file with 
#a column if concatenated 0s and 1s representing a capture history for a
#given species at each site.  This column is always marked "ch"
#Site-specific and time-specific covariates can be added to the same file
#When the file is read in (using read.table), you must specify the class 
#(character or numeric) of each column
BEAR <- read.table("Bearall2.txt", colClasses=c("character", rep("numeric",15), rep("character", 1),rep("numeric",13),rep("character",289)), header=TRUE)

#The code below is only needed if using daily covariates which were marked 
#with "." when a given camera was not functioning, it replaces the "."s 
#with 0s
for (i in 32:319) {
  BEAR[BEAR[,i]==".",i]="0"
  BEAR[,i]=as.numeric(BEAR[,i]) } 

#Set any categorical covariates as factors and scale those that are
#continuous to best interpret the meaning of resulting beta coefficients
BEAR$Hunting <- factor(BEAR$Hunting)
BEAR$LatbyLong <- scale(BEAR$LatbyLong)
BEAR$Dist_5km <- scale(BEAR$Dist_5km)
BEAR$Ag_5km <- scale(BEAR$Ag_5km)
BEAR$LC_5km <- scale(BEAR$LC_5km)
BEAR$HDens_5km <- scale(BEAR$HDens_5km)
BEAR$Nearest_neighbor <- scale(BEAR$Nearest_neighbor)
BEAR$Cam_distance <- as.numeric(BEAR$Cam_distance)
BEAR$Cam_distance <- scale(BEAR$Cam_distance)
BEAR$People_site <- scale(BEAR$People_site)

###########################Run the model##################################

#The "mark" wrapper calls Program Mark.  The user must specify the type of
#model to run, in this case "Occupancy".  Then, define the model parameters
#in much the same way you would using lm() in base R.  If using
#categorical covariates, you will enter these in the "groups" argument and
#will obtain separate occupancy estimates for each group.
mod1=mark(BEAR, model="Occupancy",
                 model.parameters=list(p=list(formula=~NVDI_site+WeekDay+
                                                Cloud),
                                       Psi=list(formula=~LatbyLong+
                                                  Dist_5km+Ag_5km+LC_5km+
                                                  Edge_5km+HDens_5km)), 
                 groups=c("Hunting"))

mod2=mark(BEAR, model="Occupancy",
          model.parameters=list(p=list(formula=~NVDI_site+WeekDay+
                                         Cloud),
                                Psi=list(formula=~Ag_5km+LC_5km)), 
          groups=c("Hunting"))

mod3=mark(BEAR, model="Occupancy",
          model.parameters=list(p=list(formula=~NVDI_site+WeekDay+
                                         Cloud),
                                Psi=list(formula=~LC_5km)), 
          groups=c("Hunting"))

###############################Rank by AIC#############################
cm<-collect.models(lx = NULL, type="Occupancy", table = TRUE)
table<-model.table(cm)

#Write table to file in working directory
write.csv(table, file="AIC_Table.csv")