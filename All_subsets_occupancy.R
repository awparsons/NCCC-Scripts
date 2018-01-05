##########################################################################
############# Using MuMIN and RMark to run all model subsets #############
########################## Occupancy Example #############################
##########################################################################

#Install the RMark package and load it into the workspace
install.packages("RMark")
install.packages("MuMIn")
library(RMark)
library(MuMIn)

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

####################Run the most parameterized model######################

#The "mark" wrapper calls Program Mark.  The user must specify the type of
#model to run, in this case "Occupancy".  Then, define the model parameters
#in much the same way you would using lm() in base R.  If using
#categorical covariates, you will enter these in the "groups" argument and
#will obtain separate occupancy estimates for each group.
mod_global=mark(BEAR, model="Occupancy",
          model.parameters=list(p=list(formula=~NVDI_site+WeekDay+
                                         Cloud),
                                Psi=list(formula=~LatbyLong+
                                           Dist_5km+Ag_5km+LC_5km+
                                           Edge_5km+HDens_5km)), 
          groups=c("Hunting"))

######################### Run all subsets #######################
#This will run all combinations of covariates in the global model
#and rank them by QAIC
#remember to use the c-hat from the global model
dd=dredge(mod_global,rank=QAIC, chat=3.794)

#Output the results to a file 
write.csv(dd,"BEARQAIC.csv")

#Get model-averaged parameter estimates for all models within top 
#4 QAIC points
final_coefs_subset <-summary(model.avg(dd, subset = delta < 4))
summary<-summary(final_coefs_subset)
summary

#Get variable importance ranking for full model set
#This sums the AIC weights of each variable across the models in the
#set where that variable occurs.  If a variable occurs frequently
#over the top ranked models, it will receive a higher ranking
final_coefs <-summary(model.avg(dd))
varimp<-summary(final_coefs)
varimp

############## If you have a lot of covariates, you can ############
############## first select the best detection (p) model ###########
############## followed by the best occupancy (psi) model ##########
#Fix the global Psi model and run all subsets of the p covariates
#Use QAIC for model selection due to overdispersion
#Check the output of the Global model (above) to get c-hat for 
#dredge below

#Pull the coefficient names so they can be written correctly in the
#dredge argument
MuMIn:::fixCoefNames(names(coeffs(mod_global)))

#Dredge the detection model, holding the global Psi model fixed.
#This will run all combinations of p covariates in the global model
#and rank them by QAIC
dd=dredge(mod_global, fixed=c("psi(LatbyLong)", "psi(Dist_5km)", "psi(Ag_5km)", "psi(LC_5km)", "psi(Edge_5km)", "psi(HDens_5km)"),rank=QAIC, chat=3.794)

#Output the results to a file 
#Examine the .csv file and choose the top p model
#or the least parameterized p within the top 4QAIC points
write.csv(dd,"pBEARQAIC.csv")

#Then fix that top p model and run all subsets of the Psi covariates
#remember to use the c-hat from the global model
MuMIn:::fixCoefNames(names(coeffs(mod_global)))
dd2=dredge(mod_global, fixed=c("p(NVDI_site)","p(WeekDay)","p(Cloud)"),rank=QAIC, chat=3.794)
write.csv(dd2,"psiBEARQAIC.csv")