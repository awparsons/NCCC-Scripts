###############################################################################
#                   eMammal Data Analysis                                     #
#                     Biodiversity Lab                                        #
###############################################################################

############################################################################### 
#                   Step 1: Setting the working directory                     #
#                      and installing/loading packages                        #
###############################################################################
#First direct R to your working directory, the folder where you will store your 
#analysis inputs and outputs.
#Change the below as appropriate for your machine, using R studio you can go to 
#"Files" in the righthand pane, then "...", navigate to the folder, then click the
#"more" gear and select "Set as working directory"
setwd("C:/Users/Arielle/Desktop/NRC/eMammal/WRC/Scripts/Scripts")

#Then, you will install and load the packages required for the analyses you will do
#The following code checks if the required packages exist in your library
#If you do not have that package, it will go ahead and install them.
list.of.packages<-c("dplyr", "plyr", "xtable", "overlap", "ggplot2", "activity")
new.packages<-list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages))install.packages(new.packages)

#Now load the needed packages
library(plyr)
library(dplyr)
library(xtable)
library(overlap)
library(ggplot2)
library(activity)
library(data.table)

############################################################################### 
#                   Step 2: Load and clean the dataset                        #
#                                                                             #
###############################################################################
#Load the latest dataset downloaded from the website
#Note that the filename will change each time so make sure it is
#edited properly below. 
dat <-read.csv(file="sianctapi-selected-observations-5ac6624e53b28.csv")
names(dat)#these are the column names
unique(dat$Subproject)#these are the counties in the dataset

#Fix the timestamps
dat$Begin <- gsub("T", " ", dat$Begin.Time)  # This will make a new column called "Begin" with just date and time separated
dat$End <- gsub("T", " ", dat$End.Time)
ols.sys.timezone <- Sys.timezone() #Setting timezone
Sys.setenv(TZ = 'EST') #Setting timezone --- "EST" is eastern timezone
dat$Begin2 <- as.POSIXct(dat$Begin, format = "%Y-%m-%d %H:%M:%S") #This formats the date and time to desired formats
dat$End2 <- as.POSIXct(dat$End, format = "%Y-%m-%d %H:%M:%S")
class(dat$Begin2)

#Fix misspelled county names
dat$Subproject<-revalue(dat$Subproject, c("Allegheny County"="Alleghany County"))

################## Fix/remove any incorrect timestamps #############################

#Resaves as a new object in case we need to backtrack to the old
#object
z<-dat

#Clean out rows where camera time not set properly
#The project started in 2016 and it is now 2018 so remove all other
#years
z$Date<-as.Date(z$Begin, format="%Y-%m-%d")
summary(z$Date)
z$Year<-as.numeric(format(z$Date, format="%Y"))
z<-subset(z, Year> "2016" & Year < "2019")
summary(z$Date)

#Calculate start and end dates for each camera
detach("package:plyr")
library(dplyr)
z <- z%>%group_by(Deployment.Name)%>%mutate(Start.Date=min(Begin2, na.rm = T),End.Date=max(End2, na.rm=T))
z<-as.data.frame(z)
names(z)
summary(z$Start.Date)
z$Total.days.Sampled <- difftime(z$End.Date, z$Start.Date, units = "days") # calculate how long the camera was sampling, in days.
names(z)
z$Total.days.Sampled

library(plyr)

################## Check that the Species names are correct #######################
#Double check the common names and species names to make sure the data does not have species not present in that area
unique(z$Common.Name)

#Fix any animal names that are incorrectly spelled
z$Common.Name[z$Common.Name == 'Elk aka Red Deer'] <- 'Elk_Red Deer'
z$Common.Name[z$Common.Name == 'Unknown Flying Squirrel'] <- 'Southern Flying Squirrel'

#Remove animals that you do not want in your data
#This remove_spp removes all but wild mammals, edit as needed
remove_spp<-("Wild Turkey|Camera Trapper|Domestic Dog|Domestic Cat|American Crow|Human non-staff|Other Bird species|Unknown Bird|Unknown Coati_Raccoon|No Animal|Unknown Animal|Vehicle|Calibration Photos|Bicycle|Animal Not on List|Time Lapse|Raptor species|Owl species|Unknown Canid|Unknown Rabbit_Hare|Reptile species|Unknown Squirrel|Blue Jay|Unknown Felid|Unknown Small Rodent|Domestic Horse|Domestic Cow|Northern Bobwhite|Ruffed Grouse|Common Raven")
df<-z[grep(remove_spp, z$Common.Name, invert=T),] 
unique(df$Common.Name)

###### Assign counties into habitat regions (piedmont, mtns, coast, sandhills) ######
coastal<-("Camden County|Gates County|Pender County|New Hanover County|Beaufort County|Martin County|Pitt County|Greene County|Wilson County|Wayne County|Lenoir County|Robeson County|Johnston County|Brunswick County|Columbus County|Bladen County|Onslow County|Sampson County|Carteret County|Duplin County|Jones County|Craven County|Cumberland County|Pamlico County|Hyde County|Dare County|Tyrrell County|Edgecombe County|Washington County|Nash County|Bertie County|Chowan County|Halifax County|Perquimans County|Currituck County|Pasquotank County|Northampton County|Hertford County")
sandhills<-("Harnett County|Moore County|Hoke County|Scotland County|Richmond County|Lee County")
mountains<-("Jackson County|Transylvania County|Macon County|Cherokee County|Clay County|Henderson County|Graham County|Haywood County|Swain County|Buncombe County|Madison County|Yancey County|Mitchell County|Avery County|Watauga County|Ashe County|Alleghany County|McDowell County")
piedmont<-("Iredell County|Wilkes County|Union County|Cabarrus County|Wake County|Franklin County|Durham County|Orange County|Chatham County|Alamance County|Anson County|Mecklenburg County|Montgomery County|Stanly County|Gaston County|Cleveland County|Rutherford County|Polk County|Lincoln County|Rowan County|Davidson County|Randolph County|Catawba County|Burke County|Davie County|Caldwell County|Alexander County|Guilford County|Forsyth County|Granville County|Yadkin County|Vance County|Warren County|Person County|Caswell County|Rockingham County|Surry County|Stokes County")

coastal.df<-as.data.frame(df[grep(coastal, df$Subproject, invert=F),])
sandhills.df<-as.data.frame(df[grep(sandhills, df$Subproject, invert=F),])
mountains.df<-as.data.frame(df[grep(mountains, df$Subproject, invert=F),])
piedmont.df<-as.data.frame(df[grep(piedmont, df$Subproject, invert=F),])

coastal.df$Region <-c("Coastal Plain")
sandhills.df$Region<-c("Sandhills")
mountains.df$Region<-c("Mountains")
piedmont.df$Region<-c("Piedmont")

df2<-as.data.frame(rbind(coastal.df, sandhills.df, mountains.df, piedmont.df))
names(df2)
df2$Region

############## Turn the time into radians for activity plots ######################
#We do this since time (daily activity) is circular!

#Splits the time stamp into Date and Time Columns and names the new columns
df2$Time <- format(as.POSIXct(df2$Begin2) ,format = "%H:%M:%S")

#Splits new time into hour, minutes and seconds and names the new
#columns
setDT(df2)[,paste0("Times",1:3):= tstrsplit(Time,":")]
df2$hours<-as.numeric(df2$Times1)
df2$mins<-as.numeric(df2$Times2)
df2$seconds<-as.numeric(df2$Times3)

#Converts all time to minutes then combines into total minutes
df2$hours<-df2$hours * 60
df2$seconds<-df2$seconds / 60
df2$totalminutes<-df2$hours + df2$mins + df2$seconds

#Calculates the time "position" of each detection over the deployment
df2<-as.data.frame(df2%>%group_by(Deployment.Name)%>%mutate(totalminutes2=(df2$totalminutes-min(df2$totalminutes))/(diff(range(df2$totalminutes)))))

#Converts time "position" to radians
df2$radians <- df2$totalminutes2 * 2 * pi
df2$radians
################################################################################## 
#                   Step 3: Define time blocks (2 months)                        #
#                                                                                #
##################################################################################

#Resaves as a new object
ss<-df2

#First find the min and max of the study
min(ss$Date)
max(ss$Date)

#Set the number of 2 month (60 days) intervals in the study period
n<-(as.numeric(max(ss$Date)-min(ss$Date))/60)+1
n<-round(n, 0)
n
y<-seq(from=60, to=(60*n), by=60)
y

#Split the data into those 2 month periods and mark each period in a column
#called "Block"
t<-vector("list", length(y))
t[[1]]<-ss[ss$Date >= min(ss$Date) & ss$Date <= (min(ss$Date)+60),]
t[[1]]$Block<-c("1")
for (i in 2:length(y)){
  t[[i]]<-ss[ss$Date > (min(ss$Date)+y[i-1]) & ss$Date <= (min(ss$Date)+y[i]),]
  if(nrow(t[[i]])<1){is.null(t[[i]])}
  else{t[[i]]$Block<-c(i)}
}  

#Recombine the list into a data frame
df3 <- do.call("rbind", t)
df3<-as.data.frame(df3)
names(df3)
head(df3, 15)
unique(df3$Block)

################################################################################## 
#                   Step 4: Choose the species                                   #
#                                                                                #
##################################################################################
#the name must match this list:
unique(df3$Common.Name)

#For single species, remove the # before "species" below
#species<-("Coyote")

#for multiple species:
species<-c("Coyote", "Grey Fox")

df.sp <- as.data.frame(subset(df3, Common.Name %in% species))

#################################################################################
#       Step 5: Make Activity (time of day) graphs for 2 month periods          #
#                                                                               #            
#################################################################################
#First define the species and then make the plot
species1<-c("Coyote")
species2<-c("Grey Fox")
df.sp1 <- as.data.frame(subset(df3, Common.Name %in% species1))
df.sp2 <- as.data.frame(subset(df3, Common.Name %in% species2))

########################## Make the Plot ########################################
#Split the data into a list to loop through and create lists to save output
tt1<-split(df.sp1, list(df.sp1$Block, df.sp1$Region))
tt2<-split(df.sp2, list(df.sp2$Block, df.sp2$Region))

#Remove elements with just 5 rows since the low sample size will
#not give accurate results
for(i in names(tt1)){if (nrow(tt1[[i]]) < 6) {tt1[[i]]<-NULL}}
for(i in names(tt2)){if (nrow(tt2[[i]]) < 6) {tt2[[i]]<-NULL}}

dp_sp1<-list()
dp_sp2<-list()

#Loop through the 2 month periods, making an activity pattern for each species and
#saving to a file in your working directory
for (i in names(tt1)){
  for (j in names(tt2)){
  dp_sp1[[i]]<-densityPlot(tt1[[i]]$radians, xscale = 24, xcenter = "noon", add = FALSE, rug = FALSE, 
              extend = NULL, n.grid = 100, adjust = 0.2, main= paste(species1, i, min(tt1[[i]]$Date), "-", max(tt1[[i]]$Date), sep=" "),
              xlab = "Time", ylab = "Density")
  dev.copy(jpeg,filename=paste(species1, i,"_densityPlot.jpg", sep=""))
  dev.off ()
  
  dp_sp2[[j]]<-densityPlot(tt2[[j]]$radians, xscale = 24, xcenter = "noon", add = FALSE, rug = FALSE, 
                           extend = NULL, n.grid = 100, adjust = 0.2, main= paste(species2, j, min(tt2[[j]]$Date), "-", max(tt2[[j]]$Date), sep=" "),
                           xlab = "Time", ylab = "Density")
  dev.copy(jpeg,filename=paste(species2, j,"_densityPlot.jpg", sep=""))
  dev.off ()
  
  }
}

#Now do the same as above but put them on the same plot
#Use only elements with both species present
tt1p<-tt1[names(tt1) %in% names(tt2)]
  
for (i in names(tt1p)){
    dp_sp1[[i]]<-densityPlot(tt1p[[i]]$radians, xscale = 24, xcenter = "noon", add = FALSE, rug = FALSE, 
                             extend = NULL, n.grid = 100, adjust = 0.2, main= paste(unique(tt1p[[i]]$Region), min(tt1p[[i]]$Date), "-", max(tt1p[[i]]$Date), sep=" "),
                             xlab = "Time", ylab = "Density")
    densityPlot(tt2[[i]]$radians, add = TRUE, col = 'blue')
    legend ('bottomleft', c(species1,species2), lty = 1, col = c('black','blue'))
    dev.copy(jpeg,filename=paste(species1, species2, i,"_densityPlot.jpg", sep=""))
    dev.off ()
    
  }

################################################################################
#         Step 6: Determing species activity pattern overlap                   #
#                                                                              #
################################################################################
#Plot the density of two species in relation to time while highlighting periods
#of overlap
ol_plt<-list()
ol_est<-NA

for (i in names(tt1p)){
    ol_plt[[i]]<-overlapPlot(tt1p[[i]]$radians, tt2[[i]]$radians, 
             main = paste(species1, "and", species2, "Overlap\n",
                          min(tt2[[i]]$Date), "-", max(tt2[[i]]$Date), "\n", i, sep=" "))
    legend ('top',c(species1, species2), lty = c(1,2), col = c(1,4), bty = 'n')
    dev.copy(jpeg,filename=paste("Overlap_", species1, species2, i, ".jpg", sep=""));
    dev.off ()
    #Calculates estimated activity pattern overlap (%) based on times of observation 
    #for 2 species 
    ol_est[i]<-overlapEst(tt1p[[i]]$radians, tt2[[i]]$radians, type=c("Dhat1"))
}

as.data.frame(ol_est)
