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
if(length(new.packages))installed.packages(new.packages)

#Now load the needed packages
library(dplyr)
library(plyr)
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
dat <-read.csv(file="sianctapi-selected-observations-5a04a73af3264.csv")
names(dat)#these are the column names
unique(dat$Subproject)#these are the counties in the dataset

#Fix the timestamps
dat$Begin <- gsub("T", " ", dat$Begin.Time)  # This will make a new column called "Begin" with just date and time separated
dat$End <- gsub("T", " ", dat$End.Time)
ols.sys.timezone <- Sys.timezone() #Setting timezone
Sys.setenv(TZ = 'EST') #Setting timezone --- "EST" is eastern timezone
dat$Begin2 <- as.POSIXct(dat$Begin, format = "%Y-%m-%d %H:%M:%S") #This formats the date and time to desired formats
dat$End2 <- as.POSIXct(dat$End, format = "%Y-%m-%d %H:%M:%S")

#Fix misspelled county names
dat$Subproject<-revalue(dat$Subproject, c("Allegheny County"="Alleghany County"))

################## Fix/remove any incorrect timestamps #############################
z<-dat
#Calculate start and end dates for each camera
start.dates <- filter(z, Begin2 == min(Begin2, na.rm = T))[,c("Deployment.Name", "Begin2")]
colnames(start.dates) <- c("Deployment.Name", "Start.Date")

end.dates <- filter(z, Begin2 == max(Begin2, na.rm = T))[,c("Deployment.Name", "Begin2")]
colnames(end.dates) <- c("Deployment.Name", "End.Date")

z <- merge(z, start.dates, by = "Deployment.Name", all.x = T, suffixes = '')
z <- merge(z, end.dates, by = "Deployment.Name", all.x = T, suffixes = '')
z$Total.days.Sampled <- difftime(z$End.Date, z$Start.Date, units = "days") # calculate how long the camera was sampling, in days.

#Clean out rows where camera time not set properly
times <- c("2012-01-01 00:00:00", "2013-01-01 00:00:00", 
           "2014-01-01 00:00:00", "2015-01-01 00:00:00", 
           "2016-01-01 00:00:00", "2017-01-01 00:00:00", 
           "2018-01-01 00:00:00", "2019-01-01 00:00:00", 
           "2020-01-01 00:00:00", "2021-01-01 00:00:00")

#If a deployment has one of the times above, remove any 
#data with different years than the max year
`%notin%` <- function(x,y) !(x %in% y) 
z$TI<-z$Begin %notin% times
z$Date<-as.Date(z$Begin, format="%Y-%m-%d")
z$Year<-format(z$Date, format="%Y")
z$RM<-NA
z<-z%>%filter(Year<2018)
p<-z %>%group_by(Deployment.Name)%>%mutate(RM=(ifelse(any(TI=="FALSE") & Year < max(Year),
                                                         "remove", "keep")))
z<-filter(p, RM=="keep")

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

############## Turn the time into radians for activity plots ######################
setDT(df2)[,paste0("Begin3",1:2):= tstrsplit(Begin," ")] #Splits the time stamp into Date and Time Columns
setnames(df2, old = c('Begin31','Begin32'), new = c('Date','Time')) #Renames the new columns 

setDT(df2)[,paste0("Times",1:3):= tstrsplit(Time,":")]
df2$hours<-as.numeric(df2$Times1)
df2$mins<-as.numeric(df2$Times2)
df2$seconds<-as.numeric(df2$Times3)

df2$hours<-df2$hours * 60
df2$seconds<-df2$seconds / 60
df2$totalminutes<-df2$hours + df2$mins + df2$seconds

#This gives you a warning but works anyway
df2$totalminutes2<-apply(df2, MARGIN=2, 
                         FUN=function(x) (df2$totalminutes-min(df2$totalminutes))/diff(range(df3$totalminutes)))

df2$radians <- df2$totalminutes2 * 2 * pi  #This turns total minutes into radians
#Time is now converted into radians

################################################################################## 
#                   Step 3: Define time blocks (2 months)                        #
#                                                                                #
##################################################################################
ss<-df2

#First find the min and max of the study
min(ss$Date)
max(ss$Date)

#Set the number of 2 month intervals in the study period
n<-(as.numeric(max(ss$Date)-min(ss$Date))/60)+1
n<-round(n, 0)
y<-seq(from=60, to=(60*n), by=60)

#Split the data into those 2 month periods and mark each period in a column
#called "Block"
t<-list()
t[[1]]<-ss[ss$Date >= min(ss$Date) & ss$Date <= (min(ss$Date)+60),]
t[[1]]$Block<-c("1")
for (i in 2:length(y)){
  t[[i]]<-ss[ss$Date > (min(ss$Date)+y[i-1]) & ss$Date <= (min(ss$Date)+y[i]),]
  t[[i]]$Block<-c(i)
}  

#Recombine the list into a data frame
df3 <- do.call("rbind", t)
df3<-as.data.frame(df3)

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

#Add list of empty cameras back to species subset
`%notin%` <- function(x,y) !(x %in% y) 
cam<-subset(df3, !(Deployment.Name %in% df.sp$Deployment.Name))
cam$Count[cam$Count>0]  <- 0
cam$Common.Name[which(cam$Common.Name %notin% species)] <- NA
ss<-rbind(df.sp, cam)

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

#Remove elements with just 5 rows
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
    legend ('bottomleft', c(species1,species2), lty = 1, col = c('black','blue'), bg = 'white')
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
    ol_plt[[i]]<-overlapPlot (tt1p[[i]]$radians, tt2[[i]]$radians, 
             main = paste(species1, "and", species2, "\nActivity Pattern Overlap\n",
                          min(tt2[[i]]$Date), "-", max(tt2[[i]]$Date), sep=" "))
    legend ('top',c(species1, species2), lty = c(1,2), col = c(1,4), bty = 'n')
    dev.copy(jpeg,filename=paste(species1, species2, i, "_temporaloverlap.jpg", sep=""));
    dev.off ()
    #Calculates estimated activity pattern overlap (%) based on times of observation 
    #for 2 species 
    ol_est[i]<-overlapEst(tt1p[[i]]$radians, tt2[[i]]$radians, type=c("Dhat1"))
}

as.data.frame(ol_est)
