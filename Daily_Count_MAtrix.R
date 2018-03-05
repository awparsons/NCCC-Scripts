#########################################################################
######################### Create Daily Count Matrix #####################
#########################################################################
require(dplyr)
require(plyr)
require(reshape2)
require(lubridate)
require(reshape)

#Set the time zone of the collected data
ols.sys.timezone <- Sys.timezone()
Sys.setenv(TZ = 'GMT')

#Set the capture period (minute, hour, day, week, month)
#Set in terms of day, so day=1, hour=1/24, minute=1/(24*60) and week=7
#In this example I will use a by-minute capture period
day <- 1

#Set the number of seconds in the sample period
#for a one day sample period, would be 60*60*24 for example
day.sec <- 60*60*24

########### Load the data downloaded from the eMammal website ###########

#set the appropriate working directory
setwd("~")  

#Read in the downloaded file
df <- read.csv("sianctapi-selected-observations-5a04a73af3264.csv", stringsAsFactors=FALSE)

######################## Fix the timestamps ###############################
# Remove any data with no dates
df[df$Begin.Time=="",] <- NA
df <- df[!is.na(df$Begin.Time),]

#Replace the Ts in the timestamps with a space
df$Begin <- gsub("T", " ", df$Begin.Time)
df$End <- gsub("T", " ", df$End.Time)

#Format the times as POSIXct, the format that R uses for times
df$Begin2 <- as.POSIXct(as.character(df$Begin, format = "%Y-%m-%d %H:%M:%S"))
df$End2 <- as.POSIXct(as.character(df$End, format = "%Y-%m-%d %H:%M:%S"))
str(df$Begin2)
str(df$End2)

############### Calculate Start and End dates for each camera ############
z <- arrange(df, Deployment.Name, Begin2)
z <- group_by(z, Deployment.Name)


start.dates <- filter(z, Begin2 == min(Begin2, na.rm = T))[,c("Deployment.Name", "Begin2")]
colnames(start.dates) <- c("Deployment.Name", "Start.Date")

end.dates <- filter(z, Begin2 == max(Begin2, na.rm = T))[,c("Deployment.Name", "Begin2")]
colnames(end.dates) <- c("Deployment.Name", "End.Date")

#Remove duplicates (rarely there are 2 or more events with same first or last Begin2)  
start.dates <- start.dates[!duplicated(start.dates$Deployment.Name),]
end.dates <- end.dates[!duplicated(end.dates$Deployment.Name),]

#Merge back into the original df
df <- merge(df, start.dates, by = "Deployment.Name", all.x = T, suffixes = '')
df <- merge(df, end.dates, by = "Deployment.Name", all.x = T, suffixes = '')

#Calculate the number of days/minutes/hours each camera was working
#Set the units argument as needed, "weeks", days", "hours", "mins", "secs"
df$Total.days.Sampled <- difftime(df$End.Date, df$Start.Date, units = "mins")

################## Make a list of all existing deployments ###############

cams.list <- unique(as.character(df$Deployment.Name))

########### Select the species to make the capture history for ###########

species <- "Coyote"

#Subset the data for just detections of that species
df.sp <- subset(df, Common.Name == species)

####### Calculate Sample Period for each observation for each Camera ####

#Create sampling period IDs
z <- df.sp
z$Deployment.Name <- as.character(z$Deployment.Name)

z$SamplePeriod <- NA

for (i in unique(z$Deployment.Name)){
  print(i)
  x <- z[z$Deployment.Name == i,]
  
  z[z$Deployment.Name == i,]$SamplePeriod <- cut(x$Begin2, breaks = as.POSIXct(seq(from = min(x$Start.Date, na.rm = T), by = day.sec, to = max(x$End.Date, na.rm = T) + day.sec)), labels = F)
}

max(z$SamplePeriod)
summary(z$SamplePeriod)

#Remove all columns but Deployment.Name and SamplePeriod
df8 <- z[ -c(2:(ncol(z)-1)) ]

################## Add in missing deployment names for ##################
############## deployments that did not detect the species ##############
########################### of interest #################################

# Find names of missing cams
Missing_cam <- setdiff(cams.list,df8$Deployment.Name) 

#Mark them with 0s as SamplePeriod which can be easily removed later
M_C<-as.data.frame(cbind(Missing_cam, rep(0, length(Missing_cam))))
names(M_C)<-c("Deployment.Name", "SamplePeriod")

#Add them into the dataframe with the detection data
df4<-rbind(df8, M_C)

#Sort by Deployment
df4<-df4[order(df4$Deployment.Name), ]

#################### Create the capture history matrix ###################
########################## Just like a Pivot Table #######################

#Reshape the data using melt
transform=melt(df4, id.vars="Deployment.Name")
pivot=cast(transform, Deployment.Name ~ value)

#Delete the first sample period (0) we made when adding the rest of the
#Deployments
pivot<- pivot[ -c(2) ] 
pivot[is.na(pivot)]=0

#Get everything ready to output the CH find length of history
df4$SamplePeriod<-as.numeric(df4$SamplePeriod)

######### Add any missing hours (or days, minutes etc.) of data ##########

#First create the full sequence of hours from beginning to end of the 
#study that should be accounted for
occStr <- seq(1,max(df4$SamplePeriod),1);
occStr <- as.character(occStr); 
occStr <- c("Deployment.Name",occStr); # pre-pend tag onto occStr

#Then find the names of missing time columns
Missing_time <- setdiff(occStr,names(pivot)) 

#Add them to the to the dataframe, filled with 0s and set in ordinal
#sequence
pivot[Missing_time] <- 0
pivot <- pivot[occStr]

############### Add "NA"s where cameras were not running ###############

#First create a list of the maximum Sample Period for each camera in the
#entire dataset, this will be larger than the last time we calcualted
#this since that was only for a specific species
z2 <- df
z2$Deployment.Name <- as.character(z2$Deployment.Name)

z2$SamplePeriod <- NA

for (i in unique(z2$Deployment.Name)){
  print(i)
  x <- z2[z2$Deployment.Name == i,]
  
  z2[z2$Deployment.Name == i,]$SamplePeriod <- cut(x$Begin2, breaks = as.POSIXct(seq(from = min(x$Start.Date, na.rm = T), by = day.sec, to = max(x$End.Date, na.rm = T) + day.sec)), labels = F)
}

max(z2$SamplePeriod)
summary(z2$SamplePeriod)

df5 <- z2[ -c(2:(ncol(z2)-1)) ]

max_period<-aggregate(.~ Deployment.Name,data=df5, FUN= max)
max_period

#Then for each title, insert "NA" for each Sample Period greater than
#the maximum
pivot2<-pivot
for(j in 2:ncol(pivot2)){
  for(i in 1:nrow(pivot2)){
    if (j > max_period[i,2]){
      pivot2[i,j]<-"NA"}
  }}

#Save for later use
write.csv(pivot2, file=paste("DailyCountCH_", species, ".csv", sep=""))
