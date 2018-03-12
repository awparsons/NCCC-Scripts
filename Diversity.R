##################################################################################
#                        Diversity and Species Richness                          #
##################################################################################

#Load the necessary libraries
library(iNEXT)
library(dplyr)
library(reshape2)
library(ggplot2)

#Set the appropriate working directory 
setwd("C:/Users/Arielle/Desktop/NRC/eMammal/WRC/Scripts/Scripts")

#Load the summary datasets created in the DataCleaning script
dat <-read.csv(file="eMammaldata.csv")
covs<-read.csv(file="SiteInfo.csv")

#Remove entries that are not wild mammal species
remove_spp<-("Domestic Horse|Domestic Cow|Ruffed Grouse|Domestic Dog|Domestic Cat|Wild Turkey|Camera Trapper|No Animal|Unknown Animal|Vehicle|Unknown Squirrel|Unknown Small Rodent|Unknown Rabbit_Hare|Unknown Flying Squirrel|Unknown Felid|Unknown Coati_Raccoon|Unknown Canid|Unknown Bird|Time Lapse|Reptile species|Raptor species|Owl species|Other Bird species|Northern Bobwhite|Human non-staff|Common Raven|Calibration Photos|Blue Jay|Bicycle|Animal Not on List|American Crow")
dat$cn<-as.character(dat$Common.Name)
df<-dplyr::filter(dat, !grepl(remove_spp,cn))
unique(df$Common.Name)

#Sum the count of each species in each deployment
summ<-df%>%group_by(Deployment.Name, Common.Name)%>%summarize(summ=sum(Count))

#Reshape the data
transform=melt(summ, id.vars=c("Deployment.Name", "Common.Name"))
pivot=dcast(transform, formula=Deployment.Name + value~Common.Name)

#Change NAs to 0, now you should have a pivot table
pivot[is.na(pivot)]<-0
pivot

#Convert counts to 1 or 0
pivot[3:28][pivot[3:28] > 1] <- 1

#Combine with the site-specific covariates
df2<-merge(pivot, covs, by="Deployment.Name")

#Assign counties into habitat regions (piedmont, mtns, coast, sandhills)#
coastal<-("Camden County|Gates County|Pender County|New Hanover County|Beaufort County|Martin County|Pitt County|Greene County|Wilson County|Wayne County|Lenoir County|Robeson County|Johnston County|Brunswick County|Columbus County|Bladen County|Onslow County|Sampson County|Carteret County|Duplin County|Jones County|Craven County|Cumberland County|Pamlico County|Hyde County|Dare County|Tyrrell County|Edgecombe County|Washington County|Nash County|Bertie County|Chowan County|Halifax County|Perquimans County|Currituck County|Pasquotank County|Northampton County|Hertford County")
sandhills<-("Harnett County|Moore County|Hoke County|Scotland County|Richmond County|Lee County")
mountains<-("Jackson County|Transylvania County|Macon County|Cherokee County|Clay County|Henderson County|Graham County|Haywood County|Swain County|Buncombe County|Madison County|Yancey County|Mitchell County|Avery County|Watauga County|Ashe County|Alleghany County|McDowell County")
piedmont<-("Iredell County|Wilkes County|Union County|Cabarrus County|Wake County|Franklin County|Durham County|Orange County|Chatham County|Alamance County|Anson County|Mecklenburg County|Montgomery County|Stanly County|Gaston County|Cleveland County|Rutherford County|Polk County|Lincoln County|Rowan County|Davidson County|Randolph County|Catawba County|Burke County|Davie County|Caldwell County|Alexander County|Guilford County|Forsyth County|Granville County|Yadkin County|Vance County|Warren County|Person County|Caswell County|Rockingham County|Surry County|Stokes County")

coastal.df2<-as.data.frame(df2[grep(coastal, df2$Subproject, invert=F),])
sandhills.df2<-as.data.frame(df2[grep(sandhills, df2$Subproject, invert=F),])
mountains.df2<-as.data.frame(df2[grep(mountains, df2$Subproject, invert=F),])
piedmont.df2<-as.data.frame(df2[grep(piedmont, df2$Subproject, invert=F),])

coastal.df2$Region <-c("Coastal Plain")
sandhills.df2$Region<-c("Sandhills")
mountains.df2$Region<-c("Mountains")
piedmont.df2$Region<-c("Piedmont")

df2<-as.data.frame(rbind(coastal.df2, sandhills.df2, mountains.df2, piedmont.df2))

##################### Estimate Diversity and Richness #########################
#Overall
df3<-df2[-c(1:2,29:39)]
df3<-as.matrix(df3)
df3<-t(df3)
df3<-as.incfreq(df3)
rar_all<-iNEXT(df3, q=0, datatype="incidence_freq")
rar_all$AsyEst#these are the estimates

#By Region
df4<-split(df2, list(df2$Region))
df4<- lapply(df4, function(x)x[ ,-c(1:2,29:39) ])
df4<- lapply(df4, function(x)as.matrix(x))
df4<- lapply(df4, function(x)t(x))
df4<-lapply(df4, as.incfreq)
rar_reg<-iNEXT(df4, q=0, datatype="incidence_freq")
rar_reg$AsyEst

#Making graphs
#First source the necessary function (separate script file)
source("ggiNEXT_fun.R")

#Richness
plt<-ggiNEXT.iNEXT(rar_reg)
plt2<-plt + theme_classic() + 
  theme(legend.position = "top", legend.title=element_blank())+
  theme(text=element_text(size=12, colour="black"))+
  ylab("Species richness")+
  theme(axis.line.x = element_line(color="black", size=1),
        axis.line.y = element_line(color="black", size=1),
        axis.text.y = element_text(color="black", size=12),
        axis.ticks.x=element_blank())

ggsave('richness.jpg', plt2, width = 18, height = 14, units = 'cm')

#Shannon Diveristy
div<-rar_reg$AsyEst
div<-div[div$Diversity=="Shannon diversity",]

plt3<-ggplot(div, aes(x = Site, y = log(Estimator)))+
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", fill="steel blue",
           size=.3) +
  geom_errorbar(aes(ymin=log(LCL), ymax=log(UCL)),
                size=0.3,
                width=.2,
                position=position_dodge(.9))+
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank()) +
  theme(axis.text = element_text(colour="black", size = 12))+
  xlab("Habitat") + ylab("Shannon diversity") +
  theme(text = element_text(size = 12, colour="black"))

ggsave('Shannon_diversity.jpg', plt3, width = 18, height = 14, units = 'cm')