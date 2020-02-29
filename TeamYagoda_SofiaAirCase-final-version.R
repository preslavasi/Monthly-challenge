## Week 1-------
#Step 1: Importing data ----
#setwd("D:\\Preslava\\uni\\Business Analytics\\Tuesday class\\Monthly Challenge Oct\\Air Tube")
setwd("C:\\Users\\Daria\\Desktop\\business analytics\\tuesday\\data for the air case\\Air Tube")

air18=read.csv("data_bg_2018.csv", na.string=c("","NA"," "), stringsAsFactors = F)
air17=read.csv("data_bg_2017.csv", na.string=c("","NA"," "), stringsAsFactors = F)
#sofiatopo=read.csv("sofia_topo.csv", na.string=c("","NA"," "), stringsAsFactors = F)
sofiatopo=read.csv("C:\\Users\\Daria\\Desktop\\business analytics\\tuesday\\data for the air case\\TOPO-DATA\\sofia_topo.csv", na.string=c("","NA"," "), stringsAsFactors = F)

#Step 2: Inspecting elements of the datasets and checking for inconsistencies ----
dataclass<-list()

dataclass$air17<-data.frame(names(air17))
colnames(dataclass$air17)="Name"
dataclass$air17$Name<-sapply(air17,class)

dataclass$air18<-data.frame(names(air18))
colnames(dataclass$air18)="Name"
dataclass$air18$Name<-sapply(air18,class)

dataclass$sofiatopo<-data.frame(names(sofiatopo))
colnames(dataclass$sofiatopo)="Name"
dataclass$sofiatopo$Name<-sapply(sofiatopo,class)

# We've noticed that the time class is character, which should be converted to one of the suitable classes.
#install.packages("lubridate")
library(lubridate)

air17$time=as_datetime(air17$time, tz="UTC")
class(air17$time)
air18$time=as_datetime(air18$time, tz="UTC")
class(air18$time)

#Step 3: Finding unique stations and merging the number of observations by stations----
#install.packages("dplyr")
library(dplyr)
uniquestations=list()
uniquestations$geo17=air17 %>%
  group_by(geohash) %>%
  summarise(obs=n())
uniquestations$geo18=air18 %>%
  group_by(geohash) %>%
  summarise(obs=n())

# Getting differences in the geohashes for both years

length(setdiff(uniquestations$geo18$geohash,uniquestations$geo17$geohash)) 
length(setdiff(uniquestations$geo17$geohash,uniquestations$geo18$geohash)) 
(aux=setdiff(uniquestations$geo17$geohash,uniquestations$geo18$geohash))
sum(uniquestations$geo17$obs[uniquestations$geo17$geohash %in% aux]) 

# Cleaning differences (only stations present both in 2017 and 2018 left)
air17=air17[!(air17$geohash %in% aux),]

# Merging data sets and cleaning up auxiliary variables
merged=bind_rows(air17,air18)
rm(air17,air18,uniquestations,aux,dataclass)

#Step 4: Summarizing availability of merged datasets----
# Checking for NAs.

sum(is.na(merged$geohash))#[1] 4
sum(merged$geohash == "") 
merged=merged[!(is.na(merged$geohash)==T),]

# Getting summary data about the stations
stations=merged %>% group_by(geohash) %>%
  summarise(obs=n(), tmin=min(time), tmax=max(time), day=tmax-tmin) %>%
  arrange(day,obs,geohash)

#Step 5: Summarizing stations and visualizing them on a map----
#install.packages("geohash")

library(geohash)
stations[,6:9]<-gh_decode(stations$geohash)

#install.packages("rworldmap")
library(rworldmap)
newmap=getMap(resolution="low")
windows()
plot(newmap, xlim=c(23.8,27),ylim=c(41,44.5),asp=1, main = "Stations allocation within Bulgaria")
points(sofiatopo$Lon,sofiatopo$Lat,col="brown",cex=2.2, pch=17)
points(stations$lng,stations$lat,col="light green",cex=0.75,pch=23)
dev.copy(tiff ,filename="Bulgaria_map.tif") 
dev.off()

#Step 6: Filtering stations by space and time----
# Plotting histogramm and a boxplot after filtering stations by their location

colnames(sofiatopo)[1:2]=c("lat","lng")
stationsf=stations %>%
  dplyr::filter(lat<=max(sofiatopo$lat) & lat>=min(sofiatopo$lat) & lng<=max(sofiatopo$lng) & lng>=min(sofiatopo$lng))
layout(matrix(c(1,2), nrow=1, ncol=2))
hist(as.numeric(stationsf$day), col="light green", main = "Frequency of observations", xlab = "Days")
boxplot(as.numeric(stationsf$day), col="light blue")

# Taking a look at the discriptive statistics.

library(fBasics)
stationsf$day=as.numeric(stationsf$day)
DS=basicStats(stationsf[,c(2,5)])
quantile(stationsf$day, probs=seq(0,1,0.05))
quantile(stationsf$obs, probs=seq(0,1,0.05))

# Filtering the stations by time (with data for 10 days or less)

stationsf10d=stationsf %>%
  dplyr::filter(day<=10 & obs<=168)
stations=stationsf[!(stationsf$geohash %in% stationsf10d$geohash),]
stfinal=merged[merged$geohash %in% stations$geohash,]

rm(merged,stationsf,stationsf10d)
save(list=ls(),file="week1.RData")

## Week 2-----------
setwd("C:\\Users\\Daria\\Desktop\\business analytics\\tuesday\\data for the air case\\Air Tube")
load("week1.RData")

#Step 1: Final list of geo-units----
library(geosphere)

# General idea: looking for the closest points to the stations for clustering them by that point.
# creating auxiliary table, which illustrates the distance between particular stations (rows) and Sofia's topo points (columns) 
sofiatopo$pnt<-c(1:196)
aux<-data.frame(matrix(NA,nrow=dim(stations)[1],ncol=dim(sofiatopo)[1])) 
for (i in 1:dim(sofiatopo)[1]){
  for (j in 1:dim(stations)[1]){
    aux[j,i]=distGeo(stations[j,6:7],sofiatopo[i,1:2])
  }
}
# Preparing stations data for clustering
stations$pnt<-apply(aux,1,which.min)
stations<-merge(stations, sofiatopo[,c(3:4)], by="pnt")
stations=stations[,c(2,1,11,3:10)]

# Visualizing stations in Sofia for clustering by point
windows()
plot(newmap, xlim=c(min(sofiatopo$lng), max(sofiatopo$lng)), ylim=c(min(sofiatopo$lat), max(sofiatopo$lat)), asp=1, main = "Stations within boundaries of Sofia")
points(sofiatopo$lng,sofiatopo$lat,col='light grey',cex=1.5,pch=19)
points(stations$lng,stations$lat,col='blue',cex=1,bg = 'light blue', pch=25)
#dev.copy(tiff ,filename="Sofia_topo_and_stations.tif") 
#dev.off()

#Step 2: Merging cluster information and summarizing the datasets----
df=merge(stations[,c(1:7)], stfinal, by="geohash")
rm(aux,stfinal,i,j)

# actually clustering and summarising parameters
library(dplyr)
dfgeo=df %>%
  group_by(pnt) %>%
  summarise(elev=unique(Elev),st.num=length(unique(geohash)), obs=n(),t.min=min(time),t.max=max(time),p1.min=min(P1),p1.max=max(P1), p2.min=min(P2),p2.max=max(P2), temp.min=min(temperature),temp.max=max(temperature),press.min=min(pressure),press.max=max(pressure),hum.min=min(humidity),hum.max=max(humidity))%>%
  arrange(pnt)

colnames(dfgeo)[1]="clust.ID"
colnames(df)[2]="clust.ID"
colnames(stations)[2]="clust.ID"

#Step 3: Cleaning data of mismeasurements and inconsistencies, comparing them with the official data.---- 

sum(is.na(df)) #[1] 0
#setwd("D:\\Preslava\\uni\\Business Analytics\\Tuesday class\\Monthly Challenge Oct\\EEA Data")
setwd("C:\\Users\\Daria\\Desktop\\business analytics\\tuesday\\data for the air case\\euromeasurements")

# Importing official data from EEU for 2017 and 2018.
#eeu=list.files(path="D:\\Preslava\\uni\\Business Analytics\\Tuesday class\\Monthly Challenge Oct\\EEA Data")
eeu=list.files(path="C:\\Users\\Daria\\Desktop\\business analytics\\tuesday\\data for the air case\\euromeasurements")
deu=lapply(eeu,read.csv,na.string=c("","NA"," "), stringsAsFactors = F, fileEncoding="UTF-16LE")
View(deu[[1]])

# Giving names to lists from the folder
for(i in 1:length(eeu)){
  eeu[i]=gsub("BG_5_","st",eeu[i])
  eeu[i]=gsub("_timeseries.csv","",eeu[i])
  names(deu)[i]=eeu[i]
}
names(deu)
rm(eeu,i)

# Importing the official weather data.
setwd("C:\\Users\\Daria\\Desktop\\business analytics\\tuesday\\data for the air case\\METEO-data")
#setwd("D:\\Preslava\\uni\\Business Analytics\\Tuesday class\\Monthly Challenge Oct\\METEO-data")
weather=read.csv("lbsf_20120101-20180917_IP.csv",na.string=c("","NA"," ","-9999"), stringsAsFactors = F)
weather = weather[weather$year>=2017,]

#Here we are identifying min and max results for the variables from df data frame.
cc=rep(NA,8)
names(cc)=c("P1min","P1max","Tmin","Tmax","Hmin","Hmax","Pmin","Pmax")

cc["P1min"]=min(sapply(deu,function(x,y) min(y[,x]), x="Concentration")) #0.09
cc["P1max"]=max(sapply(deu,function(x,y) max(y[,x]), x="Concentration")) #689.65

#Here we create boundaries for all the varibles so we could set up rules for editing.
#These rules should be in the limits of the lowest and highest values of the variables.
cc["Tmin"]=min(weather$TASMIN)
cc["Tmax"]=max(weather$TASMAX)
cc["Hmin"]=min(weather$RHMIN)
cc["Hmax"]=max(weather$RHMAX)
cc["Pmin"]=100*min(weather$PSLMIN)
cc["Pmax"]=100*max(weather$PSLMAX)
cc
rm(weather)

# Filtering current data by newly received extremum
#install.packages("editrules")
library(editrules)
edited=editset(c("P1 >= 0.09", "P1 <= 689.65","temperature >= -20.5572", "temperature <= 38.892","humidity >= 5", "humidity <= 100","pressure >= 99018", "pressure <= 105113.5"))

#Localizing errors within the data
aux=localizeErrors(edited,df)
names(aux)
# Cheking number of mismeasurements in P1
sum(aux$adapt[,"P1"]) #[1] 70259

df$mm=ifelse(aux$adapt[,"P1"]==T,1,0)
dmm=df %>%
  group_by(geohash) %>%
  summarise(freq=n(),mm=sum(mm),perc=mm/freq) %>%
  arrange(desc(perc))

#Removing 46 stations with overall mismeasurements (by perc value)
auxlist=dmm$geohash[dmm$perc==1]
df=df[!(df$geohash %in% auxlist),]

#Localizing errors again.
aux=localizeErrors(edited,df)
sum(aux$adapt[,"P1"]) #20962
sum(aux$adapt[,"temperature"]) 
#17203
sum(aux$adapt[,"humidity"]) 
#101222
sum(aux$adapt[,"pressure"]) 
#1692247
quantile(df$pressure, probs=c(0.05,0.25,0.50,0.75,0.95))
prout=boxplot.stats(df$pressure, coef=2,do.conf = F, do.out=F)
prout$stats
cc["Pmin"]=prout$stats[1]

#Removing all outliers from set by cc-vector boundaries

df$P1=ifelse(df$P1<cc["P1min"],NA,ifelse(df$P1>cc["P1max"],NA,df$P1))
df$temperature=ifelse(df$temperature<cc["Tmin"],NA,ifelse(df$temperature>cc["Tmax"],NA,df$temperature))
df$humidity=ifelse(df$humidity<cc["Hmin"],NA,ifelse(df$humidity>cc["Hmax"],NA,df$humidity))
df$pressure=ifelse(df$pressure<cc["Pmin"],NA,ifelse(df$pressure>cc["Pmax"],NA,df$pressure))

rm(dfgeo,dmm,aux,auxlist,cc,edited,prout)

#Step 4: Aggregating data by geo-units----
#Creating a time series vector, which should be constructed on hourly basis.
timeser=list()
geounits=list()
k=unique(df$clust.ID)
for (i in 1:length(k)){
  geounits[[i]]=df[df$clust.ID==k[i],]
  timeser[[i]]=as.data.frame(seq.POSIXt(from=min(geounits[[i]]$time),to=max(geounits[[i]]$time), by="hour"))
  colnames(timeser[[i]])[1]="time"
}

geounitsagg=list()
for (i in 1:length(k)){
  geounitsagg[[i]]=aggregate(geounits[[i]][,c("clust.ID","Elev","time","P1","temperature","humidity","pressure")],by=list(geounits[[i]]$time),FUN=mean)
}

#Merging the geo-units list with the aggregated list
for (i in 1:length(k)){
  timeser[[i]]=merge(timeser[[i]],geounitsagg[[i]],by="time")
  timeser[[i]]=timeser[[i]][,-2]
}
rm(geounits,geounitsagg,i,k)

save(list = ls(), file = "week2.RData")

#Step 5: Inspecting and summarizing important characteristics----
setwd("C:\\Users\\Daria\\Desktop\\business analytics\\tuesday\\data for the air case\\METEO-data")
load("week2.RData")

library(fBasics)
library(tseries)
stats=list()
stats$P1=list()
stats$temp=list()
stats$h=list()
stats$pr=list()
for (i in 1:length(timeser)){
  stats$P1[[i]]=basicStats(timeser[[i]][,c("P1")])
  stats$temp[[i]]=basicStats(timeser[[i]][,c("temperature")])
  stats$h[[i]]=basicStats(timeser[[i]][,c("humidity")])
  stats$pr[[i]]=basicStats(timeser[[i]][,c("pressure")])
}
stats$P1=as.data.frame(stats$P1)

windows()
plot(t(stats$P1["NAs",])/t(stats$P1["nobs",]))
windows()
layout(matrix(c(2,1), nrow=1, ncol=2))
plot(t(stats$P1["Skewness",]), main="Skewness") # assymetric distribution
plot(t(stats$P1["Kurtosis",]), main="Kurtosis") # rather fat tails


# Performing stationarity test
DFT=rep(NA,length(timeser))
for (i in 1:length(timeser)){
   a=adf.test(na.omit(timeser[[i]][,"P1"]))
   DFT[i]=a$p.value
   rm(a)
}
windows()
plot(DFT, main = "ADF Stationarity test for clustered data", ylab = "DFT p-value", xlab = "Cluster number", type = "p")
points(DFT, col = "orange", pch = 21, bg = "yellow", cex = 1.55, lwd = 2)
#dev.copy(tiff ,filename="ADF_test.tif") 
#dev.off()

which(DFT>0.05) #[1]  98 102

## Week 3
#Step 1: Checking the variables from the official data----

#setwd("D:\\Preslava\\uni\\Business Analytics\\Tuesday class\\Monthly Challenge Oct\\EEA Data")
setwd("C:\\Users\\Daria\\Desktop\\business analytics\\tuesday\\data for the air case\\EEA Data")

#Making a check whether variables' classes are correct 

sapply(deu$st60881_2018$DatetimeBegin,class)
sapply(deu$st60881_2018$DatetimeEnd,class)
sapply(deu$st60881_2018$Concentration,class)

# Fixing time's class
library(lubridate)
for (i in 1:length(deu)){
  deu[[i]]=deu[[i]][,c("DatetimeEnd","Concentration","DatetimeBegin")]
  deu[[i]]$DatetimeEnd=ymd_hms(deu[[i]]$DatetimeEnd, tz="Europe/Athens")
  deu[[i]]$DatetimeBegin=ymd_hms(deu[[i]]$DatetimeBegin, tz="Europe/Athens")
  colnames(deu[[i]])=c("time","P1eu","begin")
}

#Classes fixed
View(deu)

# Creating time series vector for official P10 measurements (like in Step 4 week2)
timesereu=list()
for (i in 1:length(deu)){
  timesereu[[i]]=as.data.frame(seq.POSIXt(from=min(deu[[i]]$time),to=max(deu[[i]]$time), by="hour"))
  colnames(timesereu[[i]])[1]="time"
}

# Making a list for P10 Concentration for every station
library(dplyr)
for (i in 1:length(deu)){
  timesereu[[i]]=left_join(timesereu[[i]],deu[[i]],by="time")
}
names(timesereu)=names(deu)

#Binding the the official stations datasets for 2017 and 2018
timesereu$st9421=bind_rows(timesereu$st9421_2017,timesereu$st9421_2018)
timesereu$st9572=bind_rows(timesereu$st9572_2017,timesereu$st9572_2018)
timesereu$st9616=bind_rows(timesereu$st9616_2017,timesereu$st9616_2018)
timesereu$st9642=bind_rows(timesereu$st9642_2017,timesereu$st9642_2018)
timesereu$st60881=timesereu$st60881_2018
timesereu=timesereu[c("st9421", "st9572","st9616","st9642","st60881")]
for (i in 1:length(timesereu)){
  colnames(timesereu[[i]])[2]=paste("P1",names(timesereu)[i],sep="")
}
rm(i)

#Checking for duplicates and removing if any
sapply(timesereu,dim)[1,]
sum(duplicated(timesereu[[1]]$time))
#92
sum(duplicated(timesereu[[2]]$time)) 
#92
sum(duplicated(timesereu[[3]]$time)) 
#92
sum(duplicated(timesereu[[4]]$time)) 
#92
sum(duplicated(timesereu[[5]]$time)) 
#7

for (i in 1:length(timesereu)){
  timesereu[[i]]=timesereu[[i]][!(duplicated(timesereu[[i]]$time)==T),]
}

# Visualising official mesuarements missings
library(imputeTS)
sapply(sapply(timesereu,is.na),sum)
windows()
layout(matrix(c(1,2), nrow=2, ncol=1))
plot(timesereu$st9616$time, timesereu$st9616$P1st9616, main = "Missings in European measurements data", 
     sub = "before interpolation", ylab = "P1", xlab = "time progress", type = "s", lty  = "dotted",
     lwd  = 2.5, col  = "dark green")

#Interpolating the missing values for P1eu
for (i in 1:length(timesereu)){
  timesereu[[i]][,2]=na.interpolation(timesereu[[i]][,2], option="linear")
}
plot(timesereu[[i]][,1:2], main = "Missings in European measurements data", 
     sub = "after interpolation", ylab = "P1", xlab = "time progress", type = "s", lty  = "solid",
     lwd  = 1.75, col  = "dark blue")
#dev.copy(tiff ,filename="Interpolation.tif") 
#dev.off()

#Step 2: Adding official statements for mismeasurements----