#Week 1 
#Step 1. Importing data-----------
rm(list=ls())

AirQ17=read.csv("C:\\Users\\Daria\\Desktop\\tuesday\\data for the air case\\week1\\data_bg_2017.csv",stringsAsFactors = FALSE)
AirQ18=read.csv("C:\\Users\\Daria\\Desktop\\tuesday\\data for the air case\\week1\\data_bg_2018.csv",stringsAsFactors = FALSE)
Sofia_Topo=read.csv("C:\\Users\\Daria\\Desktop\\tuesday\\data for the air case\\week1\\sofia_topo.csv", stringsAsFactors = FALSE)

#Step 2. Inspecting data structure-------------
#installing library
install.packages("lubridate")
#install.packages("dplyr")
install.packages("tidyverse")
install.packages("geohash")
install.packages("geoR")
install.packages("ggmap")
#install.packages("DT") 
install.packages("knitr")
#install.packages("rgdal")
install.packages("maps")
library(lubridate)
library(tidyverse)
library(geohash)
library(geoR)
library(ggmap)
library(knitr)
library(maps)
library(dplyr)

# Some basic characteristics
head(AirQ17)
head(AirQ18)
summary(AirQ17)
summary(AirQ18)

#checking the class of columns 
sapply(AirQ17, class)
sapply(AirQ18, class)
sapply(Sofia_Topo, class)
## air measurements 2017-2018

# 1. converting $time to class "POSIXct"
AirQ17$time=as_datetime(AirQ17$time, tz="UTC")
#class(AirQ17$time) #[1] "POSIXct" "POSIXt"
AirQ18$time=as_datetime(AirQ18$time, tz="UTC")

# Step 3. Handling unique stations and merging datasets-------
unique_2017<-unique(AirQ17$geohash)
length(unique_2017) #[1] 383

unique_2018<-unique(AirQ18$geohash)
length(unique_2018) #[1] 1254
notIn18=setdiff(AirQ17$geohash,AirQ18$geohash)
#View(notIn18) # 11 stations
notIn18=as.data.frame(notIn18)
Air2017_upd=subset(AirQ17, !(AirQ17$geohash %in% notIn18$notIn18))

# check the removal
length(unique(AirQ17$geohash)) #[1] 383
length(unique(Air2017_upd$geohash)) #[1] 383

# binding datasets
data_bg_full <- rbind(Air2017_upd, AirQ18)

#Step 4. Summarizing availability of merged data by stations-----------
#Checking for NAs
sum(is.na(data_bg_full$geohash)) #[1] 0 There are no NAs
sum(data_bg_full$geohash == "") # [1] 4 There are 4 empty geohashes

#Clean missing geohashes
data_bg_clean <- data_bg_full%>%
  filter(data_bg_full$geohash != "")
unique_full_set <- data.frame(unique(data_bg_clean$geohash)) #1253 unique stations == length of frequency
# summarize the observations
freq <- data.frame(table(data_bg_clean$geohash)) 

MinTime <- aggregate(time ~ geohash, data = data_bg_clean, min) #MinTime
MaxTime <- aggregate(time ~ geohash, data = data_bg_clean, max) #MaxTime

# merge MinTime and MaxTime and prepare for merging to the final Stations
minmax <- merge(MinTime, MaxTime, by="geohash")

# building the final Stations dataset
colnames(freq)[colnames(freq) == 'Var1'] <- 'geohash'
Stations <- merge(freq, minmax, by="geohash")
Stations$days <- difftime(Stations$time.y, Stations$time.x, units = "days")

# do some renaming
colnames(Stations)[colnames(Stations) == 'time.x'] <- 'MinTime'
colnames(Stations)[colnames(Stations) == 'time.y'] <- 'MaxTime'
colnames(Stations)[colnames(Stations) == 'Freq'] <- 'obs'

#filter geohashes > 4
Stations <-  filter(Stations,obs > 250) 
Stations <-  filter(Stations,days > 14) #1188 observations

# check the Stations
head(Stations)

# Step 5. Summarizing stations on the map--------
install.packages("geohash")
library(geohash)
# getting geohashes decoded

geohash_decoded <- as.data.frame(gh_decode(Stations$geohash))
Stations_decoded <- data.frame(Stations, geohash_decoded)

# plotting some maps
install.packages("leaflet")
library(leaflet)

# plot all stations on the map

Stations_decoded %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions())

# extract Sofia stations only
colnames(Sofia_Topo)[colnames(Sofia_Topo) == 'Lat'] <- 'lat'
colnames(Sofia_Topo)[colnames(Sofia_Topo) == 'Lon'] <- 'lng'
Sofia_stations <- Stations_decoded[which(Stations_decoded$lat < max(Sofia_Topo$lat) & Stations_decoded$lat > min(Sofia_Topo$lat)& Stations_decoded$lng < max(Sofia_Topo$lng) & Stations_decoded$lng > min(Sofia_Topo$lng) ), ]

# plot stations in Sofia only
Sofia_stations %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(weight = 0.1, color = "red") %>%
  addRectangles(lat1 = (min(Sofia_stations$lat)-0.01), lng1 = (max(Sofia_stations$lng)-0.18), 
                lat2 = (min(Sofia_stations$lat)+0.13), lng2 = (min(Sofia_stations$lng)+0.18),
                fillColor = "transparent")

# Step 6: Filter stations by space and time ----

# now that we have selected the stations we're going to use for the analysis,
# subset the dataset with the clear citizen data for Sofia, using the filters we defined in step 4 and 5 above 
data_bg_final <- subset(data_bg_clean, (data_bg_clean$geohash %in% Sofia_stations$geohash))
data_bg_final <- data.frame(data_bg_final, as.data.frame(gh_decode(data_bg_final$geohash)))

# Week 2.Preliminary Analysis II----------
#Step 1. Decide on the final list of geo-units that are subject to predictive analysis----
#clustering stations in Sofia by their latitude and longitude into 15 groups
set.seed(20) # allows to create a starting point for randomly generated numbers,
clusters<-kmeans(Stations_decoded[,6:7], 196)
str(clusters)
centroids<-clusters$centers
View(centroids)

#finding the closest point to the cluster’s centroid
install.packages("rgeos")
library(rgeos)
library(sp)
library(geosphere)
install.packages("nlme")
library(nlme)
install.packages("gdistance")
library(gdistance)

sp1<-SpatialPoints(centroids[,c("lat","lng")])
sp2<-SpatialPoints(Sofia_Topo[,c("lng","lat")])

install.packages("fields")
library(fields)
rdist<-data.frame(rdist(centroids, Sofia_Topo))
mindist<-data.frame(apply(rdist,1, FUN=min))
colnames(mindist) <- 'mindistance'

View(distance)
install.packages("raster")
library(raster)



############ íåóñïåøíè îïèòè, íî íå ãè òðèÿ çà ñëó÷àé, àêî ùå ïîäïîìîãíàò íà íÿêîãî äà ìó õðóìíå
>cbind(salaries, experience)

# Âåðñèÿ íà Âëàäèìèð:
set1sp <- Spati9alPoints(centroids[,c("lat","lng")])
set2sp <- SpatialPoints(Sofia_Topo[,c("Lon","Lat")])
zz <- distGeo(setsp1,setsp2)
GroupedData1 <- GroupedData
GroupedData1$nearest_topo <- apply(gDistance(sp2, sp1, byid=TRUE), 1, which.min)
GroupedData1$Elevation <- SofiaTopography[GroupedData1$nearest_topo,"Elev"]
# êðàé íà íåãîâàòà âåðñèÿ

lapply(Sofia_Topo,function(Sofia_Topo,centroids) {(sqrt((Sofia_Topo[1] - centroids[1])^2+(Sofia_Topo[2]-centroids[2])^2))},centroids)
#$`lat`
#[1] 0.1516605

#$lng
#[1] 27.57779

#$Elev
#[1] 1722.587

#$Cluster
#[1] NA
###

attach()
Then centers are given by:
  
  cnt = c(mean(m[,1]),mean(m[,2]))
So the code returning vector of distance between every row of m and cnt will be:
  
  apply(m,1,function(x,cnt) {(sqrt((x[1] - cnt[1])^2+(x[2]-cnt[2])^2))},cnt)
And the result is:
