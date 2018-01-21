# # First merging rainfall station data BOM
# adding updates to the files
setwd("Z:\\Public\\BOM_Stationdata")

# find oldest weather stations in Australia
stnFiles <- dir("Historical_data",pattern="StnDet")
maxyear <-  1990 # no stations that started reporting after 1900

#Station Data
for (i in 1:length(stnFiles)) {
  stndata <- read.csv(paste("historical_data/",stnFiles[i],sep=""), stringsAsFactors=FALSE)
  olddata <- stndata[stndata[,14]<maxyear,]
  if (i == 1) {Allold <- olddata} else Allold <- rbind(Allold,olddata)
}

# plot the stations in space
require(ggplot2)
p <- ggplot(Allold,aes(x=Longitude.to.4.decimal.places.in.decimal.degrees,
                       y=Latitude.to.4.decimal.places.in.decimal.degrees)) +
  geom_point(aes(col=First.year.of.data.supplied.in.data.file))
p

#now subset by least missing data - Using >= 90% complete and >= 90% Quality Y as an arbitrary benchmark
station_quality <- Allold[which(as.numeric(Allold$Percentage.complete.between.first.and.last.records) >= 90 &
                                  as.numeric(Allold$Percentage.of.values.with.quality.flag..Y.) >= 90 &
                            as.numeric(Allold[,15]) >= 2010),]

############----------READING STREAMFLOW Rivers---------##############

setwd("F:\\University\\Honours\\Research Project")
dir.create(paste0("Rivers"), showWarnings = FALSE)

#install.packages("RCurl")
library(RCurl)

name <- c("ID", "Station_Number", "Hydrstra_Site.ID", "LAT", "LON", "Site_Name", "Jurisdiction")
Rivers <- read.csv("HRS_Nos_as_of_20140711.csv", col.names = name)
Rivers$Station_Number <- as.character(Rivers$Station_Number)
Rivers$Site_Name <- as.character(Rivers$Site_Name)

######-------------READING RAINFALL STATION Rivers---------------#############

setwd("F:\\University\\Honours\\Research Project\\Rainfall")
dir()

Rivers[32,4] <- -29.218 ##Changing Lat at Beardy River at Haystack because it was at 312 which is wrong

river.quality <- read.csv("Catchment Quality.csv", header = T)
Rivers <- merge(Rivers, river.quality, by = "Station_Number")

#######--------------FINDING NEAREST LAT/LON-----------################
#install.packages("rgeos")
library(rgeos)
library(sp)
Riverssp <- SpatialPoints(Rivers[,5:4])
Stationssp <- SpatialPoints(station_quality[,8:7])
Rivers$nearest_in_Stations <- apply(gDistance(Stationssp, Riverssp, byid=TRUE), 1, which.min)

Rivers <- data.frame(Rivers,  Stationssp[Rivers$nearest_in_Stations],station_quality[Rivers$nearest_in_Stations,][c(2,4)])
colnames(Rivers)[11:14] <- c("Nearest Lon", "Nearest Lat", "Station_No", "Station_Name")

#####-------------Finding the euclidean distances----------------#############################

Rivers$Difference <- sqrt((Rivers$LAT - Rivers$`Nearest Lat`)^2 + (Rivers$LON - Rivers$`Nearest Lon`)^2)
Rivers <- Rivers[order(Rivers$Difference, Rivers$Station_Number),]
Rivers <- Rivers[which(Rivers$Difference<0.45),]

#Write.csv in R
Abbreviation <- substr(Rivers$Site_Name, 1, 4)
Stations <- data.frame(Abbreviation, Rivers[,c(1,5,4)])

write.csv(Stations, file = "Final_Stations.csv", row.names = FALSE)
write.csv(Rivers[,c(1,6,4,5,7,8)], file = "Final_Rivers.csv", row.names = FALSE)
write.csv(Rivers, file = "Final_RiverStations.csv", row.names = FALSE)

##############------------------------------######################

#Subset by nearest relevant stations (nearest stations from rivers)

##Just access the relevant files
# station_quality <- station_quality[Rivers$nearest_in_Stations,]
# station_quality  <- unique(station_quality) ##Number of unique stations - 93

# now subset for having temperature data
# setwd("Z:\\Public\\BOM_Stationdata")
# 
# flist <- dir("MergedUpToDateFiles/",pattern="2D")
# for (i in 1:nrow(station_quality)) {
#   #i <- 1
#   if (is.na(flist[grep(paste("0",station_quality[i,2],sep=""),flist)][1]) == FALSE) {
#     filein <- read.csv(paste("MergedUpToDateFiles/",flist[grep(paste("0",station_quality[i,2],sep=""),flist)][1],sep=""))
#   } else {
#     filein <- read.csv(paste("MergedUpToDateFiles/",flist[grep(paste(station_quality[i,2],sep=""),flist)][1],sep=""))
#   }
#   write.csv(filein,paste("stationfiles",flist[grep(paste("0",station_quality[i,2],sep=""),flist)][1],sep="/"),row.names=F)
# }
# 
# write.csv(station_quality,"Relevant Stations.csv",row.names=F,quote=F)
# 
# require(ggplot2)
# p2 <- ggplot(station_quality,aes(x=Longitude.to.4.decimal.places.in.decimal.degrees,
#                        y=Latitude.to.4.decimal.places.in.decimal.degrees)) +
#   geom_point(aes(col=First.year.of.data.supplied.in.data.file))
# 
# p2

#extract the files and copy them to a new folder
 # for (i in 1:nrow(station_quality)) {
 #   if(dir.exists("stationfiles")==F) dir.create("stationfiles")
 #   file.copy(paste("MergedUpToDateFiles/",dir("MergedUpToDateFiles",
 #                 pattern=paste("0",as.character(station_quality$Bureau.of.Meteorology.Station.Number[i]),
 #                               sep=""))[1],sep=""),"stationfiles",overwrite=T)
 # }
