setwd("F:\\University\\Honours\\Research Project")
#wd <- "C:\sersUrs\se\msil8161\\Google Drive\\River Stations in Australia"

dir.create(paste0("Rivers"), showWarnings = FALSE)

#install.packages("RCurl")
library(RCurl)

name <- c("ID", "Station_Number", "Hydrstra_Site.ID", "LAT", "LON", "Site_Name", "Jurisdiction")
Data <- read.csv("HRS_Nos_as_of_20140711.csv", col.names = name)
Data <-data.frame(Data)
head(Data)
Data$Station_Number <- as.character(Data$Station_Number)
Data$Site_Name <- as.character(Data$Site_Name)

#factor(Data$Station_Number)

for(i in 1:length(Data$Station_Number)){
  URL <- paste(c("http://www.bom.gov.au/water/hrs/content/data/",Data$Station_Number[i],"/",Data$Station_Number[i],"_daily_ts.csv"), collapse= "")
  SiteName <- Data$Station_Number[i]
  download.file(URL,file.path(paste0("Rivers"), destfile=paste(SiteName,".csv",sep="")), method="libcurl")
} 

#daily code = 136
#monthy code = 139

# bomdata<- function(station,code){
#   for(i in 1: length(station)){
#     p.url<-paste("http://www.bom.gov.au/water/hrs/index.shtml#panel=data-download&id=",station[i],"&p_display_type=availableYears&p_nccObsCode=",code,sep ="")
#     download.file(p.url,"test.txt")
#     filelist <- list.files(pattern = ".txt")
#     foo<- file(filelist,"r")
#     text<- suppressWarnings(readLines(foo))
#     close(foo)
#     l<- regexpr(":",text[1])
#     m<- unlist(gregexpr(",", text[1], perl = TRUE))
#     pc<- substr(text[1],l[[1]]+1,l[[1]]+(m[2]-(l[[1]]+1)))
#     url<-paste("http://www.bom.gov.au/water/hrs/index.shtml#panel=data-download&id=",station[i],"&p_c=",pc,"&p_nccObsCode=",code,"&p_startYear=2013", sep ="")
#     suppressWarnings(download.file(url,paste(station[i],".zip",sep= ""), mode = "wb"))
#     unlink("test.txt")
#   }
# }
# 
# bomdata(210006,136)
# 
# 
# station = "test"
# filename = paste(station,"file.csv",sep="_")
# # latitude and longitude
# # we will need to find a way to match to closest grid
# # maybe this: http://stackoverflow.com/questions/32618956/find-the-nearest-x-y-coordinate-using-r
# # maybe this: http://stackoverflow.com/questions/17275825/how-to-find-the-closest-lat-and-long-in-a-text-file
# # Lat = "-33.784413"
# # Long = "150.764955"
# # # start and finish times
# # start = "1970-01-01"
# # finish = "2012-12-01" 
# 
# coreURL = "http://www.bom.gov.au/water/hrs/index.shtml#panel=data-download"
# urlstring <- paste(coreURL,
#                    "&id=,"&time_start=",start,"T00%3A00%3A00Z&time_end=",finish,
#                    "T00%3A00%3A00Z&accept=netcdf",sep="") 
# 
# 
# test <- download(urlstring,destfile=filename)