#https://www.longpaddock.qld.gov.au/cgi-bin/silo/DataDrillDataset.php?format=file_format&lat=Y&lon=X&start=yyyymmdd&finish=yyyymmdd&username=my_username&password=my_password


##Downloading SILO rainfall Data
setwd("E:\\University\\Honours\\Research Project\\SILO")
dir.create(paste0("rainfall"), showWarnings = FALSE)
dir.create(paste0("standard"), showWarnings = FALSE)
dir.create(paste0("Morton"), showWarnings = FALSE)
  
Rivers <- read.csv("Final_Rivers.csv", header = TRUE)
Rivers <- Rivers[order(Rivers$Station_Number),]

for (i in 1:nrow(Rivers)) {
  URL <- paste(c("https://www.longpaddock.qld.gov.au/cgi-bin/silo/DataDrillDataset.php?format=RainOnly&lat=", Rivers$LAT[i], "&lon=", Rivers$LON[i], 
                 "&start=19900101&finish=20101231&username=UOSTRAN&password=BRUCE2165"), collapse = "")
  SiteNumber <- Rivers$Station_Number[i]
  download.file(URL, file.path(paste0("rainfall"),  destfile = paste(SiteNumber,".txt", sep = "")), method = "libcurl")
}

##Downloading SILO Standard data
for (i in 1:nrow(Rivers)) {
  URL <- paste(c("https://www.longpaddock.qld.gov.au/cgi-bin/silo/DataDrillDataset.php?format=Standard&lat=", Rivers$LAT[i], "&lon=", Rivers$LON[i], 
                 "&start=19900101&finish=20101231&username=UOSTRAN&password=BRUCE2165"), collapse = "")
  SiteNumber <- Rivers$Station_Number[i]
  download.file(URL, file.path(paste0("standard"),  destfile = paste(SiteNumber,".txt", sep = "")), method = "libcurl")
}

#Downloading Morton PET
for (i in 1:nrow(Rivers)) {
  URL <- paste(c("https://www.longpaddock.qld.gov.au/cgi-bin/silo/DataDrillDataset.php?format=Allmort&lat=", Rivers$LAT[i], "&lon=", Rivers$LON[i], 
                 "&start=19900101&finish=20101231&username=UOSTRAN&password=BRUCE2165"), collapse = "")
  SiteNumber <- Rivers$Station_Number[i]
  download.file(URL, file.path(paste0("Morton"),  destfile = paste(SiteNumber,".txt", sep = "")), method = "libcurl")
}
