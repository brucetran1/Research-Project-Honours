# ##Modelling exercise
# setwd("G:\\University\\Honours\\Data Access\\Modelling\\zoo SILO")

#load hydromad library
library(hydromad)
rm(river)
#read in the data
# load("A5130501.rdata")
# 
# river
# 
# river$Q = convertFlow(river$Q, from ="ML", area.km2= 187.4)
# 
# xyplot(river)
# 
# #GR4J
# CMod <- hydromad(river, sma="gr4j", routing="gr4jrouting",
#                  etmult=c(0.05,1.5),x1 = c(50,2000), x2 = c(-10,10), #some suggestions off c(0.05, 1.5) for etmult
#                  x3 =c(5,500), x4 = c(0.5,10)) #S_0 and R_0 don't have to be filled in but can go with the standards
# 
# print(CMod)
# coefficients(CMod)
# 
# #install.packages(c("randtoolbox", "sensitivity"))
# library(randtoolbox)
# library(sensitivity)
# 
# # mm <- morris(model=evalPars,
# #              factor=names(getFreeParsRanges(CMod)),
# #              r=1000,
# #              design=list(type="oat",levels=10, grid.jump=2),
# #              binf=sapply(getFreeParsRanges(CMod),min),
# #              bsup=sapply(getFreeParsRanges(CMod),max),
# #              object=CMod,
# #              objective=~hmadstat("r.squared")(Q,X)
# # ) #have to specify your model
# # mm
# 
# river.cal <- window(river, start = "1990-01-01",end = "2000-12-31") #you can choose your own periods
# river.val <- window(river, start = "2001-01-01", end = "2010-12-31") #Backward validation
# 
# CMod <- hydromad(river.cal, sma="gr4j", routing="gr4jrouting",
#                   etmult=0.15,x1 = c(50,2000), x2 = c(-10,10),
#                   x3 =c(5,500), x4 = c(0.5,10)) #S_0 = 0.5, R_0 = 0.5)
# 
# riverFit <- fitByOptim(CMod,objective=~hmadstat("r.squared")(Q,X),
#                         samples=1000,method="PORT")
# riverFit1 <- fitByOptim(CMod,objective=~hmadstat("r.sq.log")(Q,X),
#                        samples=1000,method="PORT")
# 
# summary(riverFit)
# summary(riverFit1)
# objFunVal(riverFit)
# xyplot(riverFit1, with.P=TRUE, xlim=as.Date(c("1990-01-01", "2000-01-31")))
# xyplot(riverFit, with.P=TRUE, xlim=as.Date(c("2001-01-01", "2010-01-31")))
# ?fitByOptim
# sim.val <- update(riverFit, newdata = river.val)
# summary(sim.val)
# objFunVal(sim.val)
# xyplot(sim.val, with.P=TRUE, xlim=as.Date(c("2001-01-01", "2010-01-31")))
# xyplot(sim.val, with.P=TRUE, xlim=as.Date(c("1990-01-01", "2000-01-31")))
# 
# 
# summary(river$P)
########-----------------FLOW DURATION CURVES-------------------########
setwd("F:\\University\\Honours\\Research Project\\Modelling\\zoo SILO")
river.list <- dir()
#install.packages("tidyverse")
library(tidyverse)

FDC.summary <- data.frame(Number = rep(NA, length(river.list)), 
                          Name = rep(NA, length(river.list)),
                          P25.FDC1 = rep(NA, length(river.list)),
                          P50.FDC1 = rep(NA, length(river.list)),
                          P75.FDC1 = rep(NA, length(river.list)),
                          P90.FDC1 = rep(NA, length(river.list)),
                          P25.FDC2 = rep(NA, length(river.list)),
                          P50.FDC2 = rep(NA, length(river.list)),
                          P75.FDC2 = rep(NA, length(river.list)),
                          P90.FDC2 = rep(NA, length(river.list)))

percentiles <- c(0.25, 0.5, 0.75, 0.9)

for (i in 1:length(river.list)) {
  load(river.list[i])
  
  #Extract river number and name
  river.number <- as.character(strsplit(river.list[i], ".Rdata"))
  header <- read_csv(paste("F:\\University\\Honours\\Research Project\\Modelling\\Rivers\\",river.number, ".csv", sep = ""), 
                     n_max = 26, col_names = F)
  river.name <- as.character(substr(header[15,2],1,gregexpr("\\ ",header[15,2])[[1]]-1)) ##Extract name
  Area <- read.csv(paste("F:\\University\\Honours\\Research Project\\Modelling\\Rivers\\",river.number, ".csv", sep = ""),
                   sep = c(" ", ",")) ##Read in river file to extract area
  Area <- as.numeric(substr(Area[16,1], 1, gregexpr("\\,",Area[16,1])[[1]]-1)) ##Extract Area
  river$Q <- convertFlow(river$Q, from ="ML", area.km2= Area) #Convert flow from ML to mm
  
  #Subset zoo files into 1990-1996 and 2000-2006 time periods
  FDC1 <- window(river, start = "1990-01-01", end = "2000-12-31")
  FDC2 <- window(river, start = "2001-01-01", end = "2010-12-31")
  
  #streamflow duration curve analysis
  n <- nrow(FDC1)
  sort.flow1 <- sort(as.numeric(FDC1$Q), decreasing = TRUE, na.last=FALSE)
  #Sort - just a series of numbers now
  rank.flow1 <- 1:n
  Prob1 <- rank.flow1/(n+1) 
  
  m <- nrow(FDC2)
  sort.flow2 <- sort(as.numeric(FDC2$Q), decreasing = TRUE, na.last=FALSE)
  #Sort - just a series of numbers now
  rank.flow2 <- 1:m
  Prob2 <- rank.flow2/(m+1) 
  
  ##Extracting Quantlies from FDCs
  FDC1.quantiles <- quantile(sort.flow1, percentiles)
  FDC2.quantiles <- quantile(sort.flow2, percentiles)
  
  #Putting results into data.frame
  FDC.summary[i,1] <- river.number
  FDC.summary[i,2] <- river.name
  FDC.summary[i,3] <- FDC1.quantiles[1]
  FDC.summary[i,4] <- FDC1.quantiles[2]
  FDC.summary[i,5] <- FDC1.quantiles[3]
  FDC.summary[i,6] <- FDC1.quantiles[4]
  FDC.summary[i,7] <- FDC2.quantiles[1]
  FDC.summary[i,8] <- FDC2.quantiles[2]
  FDC.summary[i,9] <- FDC2.quantiles[3]
  FDC.summary[i,10] <- FDC2.quantiles[4]
  
  rm(river)
}
par(mfrow=c(2,2))
plot(FDC.summary[,3], FDC.summary[,7], xlab = "1990-2000", ylab = "2001-2010", col = "red", main = "25th percentile")
abline(0,1)
plot(FDC.summary[,4], FDC.summary[,8], xlab = "1990-2000", ylab = "2001-20010", col = "blue", main = "50th percentile")
abline(0,1)
plot(FDC.summary[,5], FDC.summary[,9], xlab = "1990-2000", ylab = "2001-2010", col = "green", main = "75th percentile")
abline(0,1)
plot(FDC.summary[,6], FDC.summary[,10], xlab = "1990-2000", ylab = "2001-2010", col = "purple", main = "90th percentile")
abline(0,1)

ratio = FDC.summary[,3]/FDC.summary[,7]
length(ratio[which(ratio > 1)])


############----------------LET'S MAKE A BIG TABLE--------------#################
##Modelling exercise
setwd("F:\\University\\Honours\\Research Project\\Modelling\\zoo SILO")

#load hydromad library
library(hydromad)
river.list <- dir()
#install.packages("tidyverse")
library(tidyverse)

GR4J.summary <-  data.frame(Number = rep(NA, length(river.list)), 
                            Name = rep(NA, length(river.list)),
                            Area = rep(NA, length(river.list)),
                            x1 = rep(NA, length(river.list)),
                            x2 = rep(NA, length(river.list)),
                            x3 = rep(NA, length(river.list)),
                            x4 = rep(NA, length(river.list)), 
                            runoff = rep(NA, length(river.list)),
                            rbias = rep(NA, length(river.list)),
                            NSE = rep(NA, length(river.list)),
                            r.sq.sqrt = rep(NA, length(river.list)),
                            r.sq.log = rep(NA, length(river.list)),
                            objFun = rep(NA, length(river.list)))

#GR4J.summary <- read.csv("G:\\University\\Honours\\Data Access\\Modelling\\summary_GR4J.csv", header = T)
GR4J.summary$Number <- as.character(GR4J.summary$Number)
GR4J.summary$Name <- as.character(GR4J.summary$Name)

##Reading in the data
for (i in 1:length(river.list)) {
  load(river.list[i])
  
  river.number <- as.character(strsplit(river.list[i], ".Rdata"))
  header <- read_csv(paste("G:\\University\\Honours\\Data Access\\Modelling\\Rivers\\",river.number, ".csv", sep = ""), 
                     n_max = 26, col_names = F)
  river.name <- as.character(substr(header[15,2],1,gregexpr("\\ ",header[15,2])[[1]]-1)) ##Extract name
  Area <- read.csv(paste("G:\\University\\Honours\\Data Access\\Modelling\\Rivers\\",river.number, ".csv", sep = ""),
                   sep = c(" ", ",")) ##Read in river file to extract area
  Area <- as.numeric(substr(Area[16,1], 1, gregexpr("\\,",Area[16,1])[[1]]-1)) ##Extract Area
  river$Q <- convertFlow(river$Q, from ="ML", area.km2= Area) #Convert flow from ML to mm

  river.cal <- window(river, start = "1990-01-01",end = "1996-12-31")

  CMod <- hydromad(river.cal, sma="gr4j", routing="gr4jrouting",
                   etmult=0.15,x1 = c(50,2000), x2 = c(-10,10),
                   x3 =c(5,500), x4 = c(0.5,10)) #S_0 = 0.5, R_0 = 0.5)
  # hydromad.stats("viney" = function(Q, X, ...) {
  #   hmadstat("r.squared")(Q, X, ...) -
  #     5*(abs(log(1+hmadstat("rel.bias")(Q,X)))^2.5)})
  riverFit <- fitByOptim(CMod,objective=~ hmadstat("viney"),
                         samples=500,method="PORT")##Let's start by maximising R2 (NSE))
  
  GR4J.summary[i,1] <- river.number
  GR4J.summary[i,2] <- river.name
  GR4J.summary[i,3] <- Area
  GR4J.summary[i,4] <- riverFit$parlist$x1
  GR4J.summary[i,5] <- riverFit$parlist$x2
  GR4J.summary[i,6] <- riverFit$parlist$x3
  GR4J.summary[i,7] <- riverFit$parlist$x4
  GR4J.summary[i,8] <- summary(riverFit)$runoff
  GR4J.summary[i,9] <- summary(riverFit)$rel.bias
  GR4J.summary[i,10] <- summary(riverFit)$r.squared
  GR4J.summary[i,11] <- summary(riverFit)$r.sq.sqrt
  GR4J.summary[i,12] <- summary(riverFit)$r.sq.log
  GR4J.summary[i,13] <- objFunVal(riverFit)
  
  Fitted <- riverFit$fitted.values
  save(Fitted, file = paste(".\\Fitted_1990_1996_3\\", river.number, ".rdata", sep = ""))
   
  rm(river)
}

#GR4J.summary <- GR4J.summary[order(GR4J.summary[,13], GR4J.summary[,1]),]
write.csv(GR4J.summary, file = "summary_GR4J_1990_1996_3.csv", row.names = FALSE)
