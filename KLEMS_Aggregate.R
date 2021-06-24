library(readxl)
library(systemfit)
library(dplyr)
library(reshape2)
library(stringr)
library(plm)

#Collecting Aggregation Indices
AggIndex <- read.csv("Index.csv")
Agg_0D <- AggIndex[,c(1,3)]
Agg_1D <- AggIndex[,c(1,4)]
Agg_2D <- AggIndex[,c(1,5)]
Agg_3D <- AggIndex[,c(1,6)]
Agg_4D <- AggIndex[,c(1,7)]
Agg_Opt <- AggIndex[,c(1,8)]

#List of Countries Being Studied
cID <- c("AT", "BE", "CZ", "DE", "DK", "EE", "ES", "GR", "FI", "FR", "HU",
         "IE", "IT", "LT", "NL", "PL", "SE", "SI", "GB", "US", "JP")

#Extracting Monetary Inputs Data from Constant-Price IO Table (Reference Year: 2010)
Constant <- data.frame()

for (country in rev(cID)){
  ntnldata <- data.frame()
  for (year in 1995:2016){
    tempdata <- read_excel(paste0("KLEMS_Constant_",year,".xlsx"), sheet = country)
    ntnldata <- rbind(ntnldata,tempdata)
  }
  assign(paste0(country), ntnldata)
  Constant <- rbind(get(paste0(country)), Constant)
  rm(list = paste0(country))
}

colnames(Constant)[1] <- "Industry"
Constant <- arrange(Constant,Country,Year,Industry)

#Extracting Monetary Inputs Data from Current-Price IO Table
Current <- data.frame()

for (country in rev(cID)){
  ntnldata <- data.frame()
  for (year in 1995:2016){
    tempdata <- read_excel(paste0("KLEMS_Current_",year,".xlsx"), sheet = country)
    ntnldata <- rbind(ntnldata,tempdata)
    rm(tempdata)
  }
  assign(paste0(country), ntnldata)
  rm(ntnldata)
  Current <- rbind(get(paste0(country)), Current)
  rm(list = paste0(country))
}

colnames(Current)[1] <- "Industry"
Current <- arrange(Current,Country,Year,Industry)

#Collecting Price Indices of Capital and Labor Inputs
CapIdx <- read_excel("KL_PriceIndex.xlsx", sheet = "CAP")
LabIdx <- read_excel("KL_PriceIndex.xlsx", sheet = "LAB")

CapIdx[CapIdx == "NA"] <- NA
LabIdx[LabIdx == "NA"] <- NA

CapIdx <- melt(CapIdx, id=c("Year", "Country"))
LabIdx <- melt(LabIdx, id=c("Year", "Country"))

colnames(CapIdx)[3] <- "Industry"
colnames(LabIdx)[3] <- "Industry"

colnames(CapIdx)[4] <- "Index"
colnames(LabIdx)[4] <- "Index"

CapIdx <- arrange(CapIdx,Country,Year,Industry)
LabIdx <- arrange(LabIdx,Country,Year,Industry)

Kc <- Current$Capital / as.numeric(CapIdx$Index) * 100
Lc <- Current$Labor / as.numeric(LabIdx$Index) * 100

#Building KLEMS Dataset
KLEMS <- cbind.data.frame(CapIdx[1:3])
KLEMS <- cbind.data.frame(KLEMS,Current[4:5],Current[c(6,8,10,11)],Kc,Lc,Constant[c(4,6,8,9)])

names(KLEMS) <- c("Year", "Country", "Industry", "K", "L", "E", "M", "S", "GO", "Kc", "Lc", "Ec", "Mc", "Sc", "GOc")

KLEMS$Agg0 <- rep(AggIndex$Agg0,22*21)
KLEMS$Agg1 <- rep(AggIndex$Agg1,22*21)
KLEMS$Agg2 <- rep(AggIndex$Agg2,22*21)
KLEMS$Agg3 <- rep(AggIndex$Agg3,22*21)
KLEMS$Agg4 <- rep(AggIndex$Agg4,22*21)
KLEMS$Opt <- rep(AggIndex$Opt,22*21)

KLEMS_4 <- list(KLEMS$K, KLEMS$L, KLEMS$E, KLEMS$M, KLEMS$S, KLEMS$GO,
                KLEMS$Kc, KLEMS$Lc, KLEMS$Ec, KLEMS$Mc, KLEMS$Sc, KLEMS$GOc)
KLEMS_0 <- aggregate(KLEMS_4, by=list(KLEMS$Year, KLEMS$Country, KLEMS$Agg0), FUN=sum)
KLEMS_1 <- aggregate(KLEMS_4, by=list(KLEMS$Year, KLEMS$Country, KLEMS$Agg1), FUN=sum)
KLEMS_2 <- aggregate(KLEMS_4, by=list(KLEMS$Year, KLEMS$Country, KLEMS$Agg2), FUN=sum)
KLEMS_3 <- aggregate(KLEMS_4, by=list(KLEMS$Year, KLEMS$Country, KLEMS$Agg3), FUN=sum)
KLEMS_Opt <- aggregate(KLEMS_4, by=list(KLEMS$Year, KLEMS$Country, KLEMS$Opt), FUN=sum)
KLEMS_4 <- aggregate(KLEMS_4, by=list(KLEMS$Year, KLEMS$Country, KLEMS$Agg4), FUN=sum)

# KLEMS$Kc[is.nan(KLEMS$Kc)] <- 0
# KLEMS$Lc[is.nan(KLEMS$Lc)] <- 0
# KLEMS$Ec[is.nan(KLEMS$Ec)] <- 0
# KLEMS$Mc[is.nan(KLEMS$Mc)] <- 0
# KLEMS$Sc[is.nan(KLEMS$Sc)] <- 0
# KLEMS$GOc[is.nan(KLEMS$GOc)] <- 0

names(KLEMS_0) <- c("Year", "Country", "Industry", "K", "L", "E", "M", "S", "GO",
                    "Kc", "Lc", "Ec", "Mc", "Sc", "GOc")
names(KLEMS_1) <- c("Year", "Country", "Industry", "K", "L", "E", "M", "S", "GO",
                    "Kc", "Lc", "Ec", "Mc", "Sc", "GOc")
names(KLEMS_2) <- c("Year", "Country", "Industry", "K", "L", "E", "M", "S", "GO",
                    "Kc", "Lc", "Ec", "Mc", "Sc", "GOc")
names(KLEMS_3) <- c("Year", "Country", "Industry", "K", "L", "E", "M", "S", "GO",
                    "Kc", "Lc", "Ec", "Mc", "Sc", "GOc")
names(KLEMS_Opt) <- c("Year", "Country", "Industry", "K", "L", "E", "M", "S", "GO",
                    "Kc", "Lc", "Ec", "Mc", "Sc", "GOc")
names(KLEMS_4) <- c("Year", "Country", "Industry", "K", "L", "E", "M", "S", "GO",
                    "Kc", "Lc", "Ec", "Mc", "Sc", "GOc")

for (i in c("KLEMS_0", "KLEMS_1", "KLEMS_2", "KLEMS_3", "KLEMS_4", "KLEMS_Opt")){
  data <- get(i)
  
  data$Kc[data$Kc == 0] <- NA
  data$Lc[data$Lc == 0] <- NA
  data$Ec[data$Ec == 0] <- NA
  data$Mc[data$Mc == 0] <- NA
  data$Sc[data$Sc == 0] <- NA
  data$GOc[data$GOc == 0] <- NA
  
  data <- na.omit(data)
  
  assign(i, data)
}

KLEMS <- na.omit(KLEMS)

#New KLEMS
#nKLEMS <- KLEMS[KLEMS$Country %in% c("AT", "BE", "DE", "DK", "FI", "FR", "IE", "LU", "NL", "SE", "GB"),]
