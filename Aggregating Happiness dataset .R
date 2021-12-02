#Install libraries
library(data.table)
library(plyr)
library(dplyr)
library(purrr)

#Load datasets
Hap15 <- read.csv("2015.csv")
Hap16 <- read.csv("2016.csv")
Hap17 <- read.csv("2017.csv")
Hap18 <- read.csv("2018.csv")
Hap19 <- read.csv("2019.csv")
Hap20 <- read.csv("2020.csv")

#Rename coloumns to match
colnames(Hap15)[4] <- "Score15"
colnames(Hap16)[4] <- "Score16"
colnames(Hap17)[3] <- "Score17"
colnames(Hap18)[3] <- "Score18"
colnames(Hap19)[3] <- "Score19"
colnames(Hap20)[3] <- "Score20"
colnames(Hap18)[2] <- "Country"
colnames(Hap19)[2] <- "Country"
colnames(Hap20)[1] <- "Country"

#Subsetting all datasets to include only Country and Happiness Score
Hap15C=subset(Hap15, select=c(Country,Score15))
Hap16C=subset(Hap16, select=c(Country,Score16))
Hap17C=subset(Hap17, select=c(Country,Score17))
Hap18C=subset(Hap18, select=c(Country,Score18))
Hap19C=subset(Hap19, select=c(Country,Score19))
Hap20C=subset(Hap20, select=c(Country,Score20))

#Join by Country using dplyr and rename columns
joined <- list(Hap15C,Hap16C,Hap17C,Hap18C,Hap19C,Hap20C)
plyrjoin <- join_all(joined,by=c("Country"),type="full")

names(plyrjoin)[2] <- "Score 2015"
names(plyrjoin)[3] <- "Score 2016"
names(plyrjoin)[4] <- "Score 2017"
names(plyrjoin)[5] <- "Score 2018"
names(plyrjoin)[6] <- "Score 2019"
names(plyrjoin)[7] <- "Score 2020"

#Matching differently spelled country names
plyrjoin$Country[91] <- "Somaliland Region"
plyrjoin$Country[165] <- "Taiwan"
plyrjoin$Country[which(plyrjoin$Country=="Trinidad & Tobago")] <- "Trinidad and Tobago"
plyrjoin$Country[c(171,166)] <- "Hong Kong"
plyrjoin$Country[66] <- "Northern Cyprus"

#Merge all rows with same names
fin <- merge(plyrjoin[which(plyrjoin$`Score 2015`>0),c(1,2)],
      plyrjoin[which(plyrjoin$`Score 2016`>0),c(1,3)],by="Country",all=T)
fin <- merge(fin,
             plyrjoin[which(plyrjoin$`Score 2017`>0),c(1,4)],by="Country",all=T)
fin <- merge(fin,
             plyrjoin[which(plyrjoin$`Score 2018`>0),c(1,5)],by="Country",all=T)
fin <- merge(fin,
             plyrjoin[which(plyrjoin$`Score 2019`>0),c(1,6)],by="Country",all=T)
fin <- merge(fin,
             plyrjoin[which(plyrjoin$`Score 2020`>0),c(1,7)],by="Country",all=T)

#Pool data by taking the mean
fin$Mean <- rowMeans(fin[,c(2:7)],na.rm = TRUE)



