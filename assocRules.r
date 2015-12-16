# Project: CIS4930 Group Project
# Authors: Dax Gerts, Denzel
# Date: 23 November 2015
# Description: Procedure for detecting "usual casts" (association rule mining)

#Source data prep files
# source('datasetCreation.r')

#Dynamically load/install required packages

ready <- FALSE
loadPackages3 <- function() {
  if( require(arules) == FALSE) { install.packages("arules") }
  if( require(reshape2) == FALSE) { install.packages("reshape2") }
  if( require(stringr) == FALSE) { install.packages("stringr") }
  if( require(plyr) == FALSE) { install.packages("dplyr")}
  ready <- TRUE
}
while(ready == FALSE) { ready <- loadPackages3() }

set.seed(7131)

# Load dataset/subset dataset
assocData <- readRDS("clean10Kdataset.rds")

# Remove columns not worthy of analysis
assocDataSubset <- assocData[,c(5,6,11)]

#Split actors to temp unique columns
temp<- ldply(assocDataSubset$Actors)
colnames(temp) <- c('Actor1','Actor2','Actor3','Actor4')

#Reassign actors
assocDataSubset$Actor1 <- temp[1]
assocDataSubset$Actor2 <- temp[2]
assocDataSubset$Actor3 <- temp[3]
assocDataSubset$Actor4 <- temp[4]

#Delete temporary data frame
rm(temp)

#Drop defunct actors column
assocDataSubset <- assocDataSubset[,-c(2)]

assocDataSubset$Writer <- as.factor(assocDataSubset$Writer)
assocDataSubset$Director <- as.factor(assocDataSubset$Director)

assocDataSubset$Actor1 <- as.factor(assocDataSubset$Actor2)

assocDataSubset$Director[assocDataSubset$Director=="N/A"] <- NA
assocDataSubset$Writer[assocDataSubset$Writer=="N/A"] <- NA

#Catch alternate NAs
assocDataSubset$Actor1[assocDataSubset$Actor1=="N/A"] <- NA
assocDataSubset$Actor2[assocDataSubset$Actor2=="N/A"] <- NA
assocDataSubset$Actor3[assocDataSubset$Actor3=="N/A"] <- NA
assocDataSubset$Actor4[assocDataSubset$Actor4=="N/A"] <- NA

#Unlist and set actors as factors
assocDataSubset$Actor1 <- as.factor(unlist(assocDataSubset$Actor1))
assocDataSubset$Actor2 <- as.factor(unlist(assocDataSubset$Actor2))
assocDataSubset$Actor3 <- as.factor(unlist(assocDataSubset$Actor3))
assocDataSubset$Actor4 <- as.factor(unlist(assocDataSubset$Actor4))

#sapply(unlist(assocDataSubset$Actor1, recursive = TRUE, use.names = TRUE))
#sapply(unlist(assocDataSubset$Actor2, recursive = TRUE, use.names = TRUE))
#sapply(unlist(assocDataSubset$Actor3, recursive = TRUE, use.names = TRUE))
#sapply(unlist(assocDataSubset$Actor4, recursive = TRUE, use.names = TRUE))


dataFiltered <- apriori(assocDataSubset, support=0.1, confidence= 0.2)

