# Project: CIS4930 Group Project
# Authors: Dax Gerts, ...
# Date: 23 November 2015
# Description: Procedure for detecting "usual casts" (association rule mining)

#Dynamically load/install required packages
library(arules)
library(reshape2)
library(stringr)

# Load dataset/subset dataset
assocData <- readRDS("clean10Kdataset.rds")

# Remove columns not worthy of analysis
assocDataSubset <- assocData[,c(4,5,10)]
assocDataSubset$Writer <- as.factor(assocDataSubset$Writer)
assocDataSubset$Director <- as.factor(assocDataSubset$Director)
assocDataSubset$Actors <- colsplit(assocDataSubset$Actors, ",", names=c('Actor1','Actor2','Actor3','Actor4'))
assocDataSubset$Actor1 <- assocDataSubset$Actors$Actor1
assocDataSubset$Actor2 <- assocDataSubset$Actors$Actor2
assocDataSubset$Actor3 <- assocDataSubset$Actors$Actor3
assocDataSubset$Actor4 <- assocDataSubset$Actors$Actor4

assocDataSubset$Actors <- NULL

assocDataSubset$Actor1 <- gsub('^c\\(','',assocDataSubset$Actor1)
assocDataSubset$Actor4 <- gsub('\\)\\s*$','',assocDataSubset$Actor4)

assocDataSubset$Actor1 <- as.character(assocDataSubset$Actor1)
assocDataSubset$Actor2 <- as.character(assocDataSubset$Actor2)
assocDataSubset$Actor3 <- as.character(assocDataSubset$Actor3)
assocDataSubset$Actor4 <- as.character(assocDataSubset$Actor4)

# assocDataSubset$Actor1 <- as.factor(assocDataSubset$Actor1)
# assocDataSubset$Actor2 <- as.factor(assocDataSubset$Actor2)
# assocDataSubset$Actor3 <- as.factor(assocDataSubset$Actor3)
# assocDataSubset$Actor4 <- as.factor(assocDataSubset$Actor4)

assocDataSubset$Director[assocDataSubset$Director=="N/A"] <- NA
assocDataSubset$Writer[assocDataSubset$Writer=="N/A"] <- NA

assocDataSubset$Actor1[assocDataSubset$Actor1=="N/A"] <- NA
assocDataSubset$Actor2[assocDataSubset$Actor2=="N/A"] <- NA
assocDataSubset$Actor3[assocDataSubset$Actor3=="N/A"] <- NA
assocDataSubset$Actor4[assocDataSubset$Actor4=="N/A"] <- NA




