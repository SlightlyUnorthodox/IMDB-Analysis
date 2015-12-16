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

#define redundant rules function
redundantRules <- function(rules) {
  sub <- is.subset(rules,rules)
  sub[lower.tri(sub,diag=T)] <- NA
  red <- colSums(sub,na.rm=T) >= 1
  rrules <- rules[!red]
  rrules
} 


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

#Build ruleset
movieRules <- apriori(assocDataSubset,parameter = list(support = 0.0001,confidence = 0.9))
inspect(head(movieRules))

#Removes redundant rules
uniqueMovieRules <- redundantRules(movieRules)
#inspect(titanicUniqBetterRulesApriori)

#Sorts trimmed rules by lift
sortedUniqueMovieRules <- sort(uniqueMovieRules, by = "lift")
inspect(head(sortedUniqueMovieRules,40))


