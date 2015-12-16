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
assocDataSubset <- assocData[,c("Director","Writer","Actor1","Actor2","Actor3","Actor4")]
#assocDataSubset <- assocData[,c("Director","Writer","Actors","Producer","Cinematographer","Composer","CostumerDesigners","Genre")]

#Build ruleset
movieRules <- apriori(assocDataSubset,parameter = list(support = 0.0001,confidence = 0.9))
inspect(head(movieRules))

#Removes redundant rules
uniqueMovieRules <- redundantRules(movieRules)
#inspect(titanicUniqBetterRulesApriori)

#Sorts trimmed rules by lift
sortedUniqueMovieRules <- sort(uniqueMovieRules, by = "lift")
inspect(head(sortedUniqueMovieRules,40))


