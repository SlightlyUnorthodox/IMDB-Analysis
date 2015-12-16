# Project: CIS4930 Group Project
# Authors: Dax Gerts, ...
# Date: 23 November 2015
# Description: Procedure for detecting "usual casts" (association rule mining)

#Source data prep files
source('datasetCreation.r')

#Dynamically load/install required packages
ready <- FALSE
loadPackages1 <- function() {
  if( require(R.utils) == FALSE) { install.packages("R.utils") }
  if( require(stringr) == FALSE) { install.packages("stringr") }
  if( require(data.table) == FALSE) { install.packages("data.table") }
  if( require(jsonlite) == FALSE) { install.packages("jsonlite") }
  if( require(arules) == FALSE) { install.packages("arules") }
  if( require(arulesViz) == FALSE) { install.packages("arulesViz") }
  ready <- TRUE
}
while(ready == FALSE) { ready <- loadPackages1() }

#Set seed
set.seed(7131)

# Load dataset/subset dataset
# Must contain (directors,authors/writers,actors/actresses,etc.)

assocData <- readRDS("clean10Kdataset.rds")
assocData <- assocData[,c(4,5,10)]
assocData <- lapply(assocData,factor)



# Go forth and find interesting rules
#
#
#
#
#
#


# Redundant Rules function from Project 2
#define redundant rules function
redundantRules <- function(rules) {
  sub <- is.subset(rules,rules)
  sub[lower.tri(sub,diag=T)] <- NA
  red <- colSums(sub,na.rm=T) >= 1
  rrules <- rules[!red]
  rrules
} 



# Results for this part depend on...

# The number of different roles you have in your dataset.

# What you put in the left-hand-side of the rules.

# How you would automatically check all the possibilities.

# Moreover, provide some rules which indicate the relation between genre and the cast. You
#should explain why this finding is aligned (or not) with your results for the genre prediction
#part.


