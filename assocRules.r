# Project: CIS4930 Group Project
# Authors: Dax Gerts, ...
# Date: 23 November 2015
# Description: Procedure for detecting "usual casts" (association rule mining)

#Dynamically load/install required packages
ready <- FALSE
loadPackages <- function() {
  if( require(R.utils) == FALSE) { install.packages("R.utils") }
  ready <- TRUE
}
while(ready == FALSE) { ready <- loadPackages() }

#Source dataset creation tools/loads base libraries
source("datasetCreation.r")

#Build optimized association rules dataset
buildAssocRulesData()