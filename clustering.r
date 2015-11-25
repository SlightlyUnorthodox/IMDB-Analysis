# Project: CIS4930 Group Project
# Authors: Dax Gerts, ...
# Date: 23 November 2015
# Description: Procedure for finding similar movies (clustering)

#Dynamically load/install required packages
ready <- FALSE
loadPackages2 <- function() {
  if( require(R.utils) == FALSE) { install.packages("R.utils") }
  ready <- TRUE
}
while(ready == FALSE) { ready <- loadPackages2() }

#Source dataset creation tools/loads base libraries
source("datasetCreation.r")

#Build optimized clustering data
buildClusteringData()