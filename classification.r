# Project: CIS4930 Group Project
# Authors: Dax Gerts, ...
# Date: 23 November 2015
# Description: Genre prediction (classification) procedure

#Dynamically load/install required packages
ready <- FALSE
loadPackages3 <- function() {
  if( require(R.utils) == FALSE) { install.packages("R.utils") }
  ready <- TRUE
}
while(ready == FALSE) { ready <- loadPackages3() }

#Source dataset creation tools/loads base libraries
source("datasetCreation.r")

#Build optimized classification data
buildClassificationData()
