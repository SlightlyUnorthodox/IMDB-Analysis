# Project: CIS4930 Group Project
# Authors: Dax Gerts, ...
# Date: 23 November 2015
# Description: Genre prediction (classification) procedure

#Dynamically load/install required packages
ready <- FALSE
loadPackages3 <- function() {
  if( require(R.utils) == FALSE) { install.packages("R.utils") }
  if (require(caret) == FALSE) { install.packages("caret") }
  ready <- TRUE
}
while(ready == FALSE) { ready <- loadPackages3() }

# Load dataset/subset dataset
#
#
#
#

#Set seed
set.seed(7131)

# Partition data
part <- createDataPartition(dataset$genre,p=0.8,list=FALSE)
training <- dataset[part,]
test <- dataset[-part,]

# Save training/test data for later
write.csv(training,file="training.csv")
write.csv(test,file="test.csv")

# Go forth and build a good model
#
#
#
#
#
#



# Questions

# What were your options for classification?

# How did you evaluate different classification techniques?

# What measures have you taken to improve the results?