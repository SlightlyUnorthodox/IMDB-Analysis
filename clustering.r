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

# Load dataset
data <- readRDS("clean10Kdataset.rds")

#subset dataset to Genre list
dataGenre <- data[, 13]

#Prune the data to remove excess collection space
dataPruned <- lapply(dataGenre, function(x) {
  # Make the collection 'unique', removing all the blank collection spots
  x <- unique(x)
  
  # chop off the last 'NA' value
  x <- x[-length(x)]
  
})

# attempt to transform each unique entry with an id
dataPruned <- transform(dataGenre, id=seq_len(length(unique(dataGenre))))

# count unique genres
length(unique(dataPruned))


# Go forth and find similar movies
#
#
#
#
#
#






# Questions

# Which clustering method works best in this case? And why.

# Would clustering the movies into k' clusters where k' > k help in better categorization? And how.
