# Project: CIS4930 Group Project
# Authors: Dax Gerts, ...
# Date: 23 November 2015
# Description: Procedure for detecting "usual casts" (association rule mining)

#Dynamically load/install required packages
ready <- FALSE
loadPackages4 <- function() {
  if( require(R.utils) == FALSE) { install.packages("R.utils") }
  
  ready <- TRUE
}
while(ready == FALSE) { ready <- loadPackages4() }

# Load dataset/subset dataset
#
#
# Must contain (directors,authors/writers,producers/actors,actresses,etc.)
#



# Go forth and find interesting rules
#
#
#
#
#
#


# Results for this part depend on...

# The number of different roles you have in your dataset.

# What you put in the left-hand-side of the rules.

# How you would automatically check all the possibilities.

# Moreover, provide some rules which indicate the relation between genre and the cast. You
#should explain why this finding is aligned (or not) with your results for the genre prediction
#part.


