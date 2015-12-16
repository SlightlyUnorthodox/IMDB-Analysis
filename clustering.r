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
#dataGenre <- data[, 13]

#Prune the data to remove excess collection space
#dataPruned <- lapply(dataGenre, function(x) {
  # Make the collection 'unique', removing all the blank collection spots
#  x <- unique(x)
  
  # chop off the last 'NA' value
#  x <- x[-length(x)]
  
#})

# attempt to transform each unique entry with an id
#dataPruned <- transform(dataGenre, id=seq_len(length(unique(dataGenre))))

# count unique genres
#length(unique(dataPruned))

makeSet <- function(x){
  x$Action <- 0
  x$Adventure <- 0
  x$Animation <- 0
  x$Biography <- 0
  x$Comedy <- 0
  x$Crime <- 0
  x$Documentary <- 0
  x$Drama <- 0
  x$Family <- 0
  x$Fantasy <- 0
  x$FilmNoir <- 0
  x$History <- 0
  x$Horror <- 0
  x$Music <- 0
  x$Musical <- 0
  x$Mystery <- 0
  x$Romance <- 0
  x$SciFi <- 0
  x$Short <- 0
  x$Sport <- 0
  x$Thriller <- 0
  x$War <- 0
  x$Western <- 0
  for(i in 1:nrow(x)){
    coll <- x[i,"Genre"]
    den = 1
    for(q in 1:length(coll[[1]])){
      if(is.na(coll[[1]][q])){
      } else {
        den <- q
      }
    }
    value <- 1/den
    for(j in 1:length(coll[[1]])){
      if (is.na(coll[[1]][j])){
        next
      }
      if (coll[[1]][j] == "Action"){
        x[i,"Action"] = value
      }
      if (coll[[1]][j] == "Adventure"){
        x[i,"Adventure"] = value
      }
      if (coll[[1]][j] == "Animation"){
        x[i,"Animation"] = value
      }
      if (coll[[1]][j] == "Biography"){
        x[i,"Biography"] = value
      }
      if (coll[[1]][j] == "Comedy"){
        x[i,"Comedy"] = value
      }
      if (coll[[1]][j] == "Crime"){
        x[i,"Crime"] = value
      }
      if (coll[[1]][j] == "Documentary"){
        x[i,"Documentary"] = value
      }
      if (coll[[1]][j] == "Drama"){
        x[i,"Drama"] = value
      }
      if (coll[[1]][j] == "Family"){
        x[i,"Family "] = value
      }
      if (coll[[1]][j] == "Fantasy"){
        x[i,"Fantasy"] = value
      }
      if (coll[[1]][j] == "Film-Noir"){
        x[i,"FilmNoir"] = value
      }
      if (coll[[1]][j] == "History"){
        x[i,"History"] = value
      }
      if (coll[[1]][j] == "Horror"){
        x[i,"Horror"] = value
      }
      if (coll[[1]][j] == "Music"){
        x[i,"Music"] = value
      }
      if (coll[[1]][j] == "Musical"){
        x[i,"Musical"] = value
      }
      if (coll[[1]][j] == "Mystery"){
        x[i,"Mystery "] = value
      }
      if (coll[[1]][j] == "Romance"){
        x[i,"Romance"] = value
      }
      if (coll[[1]][j] == "Sci-Fi"){
        x[i,"SciFi"] = value
      }
      if (coll[[1]][j] == "Sport"){
        x[i,"Sport"] = value
      }
      if (coll[[1]][j] == "Short"){
        x[i,"Short"] = value
      }
      if (coll[[1]][j] == "Thriller"){
        x[i,"Thriller"] = value
      }
      if (coll[[1]][j] == "War"){
        x[i,"War"] = value
      }
      if (coll[[1]][j] == "Western"){
        x[i,"Western"]  = value
      }
    }
  }
  y <- x [,c("Action", "Adventure", "Animation", "Biography", "Comedy", "Crime", "Documentary", "Drama", "Family", "Fantasy", "FilmNoir", "History", "Horror", "Music", "Musical", "Mystery", "Romance", "SciFi", "Sport", "Short", "Thriller", "War", "Western")]
  return (y)
}
distanceMatrix <- makeSet(data)

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
