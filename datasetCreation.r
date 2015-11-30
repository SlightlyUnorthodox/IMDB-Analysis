# Project: CIS4930 Group Project
# Authors: Dax Gerts, ...
# Date: 23 November 2015
# Description: Methods for creation of IMDB dataset

#Dynamically load/install required packages
ready <- FALSE
loadPackages1 <- function() {
  if( require(R.utils) == FALSE) { install.packages("R.utils") }
  ready <- TRUE
}
while(ready == FALSE) { ready <- loadPackages1() }

#Function to streamline file download and initial parsing
#Complete list of active datasets (NOTE: this is far from all of them, and I'm not sure if all of these are even useful -Dax)

# Example/  movies.data.raw <- downloadRawDataset("movies")

# -"actors"
# -"actresses"
# -"complete-cast"
# -"composers"
# -"directors"
# -"genres"
# -"keywords"
# -"literature"
# -"locations"
# -"movies"
# -"movie-links"
# -"plot"
# -"producers"
# -"ratings"
# -"release-dates"
# -"running-times"

downloadRawDataset <- function(choice) {
  temp <- tempfile() #Prepare space for downloading files
  
  if(choice == "actors") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/actors.list.gz",temp) }
  if(choice == "actresses") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/actresses.list.gz",temp) }
  if(choice == "complete-cast") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/complete-cast.list.gz",temp) }
  if(choice == "composers") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/directors.list.gz",temp) }
  if(choice == "directors") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/directors.list.gz",temp) }
  if(choice == "genres") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/genres.list.gz",temp) }
  if(choice == "keywords") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/keywords.list.gz",temp)}
  if(choice == "literature") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/literature.list.gz") }
  if(choice == "locations") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/locations.list.gz",temp) }
  if(choice == "movies") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/movies.list.gz",temp) }
  if(choice == "movie-links") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/movie-links.list.gz",temp) }
  if(choice == "plot") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/plot.list.gz",temp) }
  if(choice == "producers") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/producers.list.gz",temp) }
  if(choice == "ratings") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/ratings.list.gz",temp) }
  if(choice == "release-dates") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/release-dates.list.gz",temp) }
  if(choice == "running-times") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/running-times.list.gz",temp) }
  
  rawData <- readLines(temp)
  unlink(temp)
  rawData
}

#Function to streamline data examination, choice arg same as above list,length is number passed to head/tail
# Calls download function, prints header and tail
# Example:  testing <- exploreData("running-times",30)
exploreData <- function(choice,length = 50) {
  dt <- downloadRawDataset(choice)
  print(head(dt,length))
  cat("Press [enter] to continue")
  line <- readline()
  print(tail(dt,length))
  dt
}



buildAssocRulesData <- function() {
  
}

buildClusteringData <- function() {
  
}

buildClassificationData <- function() {
  
}


