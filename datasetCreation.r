# Project: CIS4930 Group Project
# Authors: Dax Gerts, ...
# Date: 23 November 2015
# Description: Methods for creation of IMDB dataset

#Dynamically load/install required packages
ready <- FALSE
loadPackages <- function() {
  if( require(R.utils) == FALSE) { install.packages("R.utils") }
  ready <- TRUE
}
while(ready == FALSE) { ready <- loadPackages() }

#Function to streamline file download and initial parsing
#Complete list of active datasets (NOTE: this is far from all of them, and I'm not sure if all of these are even useful -Dax)

# Example/  movies.data.raw <- downloadRawDataset("movies")

# -"actors"
# -"actresses"
# -"complete-cast"
# -"composers"
# -"directors"
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
  
  if(choice == "actors") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/actors.list.gz") }
  if(choice == "acresses") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/actresses.list.gz") }
  if(choice == "complete-cast") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/complete-cast.list.gz") }
  if(choice == "composers") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/directors.list.gz") }
  if(choice == "directors") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/directors.list.gz") }
  if(choice == "keywords") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/keywords.list.gz")}
  if(chocie == "literature") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/literature.list.gz") }
  if(choice == "locations") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/locations.list.gz") }
  if(choice == "movies") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/movies.list.gz",temp) }
  if(choice == "movie-links") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/movie-links.list.gz") }
  if(choice == "plot") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/plot.list.gz") }
  if(choice == "producers") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/producers.list.gz") }
  if(choice == "ratings") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/ratings.list.gz") }
  if(choice == "release-dates") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/release-dates.list.gz") }
  if(choice == "running-times") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/running-times.list.gz") }
  
  rawData <- readLines(temp)
  unlink(temp)
  rawData
}

#Testing process, recorded for posterity and to explain data processing decisions
testProcedure <- function() {
  #Prepare space for downloading files
  temp <- tempfile()
  
  
  #NOTE: had to figure out the data cleaning process, used 'movies.list.gz' as the trial case
  
  #Down from IMDB FTP server to temp file
  download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/movies.list.gz",temp)
  #Skip manual decompression and read directly as raw text
  movies.data.raw <- readLines(temp)
  
  #Now to see what's inside
  head(movies.data.raw,100)
  
  #Now to check end contents
  tail(movies.data.raw,20)
  
  #NOTE: Will need to skip 14 rows to remove file heading
  
  #It appears that that the format is roughly as follows:
  
  # [1]     [2]   [3]   [4]   [5]   [6]   [7]
  # Title   Year     
  
  unlink(temp)
}




