CIS 4930 Fall 2015: Data Minining
Final Project Roadmap
========================================================
author: Dax Gerts, Christine Moore, Carl Amko, Denzel Mathew
date: December 2nd & 4th, 2015
width: 1440
height: 900

Project Tasks
========================================================

1. Dataset Creation
2. Genre Prediction (classification)
3. "The Usual casts" (association rule mining)
4. Finding similar movies (clustering)

1. Dataset Creation - Source
========================================================

The IMDB Data was retrieved from the site at *http://www.imdb.com/interfaces*

A basic procedure for downloading and preprocessing the datasets would look something like the following,


```r
temp <- tempfile() #Prepare space for downloading files
download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/actors.list.gz",temp)
rawData <- readLines(temp)
unlink(temp) #Destroy temp file
rawData
```

1. Dataset Creation - Helper Functions
========================================================


```r
downloadRawDataset <- function(choice) {
  temp <- tempfile() #Prepare space for downloading files
  
  if(choice == "actors") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/actors.list.gz",temp) }
  if(choice == "actresses") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/actresses.list.gz",temp) }
  if(choice == "complete-cast") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/complete-cast.list.gz",temp) }
  if(choice == "composers") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/directors.list.gz",temp) }
  if(choice == "directors") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/directors.list.gz",temp) }
  if(choice == "genres") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/genres.list.gz",temp) }
  if(choice == "keywords") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/keywords.list.gz",temp)}
#...etc
  
  rawData <- readLines(temp)
  unlink(temp)
  rawData
}
```

1. Dataset Creation - Helper Functions (cont.)
========================================================


```r
exploreData <- function(choice,length = 50) {
  dt <- downloadRawDataset(choice)
  print(head(dt,length))
  cat("Press [enter] to continue")
  line <- readline()
  print(tail(dt,length))
  dt
}
```