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

#NOTE:

#It appears that that the format is roughly as follows:

# [1]     [2]   [3]   [4]   [5]   [6]   [7]
# Title   Year     
unlink(temp)

