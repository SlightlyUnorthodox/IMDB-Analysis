# Project: CIS4930 Group Project
# Authors: Dax Gerts, ...
# Date: 23 November 2015
# Description: Methods for creation of IMDB dataset

#Dynamically load/install required packages
ready <- FALSE
loadPackages1 <- function() {
  if( require(R.utils) == FALSE) { install.packages("R.utils") }
  if( require(stringr) == FALSE) { install.packages("stringr") }
  if( require(data.table) == FALSE) { install.packages("data.table") }
  if( require(jsonlite) == FALSE) { install.packages("jsonlite") }
  if( require(plyr) == FALSE) { install.packages("dplyr")}
  if( require(ggplot2) == FALSE) { install.packages("ggplot2")}
  ready <- TRUE
}
while(ready == FALSE) { ready <- loadPackages1() }

#Set seed
set.seed(7131)

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
# -"writers"

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
  if(choice == "writers") { download.file("ftp://ftp.fu-berlin.de/pub/misc/movies/database/writers.list.gz",temp) }
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

# Runtime: __ on 8GB of memmory
# Parameter: size of dataset, default 10,000 movies
#   NOTE: size must be <= size of dataset/2
DEPRICATEDbuildDataset <- function(size = 10000) {
  
  #
  # MOVIES
  #
  
  # Download movies information, serves as key for building rest of table
  movies <- downloadRawDataset("movies")
  
  # Remove head and tail information
  movies <- movies[16:3570077]
  
  # Filter episodes out of data
  episodes <- grep("\\{",movies)
  movies <- movies[-episodes]
  
  # Select random sample size, keeps data at workable size
  dataset <- movies[sample(1:length(movies),size*2,replace=FALSE)]
  
  # Read dataset into tabular form, add colname, trim excess values
  dataset <- read.delim(text = dataset,header=FALSE)
  dataset <- dataset[,1]
  
  # Extract years as separate column (MESSY)
  dataset <- do.call(rbind, str_split(dataset,' \\('))
  dataset <- dataset[,1:2]
  dataset[,2] <- substr(dataset[,2],1,4)
  
  # Set column names
  colnames(dataset) <- c("movie","year")
  
  # Reset as data frame with appropriate attributes
  dataset <- data.frame(dataset)
  dataset$year <- as.numeric(as.character(dataset$year))
  
  # Filter out empty year values
  dataset <- na.omit(dataset)
  
  # Reduce sample size to original requested amount
  dataset <- dataset[1:size,]
  
  # Remove uneeded data
  rm(episodes)
  rm(movies)
  
  #
  # GENRES
  #
  
  # Download genres data
  genres <- downloadRawDataset("genres")
  
  # Remove head and tail information
  genres <- genres[381:length(genres)]
  
  # Filter noise data
  suspended <- grep("\\{",genres)
  genres <- genres[-suspended]
  
  # Read dataset into tabular form, add colname, trim excess values
  genre.set <- read.table(text=head(genres,100000))
  genre.set[,1] <- substr(genre.set[,1],2,length(genre.set[,1]))
  genre.set[,2] <- substr(genre.set[,2],2,5)
  #genre.set <- genre.set[,c(1,3)]
  
  # Reset column names
  colnames(genre.set) <- c("movie","year","genre")
  
  # Reset as data frame with appropriate attributes
  genre.set <- data.frame(genre.set)
  genre.set$year <- as.numeric(as.character(genre.set$year))
  
  # Filter out empty year values
  genre.set <- na.omit(genre.set)
  
  # Remove uneeded data
  rm(genres)
  
  #
  # ACTORS
  #
  
  # Download actors information
  #actors <- downloadRawDataset("actors")
  
  # Remove head and tail information
  #actors <- actors[240:18387396]
  
  # Read actor.set into tabular form, trim excess columns, merge
  #actor.set <- read.delim(text = actors,header=FALSE)
  #actorset <- actorset[,1:2]
  #dataset <- dataset[actor.set]
  
  #
  # ACTRESSES
  #
  
  # Download actresses information
  #actresses <- downloadRawDataset("actresses")
  
  # Remove head and tail information
  
  # Read actress.set into tabular form, trim excess columns, merge
  #actress.set <- read.delim(text = actresses,header=FALSE)
  #actress.set <- actress.set[,1:2]
  #datset <- dataset[actress.set]
  
  #
  # DIRECTORS
  #
  
  # Download directors information
  #directors <- downloadRawDataset("directors")
  
  # Remove head and tail information
  #directors <- directors[238:2762469]
  
  # Read director.set into tabular form, trim excess columns, merge
  
  
  #
  # PRODUCERS
  #
  
  #
  # WRITERS
  #
  
}

# Runtime reformat, string -> minutes (integer)
timeSet <- function(x) {
  if(length(x == 2)) {
    x = as.numeric(x) 
  } else if (length(x == 4))  {
    x = (as.numeric(substr(x,1,1))*60)+as.numeric(substr(x,2,3))
  } else {
    x = as.numeric("0")
  }
  x
}


# Reformat OMDB JSON queries as csv
jsonToCsv <- function(filename = "imdb_30K_sample.json",write=TRUE,csvfile = "imdb_30K_sample.csv") {
  data <- fromJSON(txt=as.character(filename))
  if(write==TRUE) {
    write.csv(data,file=csvfile,row.names = FALSE)
  }
  data
}

# Runtime reformat, string -> minutes (integer)
timeSet <- function(x) {
  if(length(x == 2)) {
    x = as.numeric(x) 
  } else if (length(x == 4))  {
    x = (as.numeric(substr(x,1,1))*60)+as.numeric(substr(x,2,3))
  } else {
    x = as.numeric("0")
  }
  x
}


# Clean data set, set proper types, drop invalid rows, 
preprocessing <- function(data) {
  
  # Step 1: Variable type checking
  
  # 1.1 Plot - char (fine as is)
  
  # 1.2 Rated - factor (49 levels) (sparse)
  data$Rated <- as.factor(data$Rated)
  data$Rated[data$Rated=="N/A"] <- NA
  summary(data$Rated)
  
  # 1.3 Title - char (fine as is)
  
  # 1.4 Writer - factor (>10000 levels)
  data$Writer <- as.factor(data$Writer)
  data$Writer[data$Writer=="N/A"] <- NA
  summary(data$Writer)
  
  # 1.5 Actors - list ==> factors
  data$Actors <- strsplit(data$Actors,", ")
  data$Actors <- sapply(data$Actors,'[',seq(max(sapply(data$Actors,length))),simplify=FALSE)
  data$Actors[1:5]
  
  ### BEGIN ACTORS FIX
  
  # Description: Actors was broken into 4 columns (setting max number) and re-parsed as factors
  
  #Split actors to temp unique columns
  temp<- ldply(data$Actors)
  colnames(temp) <- c('Actor1','Actor2','Actor3','Actor4')
  
  #Reassign actors to working data frame under new names ("Actor1","Actor2","Actor3","Actor4")
  data$Actor1 <- temp[1]
  data$Actor2 <- temp[2]
  data$Actor3 <- temp[3]
  data$Actor4 <- temp[4]
  
  #Delete temporary data frame
  rm(temp)
  
  #Drop defunct actors column
  data <- data[,-c(5)]
  
  #Catch alternate NAs
  data$Actor1[data$Actor1=="N/A"] <- NA
  data$Actor2[data$Actor2=="N/A"] <- NA
  data$Actor3[data$Actor3=="N/A"] <- NA
  data$Actor4[data$Actor4=="N/A"] <- NA
  
  #Unlist and set actors as factors
  data$Actor1 <- as.factor(unlist(data$Actor1))
  data$Actor2 <- as.factor(unlist(data$Actor2))
  data$Actor3 <- as.factor(unlist(data$Actor3))
  data$Actor4 <- as.factor(unlist(data$Actor4))
  
  ### END ACTORS FIX
  
  ### START CLEANING UP PRODUCER AND CINEMATOGRAPHER
  ### producer first
  
  # single quote removal
  data$Producer <- lapply(data$Producer, function(x){
    gsub("\'", "", x)#
  })
  
  # double quote removal
  data$Producer <- lapply(data$Producer, function(x){
    gsub("\"", "", x)#
  })
  
  # remove brackets
  data$Producer <- lapply(data$Producer, function(x){
    gsub("\\[", "", x)#
  })
  
  data$Producer <- lapply(data$Producer, function(x){
    gsub("\\]", "", x)#
  })
  
  data$Producer <- lapply(data$Producer, function(x){
    strsplit(x,",")
  })
  
  firstName <- lapply(data$Producer, function(x){
    x[[1]][2]
  })
  
  lastName <- lapply(data$Producer, function(x){
    x[[1]][1]
  })
  
  data$Producer <- paste(firstName, lastName)
  
  ### end fixing of producer 
  
  ### fix cinematographer
  
  # single quote removal
  data$Cinematographer <- lapply(data$Cinematographer, function(x){
    gsub("\'", "", x)#
  })
  
  # double quote removal
  data$Cinematographer <- lapply(data$Cinematographer, function(x){
    gsub("\"", "", x)#
  })
  
  # remove brackets
  data$Cinematographer <- lapply(data$Cinematographer, function(x){
    gsub("\\[", "", x)#
  })
  
  data$Cinematographer <- lapply(data$Cinematographer, function(x){
    gsub("\\]", "", x)#
  })
  
  data$Cinematographer <- lapply(data$Cinematographer, function(x){
    strsplit(x,",")
  })
  
  firstName <- lapply(data$Cinematographer, function(x){
    x[[1]][2]
  })
  
  lastName <- lapply(data$Cinematographer, function(x){
    x[[1]][1]
  })
  
  data$Cinematographer <- paste(firstName, lastName)
  
  ### end fixing cinematographer
  
  ### END OF CLEANING UP PRODUCER AND CINEMATOGRAPHER
  
  # 1.6 Type - factor
  data$Type <- as.factor(data$Type)
  data$Type[data$Type=="N/A"] <- NA
  summary(data$Type)
  
  # 1.7 imdbVotes - numeric
  data$imdbVotes <- as.numeric(data$imdbVotes)
  summary(data$imdbVotes)
  
  # 1.8 seriesID - char (fine as is)
  
  # 1.9 Season
  data$Season <- as.numeric(data$Season)
  summary(data$Season)
  
  # 1.10 Director - factor (> 10000 levels)
  data$Director <- as.factor(data$Director)
  data$Director[data$Director=="N/A"] <- NA
  summary(data$Director)
  
  # 1.11 Released - date 
  data$Released <- as.Date(data$Released,"%d %b %Y")
  
  # 1.12 Awards - to finicky to do anything with now (parse for numeric values later, maybe)
  
  # 1.13 Genre - fact list
  data$Genre <- strsplit(data$Genre,", ")
  data$Genre <- sapply(data$Genre,'[',seq(max(sapply(data$Genre,length))),simplify=FALSE)
  data$Genre[1:5]
  
  # 1.14 imdbRating - numeric
  data$imdbRating <- as.numeric(data$imdbRating)
  
  # 1.15 Poster - char (fine as is)
  
  # 1.16 Episode - numeric
  data$Episode <- as.numeric(data$Episode)
  
  # 1.17 Language - factor list
  data$Language <- strsplit(data$Language,", ")
  data$Language <- sapply(data$Language,'[',seq(max(sapply(data$Language,length))),simplify=FALSE)
  data$Language[1:5]
  
  # 1.18 Country
  data$Country <- strsplit(data$Country,", ")
  data$Country <- sapply(data$Country,'[',seq(max(sapply(data$Country,length))),simplify=FALSE)
  data$Country[1:5]
  
  # 1.19 Runtime - numeric (calls setTime function to convert values)
  data$Runtime <- gsub("[^0-9]"," ",data$Runtime)
  for(i in 1:length(data$Runtime)) {
    data$Runtime[i] = timeSet(data$Runtime[i])
  }
  data$Runtime <- as.numeric(data$Runtime)
  summary(data$Runtime)
  
  # 1.20 imdbID (fine as is)
  
  # 1.21 Metascore
  data$Metascore <- as.factor(data$Metascore)
  data$Metascore[data$Metascore=="N/A"] <- NA
  levels(data$Metascore)
  
  # 1.22 Response (irrelevant)
  
  # 1.23 Year - as numeric (only takes first year in range)
  data$Year <- as.numeric(gsub("\\-.*","",data$Year))
  summary(data$Year)
  
  # 1.24 Error (fine as is, meaningless)
  
  # 2 Load and prep extra cast values
  
  # 2.1 Name columns
  temp <- read.csv("imdb_10K_cast_plus.csv",header=FALSE)
  colnames(temp) <- c("imdbID","Producer","Cinematographer","Composer","CostumeDesigners")
  
  # 2.2 Format IDs to match
  temp$imdbID <- sapply(temp$imdbID,function(x) sprintf("%07d",x))
  temp$imdbID <- paste("tt",as.character(temp$imdbID),sep="")
  
  # 2.3 Merge new values to table
  data <- merge(data,temp,by="imdbID",all=TRUE)
  
  # 2.4 Format new variables
  data$Producer <- as.factor(data$Producer)
  data$Cinematographer <- as.factor(data$Cinematographer)
  data$Composer <- as.factor(data$Composer)
  data$CostumeDesigners <- as.factor(data$CostumeDesigners)
  # 3 Prepare valid data
  
  # 3.1 Drop non-movie entries
  data <- data[data$Type == "movie",]
  
  # 3.2 Drop bad/"N/A" entries
  data <- data[!(is.na(as.factor(data$Title))),]
  
  # 3.3 Save table as R object
  saveRDS(data,"clean10Kdataset.rds")
  
  # 3.4 Write clean csv
  csvData <- data.frame(lapply(data,as.character),stringsAsFactors=FALSE)
  write.csv(csvData,file="clean10Kdataset.csv",row.names = FALSE)
  
  # 3.5 Return output table
  data
}


