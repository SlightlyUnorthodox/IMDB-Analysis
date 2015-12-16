# Project: CIS4930 Group Project
# Authors: Dax Gerts, Denzel
# Date: 23 November 2015
# Description: Procedure for detecting "usual casts" (association rule mining)

#Source assocData prep files
# source('assocDatasetCreation.r')

#Dynamically load/install required packages

ready <- FALSE
loadPackages3 <- function() {
  if( require(arules) == FALSE) { install.packages("arules") }
  if( require(reshape2) == FALSE) { install.packages("reshape2") }
  if( require(stringr) == FALSE) { install.packages("stringr") }
  if( require(tidyr) == FALSE) { install.packages("tidyr") }
  if( require(base) == FALSE) { install.packages("base") }
  ready <- TRUE
}
while(ready == FALSE) { ready <- loadPackages3() }

set.seed(7131)

#define redundant rules function
redundantRules <- function(rules) {
  sub <- is.subset(rules,rules)
  sub[lower.tri(sub,diag=T)] <- NA
  red <- colSums(sub,na.rm=T) >= 1
  rrules <- rules[!red]
  rrules
} 



makeEvenSet <- function(x){
  x$Action <- 0
  x$Adventure <- 0
  x$Adult <- 0
  x$Animation <- 0
  x$Biography <- 0
  x$Comedy <- 0
  x$Crime <- 0
  x$Documentary <- 0
  x$Drama <- 0
  x$Family <- 0
  x$Fantasy <- 0
  x$FilmNoir <- 0
  x$GameShow <- 0
  x$History <- 0
  x$Horror <- 0
  x$Music <- 0
  x$Musical <- 0
  x$Mystery <- 0
  x$None <- 0
  x$News <- 0
  x$RealityTV <- 0
  x$Romance <- 0
  x$SciFi <- 0
  x$Short <- 0
  x$Sport <- 0
  x$TalkShow <- 0
  x$Thriller <- 0
  x$War <- 0
  x$Western <- 0
  for(i in 1:nrow(x)){
    coll <- x[i,"Genre"]
    value <- 1
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
      if (coll[[1]][j] == "Adult"){
        x[i,"Adult"] = value
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
      if (coll[[1]][j] == "Game-Show"){
        x[i,"GameShow"] = value
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
      if (coll[[1]][j] == "N/A"){
        x[i,"None"] = value
      }
      if (coll[[1]][j] == "News"){
        x[i,"News "] = value
      }
      if (coll[[1]][j] == "Reality-TV"){
        x[i,"RealityTV"] = value
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
      if (coll[[1]][j] == "Talk-Show"){
        x[i,"TalkShow"] = value
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
  y <- x [,c("Action", "Adventure", "Adult", "Animation", "Biography", "Comedy", "Crime", "Documentary", "Drama", "Family", "Fantasy", "FilmNoir","GameShow" ,"History", "Horror", "Music", "Musical", "Mystery", "None","News","RealityTV","Romance", "SciFi", "Sport", "Short", "TalkShow", "Thriller", "War", "Western")]
  return (y)
}


#Read in the RDS
assocData <- readRDS("clean10Kdataset.rds")


#Get the Regular Association Rules Subset
assocDataSubset <- assocData[,c("Director","Producer","Writer","Cinematographer","Actor1","Actor2","Actor3","Actor4")]
# assocDataSubsetGC <- assocData[,c("Genre","Director","Producer","Writer","Cinematographer","Actor1","Actor2","Actor3","Actor4")]
# 
# 
# ran <- makeEvenSet(assocData)
# 
# #Reassign genre to working assocData frame under new names ("Action", "Adventure", "Adult", "Animation", "Biography", "Comedy", "Crime", "Documentary", "Drama", "Family", "Fantasy", "FilmNoir","GameShow" ,"History", "Horror", "Music", "Musical", "Mystery", "None","News","RealityTV","Romance", "SciFi", "Sport", "Short", "TalkShow", "Thriller", "War", "Western")
# assocData$Action <- ran[1]
# assocData$Adventure <- ran[2]
# assocData$Adult <- ran[3]
# assocData$Animation <- ran[4]
# assocData$Biography <- ran[5]
# assocData$Comedy <- ran[6]
# assocData$Crime <- ran[7]
# assocData$Documentary <- ran[8]
# assocData$Drama <- ran[9]
# assocData$Family <- ran[10]
# assocData$Fantasy <- ran[11]
# assocData$FilmNoir <- ran[12]
# assocData$GameShow <- ran[13]
# assocData$History <- ran[14]
# assocData$Horror <- ran[15]
# assocData$Music <- ran[16]
# assocData$Musical <- ran[17]
# assocData$Mystery <- ran[18]
# assocData$None <- ran[19]
# assocData$News <- ran[20]
# assocData$RealityTV <- ran[21]
# assocData$Romance <- ran[22]
# assocData$SciFi <- ran[23]
# assocData$Sport <- ran[24]
# assocData$Short <- ran[25]
# assocData$TalkShow <- ran[26]
# assocData$Thriller <- ran[27]
# assocData$War <- ran[28]
# assocData$Western <- ran[29]


#Drop defunct actors column
# assocData <- assocData[,-c(13)]

# 
# Unlist and set actors as factors
# assocData$Actor1 <- as.factor(unlist(assocData$Actor1))
# assocData$Actor2 <- as.factor(unlist(assocData$Actor2))
# assocData$Actor3 <- as.factor(unlist(assocData$Actor3))
# assocData$Actor4 <- as.factor(unlist(assocData$Actor4))
# 
# assocData$Action <- as.factor(unlist(assocData$Action))
# assocData$Adventure <- as.factor(unlist(assocData$Adventure))
# assocData$Adult <- as.factor(unlist(assocData$Adult))
# assocData$Animation <- as.factor(unlist(assocData$Animation))
# assocData$Biography <- as.factor(unlist(assocData$Biography))
# assocData$Comedy <- as.factor(unlist(assocData$Comedy))
# assocData$Crime <- as.factor(unlist(assocData$Crime))
# assocData$Documentary <- as.factor(unlist(assocData$Documentary))
# assocData$Drama <- as.factor(unlist(assocData$Drama))
# 
# assocData$Family <- as.factor(unlist(assocData$Family))
# assocData$Fantasy <- as.factor(unlist(assocData$Fantasy))
# assocData$FilmNoir <- as.factor(unlist(assocData$FilmNoir))
# assocData$GameShow <- as.factor(unlist(assocData$GameShow))
# assocData$History <- as.factor(unlist(assocData$History))
# assocData$Horror <- as.factor(unlist(assocData$Horror))
# assocData$Music <- as.factor(unlist(assocData$Music))
# assocData$Musical <- as.factor(unlist(assocData$Musical))
# assocData$Mystery <- as.factor(unlist(assocData$Mystery))
# 
# assocData$None <- as.factor(unlist(assocData$None))
# assocData$News <- as.factor(unlist(assocData$News))
# assocData$RealityTV <- as.factor(unlist(assocData$RealityTV))
# assocData$Romance <- as.factor(unlist(assocData$Romance))
# assocData$SciFi <- as.factor(unlist(assocData$SciFi))
# assocData$Sport <- as.factor(unlist(assocData$Sport))
# assocData$Short <- as.factor(unlist(assocData$Short))
# assocData$TalkShow <- as.factor(unlist(assocData$TalkShow))
# assocData$Thriller <- as.factor(unlist(assocData$Thriller))
# assocData$War <- as.factor(unlist(assocData$War))
# assocData$Western <- as.factor(unlist(assocData$Western))




#Build ruleset
movieRules <- apriori(assocDataSubset,parameter = list(support = 0.0001,confidence = 0.9));
# genreRules <- apriori(assocData,parameter = list(support = 0.01,confidence = 0.9));


#Removes redundant rules
uniqueMovieRules <- redundantRules(movieRules)
# uniqueGenreRules <- redundantRules(genreRules)

#Sorts trimmed rules by lift
sortedUniqueMovieRules <- sort(uniqueMovieRules, by = "lift")
# sortedUniqueGenreRules <- sort(uniqueGenreRules, by = "lift")
inspect(head(sortedUniqueMovieRules,20))




















