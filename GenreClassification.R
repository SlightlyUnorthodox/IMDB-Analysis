##############################################################################################
#                                   Dynamically Load/Install Packages
##############################################################################################
#Dynamically load/install required packages
ready <- FALSE
loadPackages3 <- function() {
  if( require(R.utils) == FALSE) { install.packages("R.utils") }
  if( require(tidyr) == FALSE) { install.packages("tidyr") }
  if( require(dplyr) == FALSE) { install.packages("dplyr") }
  if( require(e1071) == FALSE) { install.packages("e1071") }
  if( require(kknn) == FALSE) { install.packages("klaR")}
  if( require(RWeka) == FALSE) { install.packages("RWeka") }
  if( require(oblique.tree) == FALSE) { install.packages("oblique.tree") }
  if (require(caret) == FALSE) { install.packages("caret") }
  ready <- TRUE
}
while(ready == FALSE) { ready <- loadPackages3() }

set.seed(1911)

##############################################################################################
#                                   IMDB DATA SET
##############################################################################################

#access the imdb data set we created
movieData <- readRDS("clean10Kdataset.rds")

# clean up the movie data set
# description, rating, imdbVotes, seriesID, season, type, awards, imdbRating, Poster, episode, imdbID, Metascore, response, language, and year
# were removed from table for analysis because they did not provide information helpful to classification

# check variables
names(movieData)

# remove unneeded variables
movieData <- movieData[,-c(1,2,6,7,8, 9, 12, 14, 15, 16, 17, 20, 21, 23, 30 ,31)]

# confirm variable removal
names(movieData)

# now are datasets contain only the important variables pertinent to classification
# episodes taken about because all values are N/A for this dataset

#maybe need to use lapply we'll see
#Retail_data[] <- lapply(Retail_data, factor)

#country is going to be cut down to 1 country
movieData$Country <- lapply(movieData$Country,"[",1)

# Cut genre down to 2 items
movieData$Genre <- lapply(movieData$Genre,"[",1:2)

# Cleans writer entries
movieData$Writer <- lapply(movieData$Writer, function(x) {
  gsub( " *\\(.*?\\) *", "", x)
  })

tgemp <- movieData

# Reordering first and last name and removing exraneous separators
# First step is to remove single quote and double quotes
# yes, soempeople's name may have a single quote in it however numbers are so small
# it wont affect the data

###################################### fix producer ######################################

# single quote removal
movieData$Producer <- lapply(movieData$Producer, function(x){
  gsub("\'", "", x)#
})

# double quote removal
movieData$Producer <- lapply(movieData$Producer, function(x){
  gsub("\"", "", x)#
})

# remove brackets
movieData$Producer <- lapply(movieData$Producer, function(x){
  gsub("\\[", "", x)#
})

movieData$Producer <- lapply(movieData$Producer, function(x){
  gsub("\\]", "", x)#
})

movieData$Producer <- lapply(movieData$Producer, function(x){
strsplit(x,",")
})

firstName <- lapply(movieData$Producer, function(x){
 x[[1]][2]
})

lastName <- lapply(movieData$Producer, function(x){
  x[[1]][1]
})

movieData$Producer <- paste(firstName, lastName)

########################## end fixing of producer ########################

###################################### fix cinematographer ######################################

# single quote removal
movieData$Cinematographer <- lapply(movieData$Cinematographer, function(x){
  gsub("\'", "", x)#
})

# double quote removal
movieData$Cinematographer <- lapply(movieData$Cinematographer, function(x){
  gsub("\"", "", x)#
})

# remove brackets
movieData$Cinematographer <- lapply(movieData$Cinematographer, function(x){
  gsub("\\[", "", x)#
})

movieData$Cinematographer <- lapply(movieData$Cinematographer, function(x){
  gsub("\\]", "", x)#
})

movieData$Cinematographer <- lapply(movieData$Cinematographer, function(x){
  strsplit(x,",")
})

firstName <- lapply(movieData$Cinematographer, function(x){
  x[[1]][2]
})

lastName <- lapply(movieData$Cinematographer, function(x){
  x[[1]][1]
})

movieData$Cinematographer <- paste(firstName, lastName)

########################## end fixing of Cinematographer ########################

# Create unique rows for each genre
movieData <- unnest(movieData,Genre)
movieData <- unnest(movieData, Writer)
#movieData <- unnest(movieData, Actors)
movieData <- unnest(movieData, Country)

# Drop rows with NA genre because we can't classify these
movieData <- movieData[!(is.na(as.factor(movieData$Genre))),]
movieData <- movieData[!movieData$Genre == "N/A",]
movieData$Genre <- as.factor(movieData$Genre)

# Cast as factor
movieData$Rated <- as.factor(movieData$Rated)
movieData$Title <- as.factor(movieData$Title)
movieData$Director <- as.factor(movieData$Director)
movieData$Actor1 <- as.factor(movieData$Actor1)
movieData$Actor2 <- as.factor(movieData$Actor2)
movieData$Actor3 <- as.factor(movieData$Actor3)
movieData$Actor4 <- as.factor(movieData$Actor4)
movieData$Producer <- as.factor(movieData$Producer)
movieData$Cinematographer <- as.factor(movieData$Cinematographer)
movieData$Writer <- as.factor(movieData$Writer)
movieData$Country <- as.factor(movieData$Country)

# Create test and training sets
part <- createDataPartition(movieData$Genre,p=0.8,list=FALSE)
training <- movieData[part,]
test <- movieData[-part,]

# Save training/test data for later
saveRDS(training,file="training.rds")
saveRDS(test,file="test.rds")

### TEST AND TRAINING SETS ARE A GO

# For future use, make new data table without the genre label in it
movieDataWOutGenre <- test[,-13]

##################### TO DO
# try making them factors
#figure out if unnest is best way to go
#try differnet attribute comobinations to seewhats best

#RIPPER CLASSIFIER
ripperModelMovie <- JRip(Genre~., data = training)
ripperPredictionsMovie <- predict(ripperModelMovie, movieDataWOutGenre)
# summarize results
ripCMMovie <- confusionMatrix(ripperPredictionsMovie, test$Genre)

names(movieData)

#C4.5 CLASSIFICATION
c45ModelMovie <- J48(Genre~., data = training)
c45ModelPredictionsMovie <- predict(c45ModelMovie, movieDataWOutGenre)
c45CMMovie <- confusionMatrix(c45ModelPredictionsMovie, test$Genre)


#OBLIQUE CLASSIFICATION 
obliqueModelMovie <- oblique.tree(formula = Genre~., data = training, oblique.splits = "only")
obliqueModelPredictionsMovie <- predict(obliqueModelMovie, test)
obCMMovie <- confusionMatrix(colnames(obliqueModelPredictionsMovie)[max.col(obliqueModelPredictionsMovie)], test$Genre)


#NAIVE BAYES CLASSIFIER
# train a naive bayes model
naiveBayesModel <- NaiveBayes(Genre~., data=training)
# make predictions
#look at this
predictions <- predict(naiveBayesModel, movieDataWOutGenre)
# summarize results
nbCMMovie <- confusionMatrix(predictions$class, test$Genre)


#KNN CLASSIFIER
kkModel <- kknn(Genre~., test = test, train = training, distance = 1, kernel = "triangular")
knnPredictions <- predict(kkModel, movieDataWOutGenre)
knnCMMovie <- confusionMatrix(knnPredictions, test$Genre)



##############################################################################################
#                             FINAL DISPLAYED RESULTS
##############################################################################################
#...............................CONFUSION MATRICES FOR IRIS...................................
# RIPPER
print(ripCMMovie)

#C4.5
print(c45CMMovie)

#OBLIQUE
print(obCMMovie)

#NAIVE BAYES
print(nbCMMovie)

#KNN
print(knnCMMovie)


################## IGNORE BELOW FOR NOW

#get iris accuracies
accRipIris <- ripCMIris$overall[1]
accC45Iris <- c45CMIris$overall[1]
accOBIris <- obCMIris$overall[1]
accNBIris <- nbCMIris$overall[1]
accKNNIris <- knnCMIris$overall[1]


accuracyMatrix <- matrix(c(accRipIris, accRipLE, accC45Iris, accC45LE, accOBIris, accOBLE, accNBIris, accNBLE, accKNNIris, accKNNLE), ncol = 2, byrow = TRUE)
colnames(accuracyMatrix) <- c("Iris Data Accuracies", "Life Expectancy Data Accuracies")
rownames(accuracyMatrix) <- c("Ripper", "C4.5", "Oblique Tree", "Naive Bayes", "KNN")
accuracyMatrix <- as.table(accuracyMatrix)
print(accuracyMatrix)
