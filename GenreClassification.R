##############################################################################################
#                                   Dynamically Load/Install Packages
##############################################################################################

#Dynamically load/install required packages
ready <- FALSE
loadPackages3 <- function() {
  if( require(R.utils) == FALSE) { install.packages("R.utils") }
  if( require(e1071) == FALSE) { install.packages("e1071") }
  if( require(kknn) == FALSE) { install.packages("klaR")}
  if( require(RWeka) == FALSE) { install.packages("RWeka") }
  if( require(oblique.tree) == FALSE) { install.packages("oblique.tree") }
  if (require(caret) == FALSE) { install.packages("caret") }
  ready <- TRUE
}
while(ready == FALSE) { ready <- loadPackages3() }

set.seed(7131)

##############################################################################################
#                                   IMDB DATA SET
##############################################################################################

#access the imdb data set we created
movieData <- readRDS("clean10Kdataset.rds")

# clean up the movie data set
# description, rating, imdbVotes, seriesID, season, type, awards, imdbRating, Poster, episode, imdbID, Metascore, response, and year
# were removed from table for analysis because they did not provide information helpful to classification

# check variables
names(movieData)

# remove unneeded variables
movieData <- movieData[,c(3,4,5,10,11,13,17,18,19,23)]

# confirm variable removal
names(movieData)

# now are datasets contain only the important variables pertinent to classification
# episodes taken about because all values are N/A for this dataset

#maybe need to use lapply we'll see
#Retail_data[] <- lapply(Retail_data, factor)

#country is going to be cut down to 2 items per list
movieData$Country <- lapply(movieData$Country,"[",1:2)

# Language is going to be cut down to 2 items as well
movieData$Language <- lapply(movieData$Language,"[",1:2)

# Cut genre down to 3 items
movieData$Genre <- lapply(movieData$Genre,"[",1:2)

# Cleans writer variables
movieData$Writer <- lapply(movieData$Writer, function(x) {
  gsub( " *\\(.*?\\) *", "", x)
  })

# Create unique rows for each genre
movieData <- unnest(movieData,Genre)

# Drop rows with NA genre
movieData <- movieData[!(is.na(as.factor(movieData$Genre))),]
movieData <- movieData[!movieData$Genre == "N/A",]
movieData$Genre <- as.factor(movieData$Genre)

# Create test and training sets
part <- createDataPartition(movieData$Genre,p=0.8,list=FALSE)
training <- movieData[part,]
test <- movieData[-part,]

# Save training/test data for later
saveRDS(training,file="training.rds")
saveRDS(test,file="test.rds")

############################ TO DO'S ##################################
#once testData is successfully created, need to go in and change -6 to appropriate value to create test data 
#that does not contain attribute genre to replace in the predict methods below where you see testDataMovie[,1:4]

movieDataWOutGenre <- movieData[,-6]
movieDataWOutGenre$Writer <- noParentheses


#RIPPER CLASSIFIER
ripperModelMovie <- JRip(Species~., data = trainingDataMovie)
ripperPredictionsMovie <- predict(ripperModelMovie, testDataMovie[,1:4])
# summarize results
ripCMMovie <- confusionMatrix(ripperPredictionsMovie, testDataMovie$Genre)


#C4.5 CLASSIFICATION
c45ModelMovie <- J48(Genre~., data = trainingDataMovie)
c45ModelPredictionsMovie <- predict(c45ModelMovie, testDataMovie[,1:4])
c45CMMovie <- confusionMatrix(c45ModelPredictionsMovie, testDataMovie$Genre)


#OBLIQUE CLASSIFICATION 
obliqueModelMovie <- oblique.tree(formula = Genre~., data = trainingDataMovie, oblique.splits = "only")
obliqueModelPredictionsMovie <- predict(obliqueModelMovie, testDataMovie)
obCMMovie <- confusionMatrix(colnames(obliqueModelPredictionsMovie)[max.col(obliqueModelPredictionsMovie)], testDataMovie$Genre)


#NAIVE BAYES CLASSIFIER
# train a naive bayes model
naiveBayesModel <- NaiveBayes(Genre~., data=trainingDataMovie)
# make predictions
#look at this
predictions <- predict(naiveBayesModel, testDataMovie[,1:4])
# summarize results
nbCMMovie <- confusionMatrix(predictions$class, testDataMovie$Genre)


#KNN CLASSIFIER
kkModel <- kknn(Genre~., test = testDataMovie, train = trainingDataMovie, distance = 1, kernel = "triangular")
knnPredictions <- predict(kkModel, testDataMovie[,1:4])
knnCMMovie <- confusionMatrix(knnPredictions, testDataMovie$Genre)



##############################################################################################
#                             FINAL DISPLAYED RESULTS
##############################################################################################
#...............................CONFUSION MATRICES FOR IRIS...................................
# RIPPER
print(ripCMIris)

#C4.5
print(c45CMIris)

#OBLIQUE
print(obCMIris)

#NAIVE BAYES
print(nbCMIris)

#KNN
print(knnCMIris)

#.....................CONFUSION MATRICES FOR LIFE EXPECTANCY...................................
#RIPPER
print(ripCMLExpect)

#C4.5
print(c45CMLExpect)

#OBLIQUE 
print(obCMLExpect)

#NAIVE BAYES
print(nbCMLExpect)

#KNN
print(knnCM)


#get iris accuracies
accRipIris <- ripCMIris$overall[1]
accC45Iris <- c45CMIris$overall[1]
accOBIris <- obCMIris$overall[1]
accNBIris <- nbCMIris$overall[1]
accKNNIris <- knnCMIris$overall[1]

#get life expectancy accuracies
accRipLE <- ripCMLExpect$overall[1]
accC45LE <- c45CMLExpect$overall[1]
accOBLE <- obCMLExpect$overall[1]
accNBLE <- nbCMLExpect$overall[1]
accKNNLE <- knnCM$overall[1]

accuracyMatrix <- matrix(c(accRipIris, accRipLE, accC45Iris, accC45LE, accOBIris, accOBLE, accNBIris, accNBLE, accKNNIris, accKNNLE), ncol = 2, byrow = TRUE)
colnames(accuracyMatrix) <- c("Iris Data Accuracies", "Life Expectancy Data Accuracies")
rownames(accuracyMatrix) <- c("Ripper", "C4.5", "Oblique Tree", "Naive Bayes", "KNN")
accuracyMatrix <- as.table(accuracyMatrix)
print(accuracyMatrix)
