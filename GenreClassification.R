##############################################################################################
#                                     INSTALLS
##############################################################################################
#install.packages("e1071")
#install.packages("kknn")
#install.packages("klaR")
install.packages("caret")
#install.packages("RWeka")
#install.packages("oblique.tree")
#install.packages("base")


##############################################################################################
#                                    LIBRARY CALLS
##############################################################################################
library(klaR)
#Caret is the main library in which other libraries pull from for the method calls to classify the data sets
library(caret)
library(kknn)
library(oblique.tree)


##############################################################################################
#                                   IMDB DATA SET
##############################################################################################

#access the imdb data set we created
movieData <- readRDS("clean10Kdataset.rds")

# clean up the movie data set
# description, rating, imdbVotes, seriesID, season, type, awards, imdbRating, Poster, episode, imdbID, Metascore, response, and year
# were removed from table for analysis because they did not provide information helpful to classification

# removes description and rated variables
movieData <- movieData[3:24]

# removes the rest of the variables we dont care for
movieData <- movieData[,-4] 
movieData <- movieData[,-4]
movieData <- movieData[,-4]
movieData <- movieData[,-4]

movieData <- movieData[,-6]

movieData <- movieData[,-7]
movieData <- movieData[,-7]
movieData <- movieData[,-7]

movieData <- movieData[,-10]
movieData <- movieData[,-10]
movieData <- movieData[,-10]
movieData <- movieData[,-11]


# now are datasets contain only the important variables pertinent to classification
# episodes taken about because all values are N/A for this dataset

#maybe need to use lapply we'll see
#Retail_data[] <- lapply(Retail_data, factor)


noParentheses <- lapply(movieData$Writer, function(x) {
  gsub( " *\\(.*?\\) *", "", x)
  })

movieData$Writer <- noParentheses



createTestMovieSet <- function(dataset,classifier) {
  #set seed, said first digits of UF ID so i used the first 4
  set.seed(1911)
  
  #create partition space only want 20% for testing
  part <- createDataPartition(classifier, p=0.8, list=FALSE)
  test <-dataset[-part,]
  test
}


createTrainingMovieSet <- function(dataset,classifier) {
  #set seed, said first digits of UF ID so i used the first 4
  set.seed(1911)
  
  #create partition space only want 80% for testing
  part <- createDataPartition(classifier, p=0.8, list=FALSE)
  training <-dataset[part,]
  training
}

#setting variables for the train and test sets
testDataMovie = createTestMovieSet(movieData, movieData$Genre)
trainingDataMovie = createTrainingMovieSet(movieData, movieData$Genre)

############################ TO DOS ##################################
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
