#setwd("C:/Users/mahfuzul.haque/Google Drive/KB/Data Science/Code/Practical-Machine-Learning-Project")

source("Submit.R")
library(caret)
library(randomForest)

trainingPath = "pml-training.csv" 
trainingData <- read.csv(trainingPath, na.strings = c("NA", "#DIV/0!", ""), stringsAsFactors=FALSE)

testingPath = "pml-testing.csv" 
testingData <- read.csv(testingPath, na.strings = c("NA", "#DIV/0!", ""), stringsAsFactors=FALSE)

# head(trainingData)
# names(trainingData)
# dim(trainingData) # 19622   160
# trainingData[,160]

features <- names(trainingData)
classes <- trainingData[,c(160)]

selectedFeatures <- c()

for (i in 1:length(features) ) 
  if ( sum(is.na(trainingData[,c(features[i])])) == 0 ) 
    selectedFeatures[length(selectedFeatures) + 1] = features[i]   

selectedFeatures <- selectedFeatures[-c(1,2,3,4,5,6,7)] 

refinedTrainingData <- trainingData[, c(selectedFeatures)]

refinedTrainingData$classe <- as.factor(refinedTrainingData$classe)

summary(refinedTrainingData)

set.seed(3433)
inTrain = createDataPartition(refinedTrainingData$classe, p = 3/4)[[1]]

training = refinedTrainingData[ inTrain,]
testing = refinedTrainingData[-inTrain,]

dim(training)
dim(testing)

set.seed(62433)

model_rf <- randomForest(classe ~ ., data = training)
pred <- predict(model_rf,newdata = testing)

accuracy_rf = sum(pred == testing$classe) / length(pred)

refinedTestingData <- testingData[, c(selectedFeatures)]

pred2 <- predict(model_rf,newdata = refinedTestingData)

res <- as.character(pred2)
print(res) # "B" "A" "B" "A" "A" "E" "D" "B" "A" "A" "B" "C" "B" "A" "E" "E" "A" "B" "B" "B"

pml_write_files(res)
