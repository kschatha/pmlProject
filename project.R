#Load the required packages
library(caret);library(randomForest)
library(rpart);library(rpart.plot)
library(rattle);library(RColorBrewer)

#set the seed to reproduce the same results
set.seed(12345)

#Loading the training and testing data into R
trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

training <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
testing <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))
dim(training);dim(testing)

#Size of the training data = 19622 X 160
#Size of the testing data = 20 X 160
#last column of training is the response variable called "classe"
#last column of testing is the "problem_id"

#Cleaning the data -find the near zero variance variables and removing them 
#as they do not contribute much to the model
nzv <- nearZeroVar(training)
#found 36 nzv variables so removing those
NZVtraining <- training[, -nzv]
dim(NZVtraining)
training <- NZVtraining

#Cleaning the data - removing the variables with large number of missing values
#variables with col sums > 0 have missing values.
Ntraining <- training[,colSums(is.na(training)) == 0]
#new dimensions
dim(Ntraining)
training <- Ntraining

#Clean the data - some other variables can be removed which seems not so relevant 
# to the model like - X, user_name, cvtd_timestamp
training <- training[,-c(1,2,5)]
dim(training)

#Size of the final training data - 19622 X 56

#Partition the training data into training and testing subsets for the purpose of
#cross-validation - 70% in training and 30% in testing.
inTrain <- createDataPartition( y=training$classe, p=0.7, list=FALSE)
trainSub <- training[inTrain,]
testSub <- training[-inTrain,]
dim(trainSub);dim(testSub)

#Fitting a model - We start with a single decision tree
dt_model <- rpart( classe ~ ., data=trainSub, method="class")
summary(dt_model)
#plot the decision tree
fancyRpartPlot( dt_model)

#Predicting on the cross-validation test set
dt_prediction <- predict( dt_model, testSub, type="class")
#confusion matrix
confusionMatrix( dt_prediction, testSub$classe)

#The out-of-sample error using Decision Tree is = 0.1862 and 
#the accuracy is = 0.8138. 

#We will use random forest now to see if the accuracy of the model can be improved.

#Fitting a model - We start with a single decision tree
rf_model <- randomForest( classe ~ ., data=trainSub)
summary(rf_model)

#Predicting on the cross-validation test set
rf_prediction <- predict( rf_model, testSub, type="class")
#confusion matrix
confusionMatrix( rf_prediction, testSub$classe)

#The out-of-sample error using Random Forest is =  and 
#the accurayc is = 0.9986

#Random Forest gives higher accuracy and better results as expected.

#Using the Random Forest model on the test set.
prediction <- predict( rf_model, testing, type="class")

#generating the files for the purpose of submission
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(prediction)









