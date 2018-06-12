# Carlos de Paz (2018), Practical Machine Learning (Course Project)

rm(list=ls())
library(caret)

#----------------------------------------------------------Download, read and prepare the data sets--------------------------------------------------------

data_training<- read.csv("pml-training.csv", sep=",", header=TRUE, na.strings = c("NA","#DIV/0!","")) #160 variables
data_testing<- read.csv("pml-testing.csv", sep=",", header=TRUE, na.strings = c("NA","#DIV/0!","")) #160 variables

#Clean NA values
data_training <- data_training[,(colSums(is.na(data_training)) == 0)] #60 variables
data_testing <- data_testing[,(colSums(is.na(data_testing)) == 0)] #60 variables

#Only numerical variables
data_training<-data_training[8:60] #53 variables
data_testing<-data_testing[8:60] #53 variables


#--------------------------------------------------------------  Training & Validation Subset -----------------------------------------------------------
set.seed(333)
inTrain<-createDataPartition(data_training$classe, p=.7, list=F)
training<-data_training[inTrain,]
val<-data_training[-inTrain,]

#------------------------------------------------------------------- Train the Models -----------------------------------------------------------------

#Model 1: Random Forest
modRF = train(classe ~ ., data=training, preProcess=c("center","scale"), method = "rf", ntree = 200, trControl=trainControl(method='oob'))
modRF$results$Accuracy[1] #0.9925

#Model 2: KNN
modKnn = train(classe ~ ., data=training, method = "knn", trainControl(method = "adaptive_cv"))
modKnn$results$Accuracy[1] #0.8946

#Model 3: Decision Tree
modRpart <- train(classe ~ ., data=training, method="rpart")
modRpart$results$Accuracy[1] #0.5145

#Random Forest > KNN > Decision Tree

#-------------------------------------------------Predictions, Confusion Matrix, Accuracy and Out of Sample Errors --------------------------------------

pred_val<-predict(modRF,val)
confM<-confusionMatrix(val$classe, pred_val)
confM$overall[1] #0.9940

out_of_sample_error <- 1-confM$overall[1] #0.0059 

#------------------------------------------------------------- Apply the model on the test data ---------------------------------------------------------
testing_predicts<-predict(modRF, data_testing)
testing_predicts
