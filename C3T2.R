#install packages - probably need to multiple times
install.packages("caret", dependencies = TRUE)
install.packages("lattice")
install.packages("ggplot2")
install.packages("C50", dependencies = TRUE)
install.packages("inum", dependencies = TRUE)

#load libraries and set seed
library(caret)
library(C50)
set.seed(123)
library(readr)

#Read the CSV files
CompleteResponses <- read_csv("SurveyData/CompleteResponses.csv")
SurveyIncomplete <- read_csv("SurveyData/SurveyIncomplete.csv")
attributes(CompleteResponses)
attributes(SurveyIncomplete)

#histogram of responses to check them out
hist(CompleteResponses$age)
hist(SurveyIncomplete$age)
hist(CompleteResponses$brand)
summary(CompleteResponses)
summary(SurveyIncomplete)

#Change columns to factors - car, zipcode, brand are all categories
CompleteResponses$car<-as.factor(CompleteResponses$car)
SurveyIncomplete$car<-as.factor(SurveyIncomplete$car)
CompleteResponses$zipcode<-as.factor(CompleteResponses$zipcode)
SurveyIncomplete$zipcode<-as.factor(SurveyIncomplete$zipcode)
CompleteResponses$brand<-as.factor(CompleteResponses$brand)
SurveyIncomplete$brand<-as.factor(SurveyIncomplete$brand)

#Change education level to ordinal
CompleteResponses$elevel<-as.ordered(CompleteResponses$elevel)
SurveyIncomplete$elevel<-as.ordered(SurveyIncomplete$elevel)

#Change brand column to NA for SurveyIncomplete
#This later will disable postResample, but it's garbage data anyway
#SurveyIncomplete$brand<-NA

#Create the partitions for testing and training
inTraining <- createDataPartition(CompleteResponses$brand, p = .75, list = FALSE)
training <- CompleteResponses[inTraining,]
testing <- CompleteResponses[-inTraining,]

#install.packages("gbm")

#trainControl setting 10-fold CV for use later
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated one time
  repeats = 1)

#Stochastic Gradient Boosting 
gbmFit1 <- train(brand ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE)
gbmFit1

#Find variable importance using VarImp - this didn't work
#library(varImp)
#gbmImp <- varImp(gbmFit1, scale = FALSE)
#gbmImp

#dataframe for manual tuning of mtry
rfGrid <- expand.grid(mtry=c(1,2,3,4,5))

#train Random Forest Regression model
system.time(rf1 <- train(brand ~ ., data = training, 
                 method = "rf", 
                 trControl = fitControl,
             tuneGrid=rfGrid))
#Random Forest results
rf1

#train Random Forest Regression model - without specifying mtry
rf2 <- train(brand ~ ., data = training, 
             method = "rf", 
             trControl = fitControl)
rf2

rfImp <- caret::varImp(rf1)
rfImp

#C5.0 classifier tested
c50 <- train(brand ~ ., data = training,
                 method = "C5.0", 
                 trControl = fitControl,)
c50
#Accuracy of 0.92268 and Kappa of 0.8358

#Variable Importance for C50 model
c50Imp <-varImp(c50)
c50Imp

#Find predicitons based on best performing model
predictions <- predict(c50, SurveyIncomplete)
predictions
postResample(predictions, SurveyIncomplete$brand)

#Summary of brand predicitons based on best model
summary(predictions)
#Summary of original reponses 
summary(CompleteResponses$brand)
