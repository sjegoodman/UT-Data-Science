# Load packages and libraries
library(readr)
install.packages("lattice")
install.packages("ggplot2")
library(caret)
install.packages("corrplot")
library(corrplot)
install.packages('randomForest') # For generating random forest model
library(randomForest)
install.packages("labeling")
install.packages("farver")

#Load existing product CSV
ExistingProds <-read_csv("existingproductattributes2017.csv")

summary(ExistingProds)

# dummify the data
newDataFrame <- dummyVars(" ~ .", data = ExistingProds)
readyData <- data.frame(predict(newDataFrame, newdata = ExistingProds))

summary(readyData)
#Check data types
str(readyData)

#Delete attribute with missing data
readyData$BestSellersRank <- NULL

# Find correlations
corrData <- cor(readyData, readyData$Volume)
corrData

#Correlation with a heat map
corrplot(corrData)

#Remove unneeded features
readyData$ProfitMargin <- NULL
readyData$ProductDepth <- NULL
readyData$ProductNum <- NULL
readyData$Price <- NULL
readyData$ProductWidth <- NULL
readyData$ProductHeight <- NULL




#Set seed
set.seed(123)
#Training and Testing partitions
inTraining <- createDataPartition(readyData$Volume, p = .75, list = FALSE)
training <- readyData[inTraining,]
testing <- readyData[-inTraining,]

# Train the model using the training sets and check score
linear <- lm(Volume ~ ., data = readyData)
summary(linear)

# This had a Rsquared value of 1, which tells me it is not a good model to use

#Moving on to SVM

# Set up Repeated k-fold Cross Validation
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
# Fit the model 
system.time(svm1 <- train(Volume ~., data = training, method = "svmLinear", trControl = train_control))
#View the model
svm1

# Testing SVM with test data
predictions <- predict(svm1, testing)
predictions
# This model creates negative predictions for sales volume, which is impossible
# So although it has a high R-squared of 0.95, it is not a model I can use

# Now on with Random Forest

system.time(rf1 <- train(Volume ~., data=training, method = "rf", trControl = train_control))
rf1
predictions <- predict(rf1, testing)
predictions
postResample(predictions, testing$Volume)

rfImp <- varImp(rf1)
rfImp

# OK RF had a predicted R-Squared of 0.97 and did not produce any negative values for sales volume
# Actual R-Squared when predicting the testing set was 0.86
# Time was 8 sec

# Next up is Gradient Boosting

system.time(gb1 <- train(Volume ~., data=training, method = "gbm", trControl = train_control, verbose = FALSE))
gb1
predictions <- predict(gb1, testing)
predictions
# With an estimated R-Squared value of 0.8
# Time 2.5 sec
# However, produced negative prediction values for sales volume, which is unusable

# I chose to go with Random Forest
# Prepare new product file to apply the model to it
NewProds <-read_csv("newproductattributes2017.csv")
summary(ExistingProds)

# dummify the data
newDataFrame2 <- dummyVars(" ~ .", data = NewProds)
newProdDF <- data.frame(predict(newDataFrame2, newdata = NewProds))

#Remove unneeded features
newProdDF$ProfitMargin <- NULL
newProdDF$ProductDepth <- NULL
newProdDF$ProductNum <- NULL
newProdDF$BestSellersRank <- NULL
newProdDF$Price <- NULL
newProdDF$ProductWidth <- NULL
newProdDF$ProductHeight <- NULL

# Make final predictions
finalPred <- predict(rf1, newProdDF)
finalPred

# Save predictions to a CSV file
output <- newProdDF
output$predictions <- finalPred
write.csv(output, file="C3T3output.csv", row.names = TRUE)

# Plot variable importance for chosen model
ggplot(rfImp)

