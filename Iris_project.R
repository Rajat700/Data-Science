# Install and load caret package
install.packages("caret")
library(caret)
library(rpart)
library(rattle)
library(rpart.plot)
library(entropy)
# Set the working directory
# Import Iris Data Set
Df <- read.csv("Iris.csv",header=TRUE)
head(Df,10)
Df$Id <- NULL

## Check for missing values in the data set
is.na(Df)

# Count no. of each species present in the data set
Frequency_distribution <- table(Df$Species)
Frequency_distribution
prop.table(table(Df$Species))
# Make pair plots and calculate correlation to visualize data
pairs(iris[,0:4],col="red")
correlation<- cor(Df[,0:4])
correlation 

# Create train and test data using Caret Package

intrain <- createDataPartition(Df$Species,times=1,p=0.80,list = FALSE)
train_data <- Df[intrain,]
test_data <- Df[-intrain,]

## Check distribution of classes in Train and Test data
Frequency_distribution_train <- table(train_data$Species)
Frequency_distribution_train
prop.table(table(Df$Species))

Frequency_distribution_test <- table(test_data$Species)
Frequency_distribution_test
prop.table(table(test_data$Species))


## Make a decision Tree model
dtm <- rpart(Species~.,data = train_data,method = "class")
summary(dtm)
rpart.plot(dtm)

## Make Prediction using existing Tree model
Actual <- test_data$Species
test_data$Species <- NULL

View(test_data)
predictions <- predict(dtm,test_data,type = "class")

## Make confusion Matrix to Analyse Accuracy
cf <- confusionMatrix(predictions,Actual)
cf_matrix <- cf$table
cf_matrix
cf






