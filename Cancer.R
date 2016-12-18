#install.packages("caret")
library(caret)
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
#install.packages("rattle")
library(rattle)
#library(entropy)
#install.packages("RColorBrewer")
library(RColorBrewer)
library(RGtk2)
## Import Data set into R
data_frame <- read.csv("data.csv")
str(data_frame)
View(data_frame)
ncol(data_frame)
summary(data_frame)

## Remove unwanted columns
data_frame$X <- NULL
data_frame$id <- NULL
 is.na(data_frame)
## Check class distribution
table(data_frame$diagnosis)
prop.table(table(data_frame$diagnosis))

## Divide Data into train and test data
intrain <- createDataPartition(data_frame$diagnosis,times=1,p=0.75,list = FALSE)
training_data <- data_frame[intrain,]
test_data <- data_frame[-intrain,]
View(training_data)

## Check distribution of classes in both train and test data
train_distribution <-table(training_data$diagnosis)
train_distribution_percentage<-prop.table(table(training_data$diagnosis))

test_distribution <- table(test_data$diagnosis)
test_distribution

test_distribution_percentage <- prop.table(table(test_data$diagnosis))
test_distribution_percentage

#################################################################################
## Data successfully imported and splitted into train and test data set
##############################################################################


## Step 2  Data Visualization

## Create a correlation matrix in R
cor_matrix <- cor(training_data[,-1],y=NULL)
hc <- findCorrelation(cor_matrix,cutoff = 0.85)
hc <- sort(hc)
View(hc)
reduced_data <- training_data[,-c(hc)]
View(reduced_data)
str(reduced_data)
ncol(reduced_data)

df <- training_data$diagnosis
final_train_data <-cbind(df,reduced_data)
View(final_train_data)
names(final_train_data)[1] <- paste("diagnosis")
names(final_train_data)

 #cor_matrix_test <- cor(test_data[,-1],y=NULL)
 #hc_test <- findCorrelation(cor_matrix_test,cutoff=0.85)
 #hc_test <- sort(hc_test)
 #reduced_data_test <- test_data[,-c(hc_test)]

## Convert Target variable to factors
final_train_data$diagnosis <- as.factor(final_train_data$diagnosis)
str(final_train_data)

Actual <- test_data$diagnosis
test_data$diagnosis <- NULL

## Make a support vector machine model to do classification
install.packages("e1071")
library(e1071)
svm_model <- svm(diagnosis~.,data=final_train_data)
summary(svm_model)
predictions <- predict(svm_model,test_data)
## Plot confusion matrix to measure Accuracy
cf <- confusionMatrix(predictions,Actual)
cf_matrix <- cf$table
cf_matrix
cf


