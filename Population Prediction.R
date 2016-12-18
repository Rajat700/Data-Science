# Data cleaning step
# Since data was not very large we  cleaned the data in excel only by using basic excel functions
# After cleaning, data was split in the ratio of 75:25 as training and test data respectively 

# Get working directory by using getwd()
 # getwd()

# If you want to change the working directory use setwd() command
#setwd("C:/Users/583171/Documents/mattel project")
# choose path according to your working directory
 
#Install XLConnect package in order to import data
#Make sure that Internet connectivity is enabled while executing the below command
#install.packages(XLConnect)

# LOad XL connect library to import data
library(XLConnect)

# LOad the training data excel workbook
wb <-loadWorkbook("training_data_new.xlsx")

# Extract data from each sheet and put that in the list
lst <- readWorksheet(wb,sheet=getSheets(wb))

# Combine data frames from list in single data frame and call that data frame train_frame
train_frame<- do.call(rbind,lst)

# Export train_frame to excel csv
write.csv(train_frame,"train_frame.csv")

# Training data successfully imported


# import test data file to test model
wb_test<-loadWorkbook("test_data_new.xlsx")

# Extract data from each sheet of test file and put that into a list
lst_test<-readWorksheet(wb_test,sheet=getSheets(wb_test))

# Combine data frame from lst_test list to make consolidated data frame and assign it in a variable test_frame
test_frame<-do.call(rbind,lst_test)

# Export test_frame to excel csv
#write.csv(test_frame,"test_frame.csv")

# Training and Test data successfully imported

# Step 2 Data Visualization

# Visualize population data using various plots in order to get an idea which algorithm should be used to make the model
# After looking at various plots we will use the best possible algorithm to make a predictive model




# Making linear model for prediction for urban males,urban females, rural males and rural females
# Convert City name into factors
train_frame$City.Nameafact <- as.factor(train_frame$City.Name)
test_frame$City.Nameafact <- as.factor(test_frame$City.Name)
# Convert Age group into factors
train_frame$Age.group <- as.factor(train_frame$Age.group)
test_frame$Age.group <- as.factor(test_frame$Age.group)

# Convert City code into factors
train_frame$City.code <- as.factor(train_frame$City.code)
test_frame$City.code <- as.factor(test_frame$City.code)

# 1 Linear model for urban males
model_urban_males <- lm(Total_U_males ~City.code+Age.group+Year,data=train_frame)

# predict urban males  population from 2010 to 2014 using linear model
predict_urban_males <- predict.lm(model_urban_males,test_frame)

# Insert predicted urban males population into test data frame for comparison
test_frame$predict_urban_males <- predict_urban_males

# 2 Linear model for urban females
model_urban_females <- lm(Total_U_females ~City.code+Age.group+Year,data=train_frame)

# Predict urban females population from 2010 to 2014 using linear model

predict_urban_females <- predict(model_urban_females,test_frame)

# Insert predicted urban females population into test data frame for comparison
test_frame$predict_urban_females <-predict_urban_females

# 3 Linear model for Rural males
model_rural_males <- lm(Total_R_males ~City.code+Age.group+Year,data=train_frame)

# predict rural males population from 2010 to 2014
predict_rural_males <- predict(model_rural_males,test_frame)

# Insert predicted rural males population in test_frame for comparison
test_frame$predict_rural_males <-predict_rural_males

# 4 Linear model for rural females 
model_rural_females <- lm(Total_R_females~City.code+Age.group+Year,data=train_frame)

# predict rural females population from 2010 to 2014
predict_rural_females <-predict(model_rural_females,test_frame)

# Insert predicted rural females population into test_frame for comparison
test_frame$rural_females <- predict_rural_females

# convert age groups from factors into  strings
test_frame$Age.group <- as.character(test_frame$Age.group)

# Make a new data frame which contains urban males,predicted urban males, urban females and predicted urban females
Data_frame_urban <- data.frame("City name"=test_frame$City.Name,"Year"=test_frame$Year,"Age group"=test_frame$Age.group,"Total urban males"=test_frame$Total_U_males,"Total predicted urban males"=test_frame$predict_urban_males,"Total urban females"=test_frame$Total_U_females,"predicted urban females"=test_frame$predict_urban_females)

# Export Data_frame_urban to a csv file
write.csv(Data_frame_urban,"Data_frame_urban_fact_updated.csv")

# Make a new data frame which contains rural males and rural females
Data_frame_rural <- data.frame("City name"=test_frame$City.Name,"Year"=test_frame$Year,"Age group"=test_frame$Age.group,"Total rural males"=test_frame$Total_R_males,"Total predicted rural males"=test_frame$predict_rural_males,"Total rural females"=test_frame$Total_R_females,"predicted rural females"=test_frame$rural_females)


# Export Data_frame_rural to a csv file
write.csv(Data_frame_rural,"Data_frame_rural_fact_updated.csv")
