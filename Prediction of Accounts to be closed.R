
                         ## Install and load required packages ##

install.packages('rpart')
install.packages('rpart.plot')
install.packages('RColorBrewer')
install.packages('XLConnect')
install.packages('ggplot2')

# install.packages('e1071')
# install.packages('rattle')

library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(XLConnect)
library(ggplot2)
# library(rattle)
# library(e1071)

                        ## Loading data from excel sheet ##

wb = loadWorkbook("Main_Data.xlsx")
Demo_data = readWorksheet(wb, sheet = "Data", header = TRUE)

#Final_df <- Demo_data[,-c(1,3,4,5,7,9)]

Final_df <- Demo_data[,-c(3,4,7)]
Final_cat_df <- data.frame(Final_df$ACCOUNT.KEY,
                          Final_df$Duration.in.years, 
                           Final_df$DURATION.SINCE.LAST.FRDs,
                           Final_df$NO.OF.transactions.in.last.frd,
                           Final_df$Duration.Since.Last.DIS,
                           Final_df$No..of.transactions.in.last.dis,
                           as.factor(Final_df$RISK.TOLERANCE.CATEGORY),
                           as.factor(Final_df$INVESTMENT.HORIZON.CATEGORY),
                           as.factor(Final_df$INVESTMENT.OBJECTIVE.CATEGORY),
                           as.factor(Final_df$ACCOUNT_STATUS))
# str(Final_cat_df)

                   ## divide data into training and testing (70%,30%) ##

Total_data <- sample(2,nrow(Final_cat_df),replace=TRUE,prob=c(0.7,0.3))
traindata <- Final_cat_df[Total_data==1,]
testdata <- Final_cat_df[Total_data==2,]
#write.csv(traindata,"traindata.csv")
#write.csv(testdata,"testdata.csv")

#Categorical Data for Traning set

#TR_RTC<- as.factor(traindata$RISK.TOLERANCE.CATEGORY)
#TR_IOC<-as.factor(traindata$INVESTMENT.OBJECTIVE.CATEGORY)
#TR_RFCC<-as.factor(traindata$REASON.FOR.CLOSING.CATEGORY)
#TR_IHC<-as.factor(traindata$INVESTMENT.HORIZON.CATEGORY)

# Categorical Data for Test set

#TE_RTC<- as.factor(testdata$RISK.TOLERANCE.CATEGORY)
#TE_IOC<-as.factor(testdata$INVESTMENT.OBJECTIVE.CATEGORY)
#TE_RFCC<-as.factor(testdata$REASON.FOR.CLOSING.CATEGORY)
# TE_IHC<-as.factor(testdata$INVESTMENT.HORIZON.CATEGORY)




                      ## Building Decision Tree Model ##

DTREE_MODEL<-rpart(as.factor.Final_df.ACCOUNT_STATUS. ~ .,
                   data = traindata[,-1],
                   method = "class",control = rpart.control(minsplit=5, cp=0.001))
summary(DTREE_MODEL)

                            ## Ploting decision tree
# rpart.plot(DTREE_MODEL)
# plot(DTREE_MODEL)

fancyRpartPlot(DTREE_MODEL)

                           ## Testing model and Confusion matrix ##

DTREE_PRED <- predict(DTREE_MODEL,testdata[,-c(1,10)],type = "class")

confusion <- table(pred = DTREE_PRED,testdata$as.factor.Final_df.ACCOUNT_STATUS.) ## Confusion matrix
confusion

                          ## Predicted output as a csv file ##

predicted_output <- data.frame(AccountKey=testdata$Final_df.Account.Key, 
                               Actual_status <- testdata$as.factor.Final_df.open.close., 
                               as.factor.Final_df.open.close. = DTREE_PRED)
write.csv(predicted_output, file = "Predicted_Output3.csv", row.names = FALSE)


