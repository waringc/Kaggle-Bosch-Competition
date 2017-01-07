#########################
#Processes numerical features
#input: Raw CSV supplied by Bosch of part numerical values for train/test.
#output: Processed RDS format of train and test numerical data
#
#Loads numerical data.  Sets NA values to 0 and adds a +2 offset to ensure
#the model can differentiate between NAs and true measurements
#as suggested in: (https://www.kaggle.com/cartographic/bosch-production-line-performance/bish-bash-xgboost)


library(dplyr)
library(data.table)
###############
##Process Train
dt_num_train <- fread("~/Desktop/kaggle/Bosch/input/train_numeric.csv",
                showProgress = T)

##Get Id data and remove from main data table
Id_train<-dt_num_train$Id
Response<-dt_num_train$Response
dt_num_train[ , Id := NULL]
dt_num_train[ , Response := NULL]


##Convert columns set NA to 0 and add +2 offset to values
for(col in names(dt_num_train)) set(dt_num_train, j = col, value = dt_num_train[[col]] + 2)
for(col in names(dt_num_train)) set(dt_num_train, which(is.na(dt_num_train[[col]])), col, 0)

dt_num_train$Response<-Response
dt_num_train$Id<-Id_train

saveRDS(dt_num_train, file="processed_train_numeric-10182016.RDS")
rm(list=ls())
gc()

#################
##Repeat for test
dt_num_test <- fread("~/Desktop/kaggle/Bosch/input/test_numeric.csv",
                     showProgress = T)

Id_test<-dt_num_test$Id
dt_num_test[ , Id := NULL]

##Convert columns set NA to 0 and add +2 offset to values
for(col in names(dt_num_test)) set(dt_num_test, j = col, value = dt_num_test[[col]] + 2)
for(col in names(dt_num_test)) set(dt_num_test, which(is.na(dt_num_test[[col]])), col, 0)

dt_num_test$Id<-Id_test

saveRDS(dt_num_test, file="processed_test_numeric-10182016.RDS")