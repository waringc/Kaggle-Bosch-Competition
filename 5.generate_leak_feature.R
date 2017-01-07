#########################
#Generates the "magic" or possible leak features discovered during the competition
#Refer to: https://www.kaggle.com/mmueller/bosch-production-line-performance/road-2-0-4
#input: Processed date and numeric data for train/test
#output: "magic" features saved as an RDS
#
#The train and test data where randomly sampled from the production line
#over a common period of time.  One part may be in train data and the next sequential
#part to enter the line may be in test data.  Failures appeared to be clustered together
#by start date of the part.  Therefore if a part in the test data was sequentially near
#a failure in the training data it is at a higher risk of failure.  These features
#capture this observation.
#(https://www.kaggle.com/cartographic/bosch-production-line-performance/bish-bash-xgboost)



library(dplyr)
library(data.table)

################################
##load date data and remove ID's
test_date<-as.data.table(readRDS("test_date_compressed-10182016.RDS"))
train_date<-as.data.table(readRDS("train_date_compressed-10182016.RDS"))
test_date[ , Id := NULL]
train_date[ , Id := NULL]

################################
##load numeric data and only need response and ID
keep=c("Id", "Response")
train_numeric<-as.data.table(readRDS("processed_train_numeric-10182016.RDS"))
train_numeric<-train_numeric[, keep, with = FALSE]
Response<-train_numeric[,Response]
train_numeric[,Response:=NULL]

Id_train<- train_numeric[,Id]

keep=c("Id")
test_numeric<-as.data.table(readRDS("processed_test_numeric-10182016.RDS"))
test_numeric<-test_numeric[, keep, with = FALSE]
Id_test<- test_numeric[,Id]

##################################
##Get Start and stop times of each part
train_numeric[, Start:=apply(train_date,1,min, na.rm=T)]
test_numeric[, Start:=apply(test_date,1,min, na.rm=T)]

####################
#Compute first two magic features
train_numeric[,magic_1:=c(diff(train_numeric$Id,lag=1),0)]
test_numeric[,magic_1:=c(diff(test_numeric$Id,lag=1),0)]

train_numeric[,magic_2:=c(0,rev(diff(rev(train_numeric$Id),lag=1)))]
test_numeric[,magic_2:=c(0,rev(diff(rev(test_numeric$Id),lag=1)))]

####################
#Compute second two magic features

##Combine data and sort by start time
combined_numeric<-rbind(train_numeric,test_numeric)
combined_numeric<-combined_numeric[order(Start,Id),]

combined_numeric[,magic_3:=c(diff(combined_numeric$Id,lag=1),0)]
combined_numeric[,magic_4:=c(0,rev(diff(rev(combined_numeric$Id),lag=1)))]

#reorder by Id
combined_numeric<-combined_numeric[order(Id),]

####################
#Seperate data
train_leak<-combined_numeric[combined_numeric$Id %in% Id_train,]
test_leak<-combined_numeric[combined_numeric$Id %in% Id_test,]
train_leak[,Start:=NULL]
test_leak[,Start:=NULL]

saveRDS(train_leak, file="train_leak-10192016.RDS")
saveRDS(test_leak, file="test_leak-10192016.RDS")
