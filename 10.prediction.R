#########################
#Create the predictive model using XGBoost
#input: Processed date, numeric, categorical data and generated features, XGBoost Model
#output:  Submission CSV for kaggle
#
#Apply the model and threshold from model generation to the test data.

library(dplyr)
library(data.table)
library(xgboost)
library(Matrix)
library(caret)

cat(format(Sys.time(), "%X:"))
cat(" Loading Data\n")
load("Model-11092016.Rdata")


test_numeric<-as.data.table(readRDS("processed_test_numeric-10182016.RDS"))
Id<-test_numeric$Id
test_numeric[,Id:=NULL]

test_date<-as.data.table(readRDS("test_date_compressed-10182016.RDS"))
test_date[,Id:=NULL]


test_cat<-as.data.table(readRDS("processed_test_categorical-10182016.RDS"))
test_cat[,Id:=NULL]

test_feat<-as.data.table(readRDS("test_leak55-10222016.RDS"))
test_leak<-as.data.table(readRDS('test_leak-10192016.RDS'))
test_leak[,Id:=NULL]
test_feat[,Id:=NULL]

##Combine data
test<-cbind(test_numeric,test_date,test_leak, test_feat,test_cat)
rm(test_numeric,test_date,test_leak, test_feat,test_cat)
gc()
test<-test[,names(test) %in% feat_imp$Feature, with=FALSE]

X.test<-  Matrix(as.matrix(test), sparse = T)
rm(test)

dtest <- xgb.DMatrix(X.test)
pred <- predict(m1, dtest)
summary(pred)
best=0.9980 #threshold found in cross validation

sub   <- data.table(Id = Id,
                    Response = (pred > quantile(pred, best)) * 1)

write.csv(sub, "submission.csv", row.names = F)