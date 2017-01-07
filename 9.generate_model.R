#########################
#Create the predictive model using XGBoost
#input: Processed date, numeric, categorical data and generated features
#output:  XGBoost model for prediction
#
#The model is trained on 90% of data and cross validated on the remaining 10%
#Especially important for the cross validation is determing the probability thereshold
#for parts to be predicted a failure.
#
#Parts of coding building XGBoost model based off of:
#https://www.kaggle.com/cartographic/bosch-production-line-performance/bish-bash-xgboost

library(dplyr)
library(data.table)
library(xgboost)
library(Matrix)
library(caret)

cat(format(Sys.time(), "%X:"))
cat(" Loading Data\n")

train_numeric<-as.data.table(readRDS("processed_train_numeric-10182016.RDS"))
Y<-train_numeric[,Response]
train_numeric[,Response:=NULL]
train_numeric[,Id:=NULL]

train_date<-as.data.table(readRDS("train_date_compressed-10182016.RDS"))
train_date[,Id:=NULL]


train_cat<-as.data.table(readRDS("processed_train_categorical-10182016.RDS"))
train_cat[,Id:=NULL]

#train_feat2<-as.data.table(readRDS("train_leak51-10202016.RDS"))
train_feat<-as.data.table(readRDS("train_leak55-10222016.RDS"))
train_leak<-as.data.table(readRDS('train_leak-10192016.RDS'))
train_leak[,Id:=NULL]
train_feat[,Id:=NULL]

##Combine data
train<-cbind(train_numeric,train_date,train_leak, train_feat,train_cat)
# feat_imp<-readRDS("feature_importance-10232016-65rds.RDS")
#load("Model-3456Seed50RoundNewFeatDiffParam-11092016.Rdata")
#train<-train[,names(train) %in% mod_imp$Feature, with=FALSE]
# train[,magic_start:=NULL]
# train[,magic_sum:=NULL]

X.train<-  Matrix(as.matrix(train), sparse = T)

##Fold data
set.seed(7534)
folds <- createFolds(as.factor(Y), k = 5)
valid <- folds$Fold3
model <- c(1:length(Y))[-valid]

dmodel <- xgb.DMatrix(X.train[model,], label = Y[model])
dvalid <- xgb.DMatrix(X.train[valid,], label = Y[valid])
gc(verbose=FALSE)

cat(format(Sys.time(), "%X:"))
cat(" Building model\n")

#Parameters
set.seed(1066)
nrounds<-10000
kfolds<-10

params <- list(objective = "binary:logistic",
                  eval_metric = "auc",
                  eta = 0.005, 
                  max_depth = 12,
                  colsample_bytree=1.0,
                  sub_sample=1.0,
                  min_child_weight = 1.7984,
                  base_score = 0.005)

m1 <- xgb.train(data = dmodel, param, nrounds = 1200,
                watchlist = list(val=dvalid, mod = dmodel), early.stop.round=15)

pred <- predict(m1, dvalid)

mod_imp<-xgb.importance(feature_names = names(train),model= m1)
 
num_imp<-mod_imp[mod_imp$Feature %in% names(train_numeric),]
date_imp<-mod_imp[mod_imp$Feature %in% names(train_date),]
cat_imp<-mod_imp[mod_imp$Feature %in% names(train_cat),]



#####################################
##Matthews Coefficient to determine threshold
##Matthew Correlation Coefficient code from:
##https://www.kaggle.com/cartographic/bosch-production-line-performance/bish-bash-xgboost


mc <- function(actual, predicted) {

  tp <- as.numeric(sum(actual == 1 & predicted == 1))
  tn <- as.numeric(sum(actual == 0 & predicted == 0))
  fp <- as.numeric(sum(actual == 0 & predicted == 1))
  fn <- as.numeric(sum(actual == 1 & predicted == 0))

  numer <- (tp * tn) - (fp * fn)
  denom <- ((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)) ^ 0.5

  numer / denom
}

matt <- data.table(thresh = seq(0.990, 0.999, by = 0.0001))

matt$scores <- sapply(matt$thresh, FUN =
                        function(x) mc(Y[valid], (pred > quantile(pred, x)) * 1))

print(matt)


best <- matt$thresh[which(matt$scores == max(matt$scores))]

#best = 0.9980
save(m1,mod_imp,best,file="Model-11092016.Rdata")
