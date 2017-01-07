#########################
#Uses bayesian optimization to find best parameters for XGBoost model
#input: Processed date,numeric and categorical data and all generated features
#output:  Best set of parameters for XGBoost
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

train_date<-as.data.table(readRDS("train_date_compressedNA-1020201.RDS"))
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

#Create matricies
X.train<-  Matrix(as.matrix(train), sparse = T)


set.seed(8876)

dmodel <- xgb.DMatrix(X.train, label = Y)

rm(X.train, train)

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




cat(format(Sys.time(), "%X:"))
cat(" Performing optimization\n")

##########
#Perform Bayesian optimization

##Fold Data
cv_folds <- KFold(getinfo(dmodel, 'label'), nfolds = 5, stratified = TRUE, seed = 0)


###Function to optimize
xgb_cv_bayes <- function(max.depth, subsample, colsample_bytree, eta, min_child_weight) {
  
  
  params= list(max.depth = max.depth, 
               subsample = subsample, 
               colsample_bytree = colsample_bytree,
                booster = "gbtree", 
               eta = eta,
               min_child_weight=min_child_weight,
               #gamma=gamma,
               eval_metric = "auc",
               objective = "binary:logistic",
               base_score = 0.005)
          
  
  cv <- xgb.cv(params,
               data = dmodel, 
               nround = 200,
               folds = cv_folds, prediction = TRUE, showsd = TRUE,
               early.stop.round = 3, maximize = TRUE, verbose = TRUE)
  
  # #Find best MCC Score
  matt <- data.table(thresh = seq(0.990, 0.999, by = 0.0001))

  matt$scores <- sapply(matt$thresh, FUN =
                          function(x) mc(Y, (cv$pred > quantile(cv$pred, x)) * 1))
  
  #Return results for optimization
  #list(Score = cv$dt[, max(test.auc.mean)], Pred = cv$pred)
  list(Score = max(matt$scores,na.rm = TRUE), Pred = cv$pred)

}


opt_result <- BayesianOptimization(xgb_cv_bayes,
                                   bounds = list(max.depth = c(5L, 12L),
                                                  subsample = c(0.625, 1.0),
                                                  colsample_bytree = c(0.625, 1.0),
                                                  eta = c(0.005, 0.5),
                                                  min_child_weight=c(0.5, 3.0)),
                                                 init_points = 10, 
                                                 n_iter = 25, 
                                                 verbose = TRUE)


save(opt_result,file="bayop_200feat_10262016.Rdata")