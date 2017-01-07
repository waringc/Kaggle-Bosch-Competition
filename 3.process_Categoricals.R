#########################
#Finds duplicated categorical features and saves a list of non-duplicated
#input: Raw CSV supplied by Bosch of part categoricals for train/test.  And the
#unduplicated column names
#output: Processed RDS format of train and test categorical data
#
#Loads in non-duplicated features and encodes them into integers
#which is needed for XGBoost.



library(dplyr)
library(data.table)

#Load in non-duped categorical columns
dedupe_categoricals<-readRDS("deduped_cat_names-10182016.RDS")

dt_cat_train <- fread("~/Desktop/kaggle/Bosch/input/train_categorical.csv",
                      select = c('Id',dedupe_categoricals),
                      showProgress = T)

dt_cat_test <- fread("~/Desktop/kaggle/Bosch/input/test_categorical.csv",
                      select = c('Id',dedupe_categoricals),
                      showProgress = T)

##Get Id data and remove from main data table
Id_train<-dt_cat_train$Id
dt_cat_train[ , Id := NULL]

Id_test<-dt_cat_test$Id
dt_cat_test[ , Id := NULL]

row.train=nrow(dt_cat_train)#number of rows in train

#Merge train and test together
D.cat<-rbind(dt_cat_train,dt_cat_test)

#convert categories to numeric integers
for (f in names(D.cat)){
    levels <- unique(c(D.cat[[f]]))
    D.cat[[f]]<-as.numeric(factor(D.cat[[f]],levels = levels))
}

#remove any further duplicate rows
D.cat<-D.cat[ ,!duplicated(as.list(D.cat)), with = FALSE]

train_cat=D.cat[1:row.train,]
train_cat$Id<-Id_train
test_cat=D.cat[(row.train+1):nrow(D.cat),]
test_cat$Id<-Id_test

saveRDS(train_cat, file="processed_train_categorical-10182016.RDS")
saveRDS(test_cat, file="processed_test_categorical-10182016.RDS")