#########################
#Finds duplicated categorical features and saves a list of non-duplicated
#input: Raw CSV supplied by Bosch of part categoricals train
#output: Processed RDS format of non duplicate column names
#
#Saves a list of which columns are not duplicates.  Useful since it avoids having to
#reload the entire large CSV in the future 

library(dplyr)
library(data.table)

##Load training categorical data
dt_cat_train <- fread("~/Desktop/kaggle/Bosch/input/train_categorical.csv",
                showProgress = T)


##Get Id data and remove from main data table
dt_cat_train[ , Id := NULL]


#Remove duplicated columns (aren't needed)
cat_names<-names(dt_cat_train)
dedupe_cat_names<-cat_names[!duplicated(as.list(dt_cat_train))]

#Save list of deduped names
saveRDS(dedupe_cat_names, file="deduped_cat_names-10182016.RDS")