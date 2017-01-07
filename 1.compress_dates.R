#########################
#This script compresses the data information for the parts
#input: Raw CSV supplied by Bosch of part dates for test and train
#output: Processed RDS format of part dates for test train
#
#Bosch has supplied dates for every numerical and categorical feature of each part
#however for the dates are identical for every features within the same production
#station.  The exception is stations 24 and 25 which have an entry and exit date.
#This script collapsed the date features down to a single features for each production
#line station except 24 and 25 which have an entry and exit feature.

library(dplyr)
library(data.table)

#Function to compress dates features for a given data table
compress_date<-function(dt_date){
  Id<-dt_date$Id
  dt_date[ , Id := NULL]
  
  station_names<-names(dt_date) #names of columns
  
  ##Create new data frame to store the compressed date data
  compressed_date<-Id
  compressed_date<-as.data.frame(compressed_date)
  station_names_new<-c("Id")
  names(compressed_date)<-c("Id")
  names(compressed_date)=station_names_new
  
  #for each station from 0 to 51
  for (i in c(seq(0,51))){
    #Find ntry of each part in each station
    #is always the same for all stations but 24 and 25 (ie entry and exit it the same)
    cat(i)
    tempstation<-paste0("S",as.character(i),"_")
    min<-as.data.frame(apply((dt_date[,grepl(x=station_names,pattern=paste0("_",tempstation)),with=FALSE]),1,min, na.rm=TRUE))
    station_names_new<-c(station_names_new, paste0(tempstation,"entry"))
    compressed_date<-cbind(compressed_date,min)
    
    #Find max exit of each part in station 24 or 25
    #is always the same for all stations but 24 and 25 (ie entry and exit it the same)
    if (i == 24 | i == 25){
      max<-as.data.frame(apply((dt_date[,grepl(x=station_names,pattern=paste0("_",tempstation)),with=FALSE]),1,max, na.rm=TRUE))
      compressed_date<-cbind(compressed_date,max)
      station_names_new<-c(station_names_new, paste0(tempstation,"exit"))
    }
  }
  
  names(compressed_date)=station_names_new
  compressed_date[compressed_date==Inf]=NA
  compressed_date[compressed_date==-Inf]=NA
  compressed_date
}

#Load train data data
dt_date <- fread("~/Desktop/kaggle/Bosch/input/train_date.csv",
                 showProgress = T)


##compress training data
train_date_compressed<- compress_date(dt_date)

#set NAs equal to zero
for(col in names(train_date_compressed)) set(train_date_compressed, which(is.na(train_date_compressed[[col]])), col, 0)

##save training dates to RDS (faster loading)
saveRDS(train_date_compressed, file="train_date_compressed-10182016.RDS")



###################################################
##load and perform same updated on test data
rm(list=c(dt_date,train_date_compressed))
#Load train data data
dt_date <- fread("~/Desktop/kaggle/Bosch/input/test_date.csv",
                 showProgress = T)

##compress training data
test_date_compressed<- compress_date(dt_date)

#set NAs equal to zero
for(col in names(test_date_compressed)) set(test_date_compressed, which(is.na(test_date_compressed[[col]])), col, 0)

##save training dates to RDS (faster loading)
saveRDS(test_date_compressed, file="test_date_compressed-10182016.RDS")
