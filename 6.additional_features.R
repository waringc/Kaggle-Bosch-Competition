#########################
#Generates further features for the data
#input: Processed date and numeric data for train/test
#output:  features saved as an RDS
#
#Features added include:
#-the length of time the part spent in  total in production
#-the length of time the part spent in each of the 3 lines production was divided into
#-The rate of part failures was not constant over time.  A feature was added to measure the
#the failure rate for the period the part entered the production line.
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

train_numeric[, Stop:=apply(train_date,1,max, na.rm=T)]
test_numeric[, Stop:=apply(test_date,1,max, na.rm=T)]

#assign zero to infs
train_numeric[train_numeric$Start==Inf]$Start=0
train_numeric[train_numeric$Stop==-Inf]$Stop=0

test_numeric[test_numeric$Start==Inf]$Start=0
test_numeric[test_numeric$Stop==-Inf]$Stop=0

##################################
##Feature to measure time spent in each line
line_stations<-list(1:24,25:28,29:31, 32:54)
line_names<-c("L0", "L1", "L2","L3")


  for (i in 1:4){
    STN_names<-names(train_numeric)
    min<-apply(train_date[,line_stations[[i]], with= FALSE],1, min, na.rm=T)
    max<-apply(train_date[,line_stations[[i]], with= FALSE],1, max, na.rm=T)
    diff<-max-min
    train_numeric<-cbind(train_numeric, min, max, diff)
    names(train_numeric)<-c(STN_names,paste0(line_names[i],"_entry"),paste0(line_names[i],"_exit"),paste0(line_names[i],"_time"))
    }

  for (i in 1:4){
    STN_names<-names(test_numeric)
    min<-apply(test_date[,line_stations[[i]], with= FALSE],1, min, na.rm=T)
    max<-apply(test_date[,line_stations[[i]], with= FALSE],1, max, na.rm=T)
    diff<-max-min
    test_numeric<-cbind(test_numeric, min, max, diff)
    names(test_numeric)<-c(STN_names,paste0(line_names[i],"_entry"),paste0(line_names[i],"_exit"),paste0(line_names[i],"_time"))
  }



##Combine data and sort by start time
combined_numeric<-rbind(train_numeric,test_numeric)
combined_numeric<-combined_numeric[order(Start,Id),]

#reorder by Id
combined_numeric<-combined_numeric[order(Id),]

####################
#Seperate data
train_leak<-combined_numeric[combined_numeric$Id %in% Id_train,]

#merge in responses
train_leak$Response<-Response

train_leak_failure<-train_leak[train_leak$Response==1,]
train_leak_failure<-train_leak_failure[order(Start,Id),]

train_leak_failure$csum<-cumsum(train_leak_failure$Response)

#assign zero to infs
combined_numeric[combined_numeric==Inf]=0
combined_numeric[combined_numeric==-Inf]=0

#Fit spline to failure data
spl <- smooth.spline(train_leak_failure[train_leak_failure$Start!=Inf,]$Start, y=train_leak_failure[train_leak_failure$Start!=Inf,]$csum)
pred <- predict(spl)

#Add Derivatives
combined_numeric$first_derive<-predict(spl,combined_numeric$Start,deriv=1)$y
combined_numeric$second_derive<-predict(spl,combined_numeric$Start,deriv=2)$y
combined_numeric$total_time<-combined_numeric$Stop-combined_numeric$Start

#Seperate data
train_leak<-combined_numeric[combined_numeric$Id %in% Id_train,]
test_leak<-combined_numeric[combined_numeric$Id %in% Id_test,]

saveRDS(train_leak, file="train_leak51-10202016.RDS")
saveRDS(test_leak, file="test_leak51-10202016.RDS")
