#########################
#Third set of features used for model
#input: Processed features from previous features script and processed numerical and
#       categorical data.
#output:  features saved as an RDS
#
#Generates some further features in the spirit of the leak.  Compare each part
#to other parts before and after it in a number of features.


library(dplyr)
library(data.table)
library(xgboost)
library(Matrix)
library(caret)
library(ggplot2)

train_numeric<-as.data.table(readRDS("processed_train_numeric-10182016.RDS"))
test_numeric<-as.data.table(readRDS("processed_test_numeric-10182016.RDS"))
train_leak<-as.data.table(readRDS('train_leak51-10202016.RDS'))
test_leak<-as.data.table(readRDS('test_leak51-10202016.RDS'))

train_cat<-as.data.table(readRDS("processed_train_categorical-10182016.RDS"))
test_cat<-as.data.table(readRDS("processed_test_categorical-10182016.RDS"))

Id_train<-train_numeric$Id
Id_test<-test_numeric$Id
Y<-train_numeric$Response

train_numeric[,Id:=NULL]
test_numeric[,Id:=NULL]

train_cat[,Id:=NULL]
test_cat[,Id:=NULL]

train_numeric[,Response:=NULL]


train_leak$sum<-rowSums(train_numeric)
test_leak$sum<-rowSums(test_numeric)

train_leak$cat_sum<-rowSums(train_cat, na.rm = TRUE)
test_leak$cat_sum<-rowSums(test_cat,na.rm = TRUE)


train_leak$L3_S32_F3851<-train_cat$L3_S32_F3851
test_leak$L3_S32_F3851<-test_cat$L3_S32_F3851

train_leak$L3_S32_F3853<-train_cat$L3_S32_F3853
test_leak$L3_S32_F3853<-test_cat$L3_S32_F3853

train_leak$L3_S32_F3854<-train_cat$L3_S32_F3854
test_leak$L3_S32_F3854<-test_cat$L3_S32_F3854

train_leak$L1_S24_F1844<-train_numeric$L1_S24_F1844
test_leak$L1_S24_F1844<-test_numeric$L1_S24_F1844

train_leak$L3_S33_F3855<-train_numeric$L3_S33_F3855
test_leak$L3_S33_F3855<-test_numeric$L3_S33_F3855


#train_leak$Response=train_numeric$Response
#test_leak$Response=0

train_leak$Response<-Y
test_leak$Response<-0
#test_leak$Response<-fread("sub-2213Rand-400roundsAllFeatTuneParamsecond.csv",select = c("Response"),showProgress = T)
combined_numeric<-rbind(train_leak,test_leak)

combined_numeric<-combined_numeric[order(Id),]

combined_numeric[,magic_sum:=c(diff(combined_numeric$sum,lag=1),0)]
combined_numeric[,magic_sum_back:=c(0,rev(diff(rev(combined_numeric$sum),lag=1)))]

combined_numeric[,magic_start:=c(diff(combined_numeric$Start,lag=1),0)]
combined_numeric[,magic_start_back:=c(0,rev(diff(rev(combined_numeric$Start),lag=1)))]

combined_numeric[,magic_stop:=c(diff(combined_numeric$Stop,lag=1),0)]
combined_numeric[,magic_stop_back:=c(0,rev(diff(rev(combined_numeric$Stop),lag=1)))]

combined_numeric[,magic_L0:=c(diff(combined_numeric$L0_time,lag=1),0)]
combined_numeric[,magic_L0_back:=c(0,rev(diff(rev(combined_numeric$L0_time),lag=1)))]

combined_numeric[,magic_L1:=c(diff(combined_numeric$L1_time,lag=1),0)]
combined_numeric[,magic_L1_back:=c(0,rev(diff(rev(combined_numeric$L1_time),lag=1)))]

combined_numeric[,magic_L2:=c(diff(combined_numeric$L2_time,lag=1),0)]
combined_numeric[,magic_L2_back:=c(0,rev(diff(rev(combined_numeric$L2_time),lag=1)))]

combined_numeric[,magic_L3:=c(diff(combined_numeric$L3_time,lag=1),0)]
combined_numeric[,magic_L3_back:=c(0,rev(diff(rev(combined_numeric$L3_time),lag=1)))]

combined_numeric[,magic_L0_entry:=c(diff(combined_numeric$L0_entry,lag=1),0)]
combined_numeric[,magic_L0_entry_back:=c(0,rev(diff(rev(combined_numeric$L0_entry),lag=1)))]

combined_numeric[,magic_L0_exit:=c(diff(combined_numeric$L0_exit,lag=1),0)]
combined_numeric[,magic_L0_exit_back:=c(0,rev(diff(rev(combined_numeric$L0_exit),lag=1)))]

combined_numeric[,magic_L1_entry:=c(diff(combined_numeric$L1_entry,lag=1),0)]
combined_numeric[,magic_L1_entry_back:=c(0,rev(diff(rev(combined_numeric$L1_entry),lag=1)))]

combined_numeric[,magic_L1_exit:=c(diff(combined_numeric$L1_exit,lag=1),0)]
combined_numeric[,magic_L1_exit_back:=c(0,rev(diff(rev(combined_numeric$L1_exit),lag=1)))]

combined_numeric[,magic_L2_entry:=c(diff(combined_numeric$L2_entry,lag=1),0)]
combined_numeric[,magic_L2_entry_back:=c(0,rev(diff(rev(combined_numeric$L2_entry),lag=1)))]

combined_numeric[,magic_L2_exit:=c(diff(combined_numeric$L2_exit,lag=1),0)]
combined_numeric[,magic_L2_exit_back:=c(0,rev(diff(rev(combined_numeric$L2_exit),lag=1)))]

combined_numeric[,magic_L3_entry:=c(diff(combined_numeric$L3_entry,lag=1),0)]
combined_numeric[,magic_L3_entry_back:=c(0,rev(diff(rev(combined_numeric$L3_entry),lag=1)))]

combined_numeric[,magic_L3_exit:=c(diff(combined_numeric$L3_exit,lag=1),0)]
combined_numeric[,magic_L3_exit_back:=c(0,rev(diff(rev(combined_numeric$L3_exit),lag=1)))]

combined_numeric[,magic_cat:=c(diff(combined_numeric$cat_sum,lag=1),0)]
combined_numeric[,magic_cat_back:=c(0,rev(diff(rev(combined_numeric$cat_sum),lag=1)))]

combined_numeric$fail_before<-lead(combined_numeric$Response,1, default = 0)
combined_numeric$fail_before2<-lead(combined_numeric$Response,2, default = 0)

combined_numeric$fail_after<-lag(combined_numeric$Response,1, default = 0)
combined_numeric$fail_after2<-lag(combined_numeric$Response,2, default = 0)

combined_numeric[,F3851_front:=c(diff(combined_numeric$L3_S32_F3851,lag=1),0)]
combined_numeric[,F3851_front_back:=c(0,rev(diff(rev(combined_numeric$L3_S32_F3851),lag=1)))]

combined_numeric[,F3853_front:=c(diff(combined_numeric$L3_S32_F3853,lag=1),0)]
combined_numeric[,F3853_front_back:=c(0,rev(diff(rev(combined_numeric$L3_S32_F3853),lag=1)))]

combined_numeric[,F3854_front:=c(diff(combined_numeric$L3_S32_F3854,lag=1),0)]
combined_numeric[,F3854_front_back:=c(0,rev(diff(rev(combined_numeric$L3_S32_F3854),lag=1)))]

combined_numeric[,F1844_front:=c(diff(combined_numeric$L1_S24_F1844,lag=1),0)]
combined_numeric[,F1844_front_back:=c(0,rev(diff(rev(combined_numeric$L1_S24_F1844),lag=1)))]

combined_numeric[,F3855_front:=c(diff(combined_numeric$L3_S33_F3855,lag=1),0)]
combined_numeric[,F3855_front_back:=c(0,rev(diff(rev(combined_numeric$L3_S33_F3855),lag=1)))]


##look forward or back two
combined_numeric[,magic_sum2:=c(diff(combined_numeric$sum,lag=2),0,0)]
combined_numeric[,magic_sum_back2:=c(0,0,rev(diff(rev(combined_numeric$sum),lag=2)))]

combined_numeric[,magic_start2:=c(diff(combined_numeric$Start,lag=2),0,0)]
combined_numeric[,magic_start_back2:=c(0,0,rev(diff(rev(combined_numeric$Start),lag=2)))]

combined_numeric[,magic_stop2:=c(diff(combined_numeric$Stop,lag=2),0,0)]
combined_numeric[,magic_stop_back2:=c(0,0,rev(diff(rev(combined_numeric$Stop),lag=2)))]

combined_numeric[,magic_L02:=c(diff(combined_numeric$L0_time,lag=2),0,0)]
combined_numeric[,magic_L0_back2:=c(0,0,rev(diff(rev(combined_numeric$L0_time),lag=2)))]

combined_numeric[,magic_L12:=c(diff(combined_numeric$L1_time,lag=2),0,0)]
combined_numeric[,magic_L1_back2:=c(0,0,rev(diff(rev(combined_numeric$L1_time),lag=2)))]

combined_numeric[,magic_L22:=c(diff(combined_numeric$L2_time,lag=2),0,0)]
combined_numeric[,magic_L2_back2:=c(0,0,rev(diff(rev(combined_numeric$L2_time),lag=2)))]

combined_numeric[,magic_L32:=c(diff(combined_numeric$L3_time,lag=2),0,0)]
combined_numeric[,magic_L3_back2:=c(0,0,rev(diff(rev(combined_numeric$L3_time),lag=2)))]

combined_numeric[,magic_L0_entry2:=c(diff(combined_numeric$L0_entry,lag=2),0,0)]
combined_numeric[,magic_L0_entry_back2:=c(0,0,rev(diff(rev(combined_numeric$L0_entry),lag=2)))]

combined_numeric[,magic_L0_exit2:=c(diff(combined_numeric$L0_exit,lag=2),0,0)]
combined_numeric[,magic_L0_exit_back2:=c(0,0,rev(diff(rev(combined_numeric$L0_exit),lag=2)))]

combined_numeric[,magic_L1_entry2:=c(diff(combined_numeric$L1_entry,lag=2),0,0)]
combined_numeric[,magic_L1_entry_back2:=c(0,0,rev(diff(rev(combined_numeric$L1_entry),lag=2)))]

combined_numeric[,magic_L1_exit2:=c(diff(combined_numeric$L1_exit,lag=2),0,0)]
combined_numeric[,magic_L1_exit_back2:=c(0,0,rev(diff(rev(combined_numeric$L1_exit),lag=2)))]

combined_numeric[,magic_L2_entry2:=c(diff(combined_numeric$L2_entry,lag=2),0,0)]
combined_numeric[,magic_L2_entry_back2:=c(0,0,rev(diff(rev(combined_numeric$L2_entry),lag=2)))]

combined_numeric[,magic_L2_exit2:=c(diff(combined_numeric$L2_exit,lag=2),0,0)]
combined_numeric[,magic_L2_exit_back2:=c(0,0,rev(diff(rev(combined_numeric$L2_exit),lag=2)))]

combined_numeric[,magic_L3_entry2:=c(diff(combined_numeric$L3_entry,lag=2),0,0)]
combined_numeric[,magic_L3_entry_back2:=c(0,0,rev(diff(rev(combined_numeric$L3_entry),lag=2)))]

combined_numeric[,magic_L3_exit2:=c(diff(combined_numeric$L3_exit,lag=2),0,0)]
combined_numeric[,magic_L3_exit_back2:=c(0,0,rev(diff(rev(combined_numeric$L3_exit),lag=2)))]

combined_numeric[,magic_cat2:=c(diff(combined_numeric$cat_sum,lag=2),0,0)]
combined_numeric[,magic_cat_back2:=c(0,0,rev(diff(rev(combined_numeric$cat_sum),lag=2)))]


combined_numeric[,F3851_front2:=c(diff(combined_numeric$L3_S32_F3851,lag=2),0,0)]
combined_numeric[,F3851_front_back2:=c(0,0,rev(diff(rev(combined_numeric$L3_S32_F3851),lag=2)))]

combined_numeric[,F3853_front2:=c(diff(combined_numeric$L3_S32_F3853,lag=2),0,0)]
combined_numeric[,F3853_front_back2:=c(0,0,rev(diff(rev(combined_numeric$L3_S32_F3853),lag=2)))]

combined_numeric[,F3854_front2:=c(diff(combined_numeric$L3_S32_F3854,lag=2),0,0)]
combined_numeric[,F3854_front_back2:=c(0,0,rev(diff(rev(combined_numeric$L3_S32_F3854),lag=2)))]

combined_numeric[,F1844_front2:=c(diff(combined_numeric$L1_S24_F1844,lag=2),0,0)]
combined_numeric[,F1844_front_back2:=c(0,0,rev(diff(rev(combined_numeric$L1_S24_F1844),lag=2)))]

combined_numeric[,F3855_front2:=c(diff(combined_numeric$L3_S33_F3855,lag=2),0,0)]
combined_numeric[,F3855_front_back2:=c(0,0,rev(diff(rev(combined_numeric$L3_S33_F3855),lag=2)))]


combined_numeric[,Response:=NULL]
combined_numeric[,L3_S32_F3851:=NULL]
combined_numeric[,L3_S32_F3853:=NULL]
combined_numeric[,L3_S32_F3854:=NULL]
combined_numeric[,L1_S24_F1844:=NULL]
combined_numeric[,L3_S33_F3855:=NULL]


train_leak<-combined_numeric[combined_numeric$Id %in% Id_train,]
test_leak<-combined_numeric[combined_numeric$Id %in% Id_test,]


saveRDS(train_leak, file="train_leak55-10222016.RDS")
saveRDS(test_leak, file="train_leak55-10222016.RDS")
