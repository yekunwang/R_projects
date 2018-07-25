# Loading libraries
library(data.table)
library(magrittr)
library(dplyr)
library(ggplot2)
library(tree)
library(randomForest)
library(e1071)
library(caret)

# Loading Raw Data
x=fread("~/Downloads/Quantifind_VOG_Comments_Location_Hierarchy_2018Jan_May.txt",na.strings = c("NULL",""))
# Started the project with 210 columns, reduce the predictor space by looking at whether the predictor contains too much or too little into.
names(x)

# # Cleaning Raw Data 
# for(i in 1:ncol(dat)){
#   #print(summary(dat[,i,with=FALSE]))
#   if(is.na(unique(dat[,i,with=FALSE]))){
#     print(colnames(dat[,i,with=FALSE]))
#     print(i)
#   }
#   print(unique(dat[,i,with=FALSE]))
# }

dat=fread("~/Downloads/Quantifind_VOG_Comments_Location_Hierarchy_2018Jan_May.txt",na.strings = c("NULL",""))[,c("LocationID","HourID","TransactionType","ResponseLagDays","SurveyCompletionMinutes","OverallSatisfaction","LikelyToReturn","AnticipatedNeeds","AppearanceAndUniforms","BathRoomClean","ExperienceBetter","FoodAccurate","FoodTaste","FoodTemp","Friendly","OrganizedAndFast","TablesClean","WellTrained")]
dat$OverallSatisfaction = as.factor(ifelse(dat$OverallSatisfaction <= 2, "Low",ifelse(dat$OverallSatisfaction <= 4,"Medium","High")))
dat=as.data.frame(unclass(dat[complete.cases(dat),]),stringsAsFactors = TRUE)
names(dat)

# Split training and testing dataset
train=sample(1:nrow(dat),3800)
dat.test=dat[-train,]
osat.test=dat$OverallSatisfaction[-train]

# Naive Bayes
nb.dat=naiveBayes(OverallSatisfaction~.,data=dat,subset=train)
nb.pred=predict(nb.dat,dat[-train,])
table(nb.pred,osat.test)

sum(diag(table(nb.pred,osat.test)))/length(nb.pred)

#varImp(nb.dat)

# K-fold validation
k=10
folds=sample(seq(1:k),nrow(dat),replace = TRUE)
folds_accuracy=rep(NA,k)

for(i in 1:k){
  train=which(folds!=i)
  dat.test=dat[(folds==i),]
  osat.test=dat$OverallSatisfaction[(folds==i)]
  
  nb.dat=naiveBayes(OverallSatisfaction~.,data=dat,subset=train)
  nb.pred=predict(nb.dat,dat[-train,])
  
  folds_accuracy[i] = (sum(osat.test==nb.pred)/length(osat.test))
  
}

mean(folds_accuracy)
# 88% accuracy

# Tree a random forest model
rf.dat=randomForest(OverallSatisfaction~.,data=dat,subset=train,mtry=4,importance =TRUE)
yhat.rf = predict(rf.dat,newdata=dat[-train,])
table(yhat.rf,osat.test)
sum(diag(table(yhat.rf,osat.test)))/length(yhat.rf)
# 89% accuracy

importance(rf.dat)

