RANDOM FOREST CODE (unstructured code):

## aggregated data by years
df<-read.csv("C://Users//carlvader//Downloads//data.csv", header=TRUE,stringsAsFactors
= FALSE)
df$Dwelltype[df$Dwelltype==""]<-"U"
df$Dwelltype<-as.factor(df$Dwelltype)
table(df$Dwelltype)

dfadm<-df[,c(1,3:69)]
dfadm$ADMISSIONS<-as.factor(dfadm$ADMISSIONS)
dfadm$admin<-ifelse(dfadm$ADMISSIONS==0,0,1)
dfadm$admin<-as.factor(dfadm$admin)

table(dfadm$admin)
dfadm<-dfadm[,c(2:69)]
dfadm$admin

## data partitioning
set.seed(124)
ind<-sample(2, nrow(dfadm), replace=TRUE,prob=c(0.8,0.2))
train1<-dfadm[ind==1,]
test1<-dfadm[ind==2,]
train1[is.na(train1)] <- 0
test1[is.na(test1)] <- 0


##################### randomforest inception #########################
## for admissions
library(randomForest)
rf1<-randomForest(Admissions~.,data=train1)
print(rf1)
library(caret)
p1<-predict(rf1,train1)
confusionMatrix(p1,train1$admin)
p2<-predict(rf1,test1)
confusionMatrix(p2,test1$admin)

##tuning random forest model
tuneRF(train1[,-66],train1[,66],stepfactor=1,plot=TRUE,
ntreeTry=240,trace=TRUE,improve=0.05)
rf1<-randomForest(admin~.,data=train1,
ntree=250,mtry=16,importance=TRUE,proximity=TRUE)
print(rf1)

## for readmissions
dfreadm<-df[,c(2:69)]
dfreadm$readmin<-ifelse(dfreadm$READMISSIONS==0,0,1)
dfreadm$readmin
dfreadm<-dfreadm[,c(2:69)]
dfreadm$readmin<-as.factor(dfreadm$readmin)
table(dfreadm$readmin)

set.seed(125)
ind<-sample(2, nrow(dfreadm), replace=TRUE,prob=c(0.8,0.2))
train2<-dfreadm[ind==1,]
test2<-dfreadm[ind==2,]
train2<-train2[colSums(!is.na(train2))>0]
test2<-test2[colSums(!is.na(train2))>0]

rf2<-randomForest(readmin~.,data=train2)
print(rf2)
p2<-predict(rf2,test2)
confusionMatrix(p2,test2$readmin)

##################bucketing types of admissions model - random forest ###################### 
dfadmb<-df[,c(1,3:69)]
dfadmb$admin<-ifelse(dfadmb$ADMISSIONS==0,'zero',ifelse(dfadmb$ADMISSIONS==1,'one', ifelse(dfadmb$ADMISSIONS>1 & dfadmb$ADMISSIONS<=5,'1to5',
ifelse(dfadmb$ADMISSIONS>5 & dfadmb$ADMISSIONS<=10,'5to10','10g'))))
dfadmb$admin<-as.factor(dfadmb$admin)
table(dfadmb$admin)
dfadmb$admin
dfadmb<-dfadmb[,c(2:69)]
dfadmb$admin
set.seed(124)
ind<-sample(2, nrow(dfadmb), replace=TRUE,prob=c(0.8,0.2))
trainb1<-dfadmb[ind==1,]

# for readmissions
dfreadm<-df[,c(2:69)]
dfreadm$readmin<-ifelse(dfreadm$READMISSIONS==0,0,1)
dfreadm$readmin
dfreadm<-dfreadm[,c(2:69)]
dfreadm$readmin<-as.factor(dfreadm$readmin)
table(dfreadm$readmin)
set.seed(125)
ind<-sample(2, nrow(dfreadm), replace=TRUE,prob=c(0.8,0.2))

train2<-dfreadm[ind==1,]
test2<-dfreadm[ind==2,]
train2<-train2[colSums(!is.na(train2))>0]
test2<-test2[colSums(!is.na(train2))>0]
rf2<-randomForest(readmin~.,data=train2)
print(rf2)
train1
library(caret)
varImpPlot(rf1,sort=T,n.var=10)
testb1<-dfadmb[ind==2,]
trainb1[is.na(train1)] <- 0
testb1[is.na(test1)] <- 0
rfb1<-randomForest(admin~.,data=trainb1)
print(rfb1)
library(caret)
p2<-predict(rfb1,testb1)
confusionMatrix(p2,testb1$admin)

################### BUCKETIZED DATA RANDOM FOREST(unstructured format):#########################

# for admissions
df1<-read.csv("C://Users//carlvader//Downloads//data2.csv", header=TRUE)
df<-df1[,-which(names(df1) == "Dwelltype")]
str(df) 

dfadm<-df[,c(1,3:68)]
dfadm$admin<-ifelse(dfadm$ADMISSIONS=='zero',ifelse(dfadm$ADMISSIONS==1,'one', (ifelse(dfadm$ADMISSIONS>1 &dfadm$ADMISSIONS<=5,'1to5',
(ifelse(dfadm$ADMISSIONS>5 & dfadm$ADMISSIONS<=10,'5to10','10g'))))))
table(dfadm$admin)
dfadm<-data.frame(scale(dfadm))
dfadm$admin<-as.factor(dfadm$admin)
table(dfadm$admin)
dfadm<-dfadm[,c(2:68)]
dfadm$admin

## data partitioning
set.seed(124)
ind<-sample(2, nrow(dfadm), replace=TRUE,prob=c(0.8,0.2))
train1<-dfadm[ind==1,]
test1<-dfadm[ind==2,]

## removing columns with NA values
train1=train1[colSums(!is.na(train1)) > 0]
test1=test1[colSums(!is.na(test1)) > 0]

## randomforest inception for bucket model
library(randomForest)
rf1<-randomForest(admin~.,data=train1)
print(rf1)
library(caret)
p1<-predict(rf1,train1)
confusionMatrix(p1,train1$admin)
p2<-predict(rf1,test1)
confusionMatrix(p2,test1$admin)
plot(rf1)

# tuning the model
tuneRF(train1[,-67],train1[,67],stepfactor=0.5,plot=TRUE,ntreeTry=200,trace=TRUE,improve=0.05)
hist(treesize(rf1),col="green")
varImpPlot(rf1,sort=T,n.var=15)
rf1<-randomForest(admin~.,data=train1,
ntree=200,mtry=16,importance=TRUE,proximity=TRUE)


# for readmissions
dfreadm<-df[,c(2:68)]
dfreadm$readmin<-ifelse(dfreadm$READMISSIONS==0,0,1)
dfreadm<-dfreadm[,c(2:68)]
dfreadm<-data.frame(scale(dfreadm))
dfreadm$readmin<-as.factor(dfreadm$readmin)

set.seed(125)
ind<-sample(2, nrow(dfreadm), replace=TRUE,prob=c(0.8,0.2))
train2<-dfreadm[ind==1,]
test2<-dfreadm[ind==2,]

train2<-train2[colSums(!is.na(train2))>0]
test2<-test2[colSums(!is.na(train2))>0]
rf2<-randomForest(readmin~.,data=train2)

p1<-predict(rf2,train2)
confusionMatrix(p1,train2$readmin)

p2<-predict(rf2,test2)
confusionMatrix(p2,test2$readmin)

#tuning parameters
tuneRF(train1[,-67],train1[,67],stepfactor=0.5,plot=TRUE,
ntreeTry=200,trace=TRUE,improve=0.05)
hist(treesize(rf2),col="green")
varImpPlot(rf2,sort=T,n.var=15)

LOGISTIC REGRESSION CODE (unstructured format):
df<-read.csv("C://Users//carlvader//Downloads//humanagg.csv", header=TRUE,stringsAsFactors
= FALSE)
df$Dwelltype[df$Dwelltype==""]<-"U"
table(df$Dwelltype)
str(df) #
dfadm<-df[,c(1,3:69)]
