# A-novel-approach-for-predicting-earth-quakes-using-data-mining-techniques
I have done it in R language in R*64 bit. Here I took a data set of already occured earth quakes, pre-processed it and then implemented the data mining algorithms for predicting earthquakes with the most possible accuracy. The data mining algorithms used here are: decision tree, k-means clustering, naive base classification and random forest.I have used R libraries aswell.

#decision tree code:
library("rpart")
library("rpart.plot")
play_decision <- read.table ("C:/Users/Hi/Documents/R/pre6.csv", header=TRUE, sep=",")
play_decision
summary(play_decision)
fit<-rpart (Occur ~ Latitude + Longitude + Magnitude + MMI, method="class", data=play_decision, control=rpart.control(minsplit=1), parms=list(split='information'))
summary(fit)
rpart.plot(fit,type=4,extra=2,clip.right.labs=FALSE,varlen=0,faclen=0)
newdata<-data.frame(Latitude="36.123",Longitude="21.897")
newdata
predict(model,newdata,type="class")

#K-Means clustering algorithm code:
library(plyr)
library(ggplot2)
library(cluster)
library(lattice)
library(graphics)
library(grid)
library(gridExtra)
grade_input =  as.data.frame(read.csv("C:/Users/Hi/Documents/R/p.csv")) 
grade_input
kmdata_orig = as.matrix(grade_input[,c("Latitude","Longitude","Magnitude")]) 
kmdata <- kmdata_orig[,1:3] 
kmdata
kmdata <- kmdata_orig[,2:4]
km <- kmeans(kmdata, 3, 5) 
print(km)
plot(kmdata, col = km$cluster) 
points(km$centers, col = 1:2, pch =10)

#Naive Base classification algorithm code:
library(e1071)
traindata <- read.table("C:/Users/Hi/Documents/R/pre33.csv",header=TRUE,sep=",") 
traindata
testdata <-  read.table("C:/Users/Hi/Documents/R/Naive_Mini.csv",header=TRUE,sep=",") 
testdata
data(traindata)
str(traindata)
countsToCases<-function(x,countcol="Freq"){ idx<-rep.int(seq_len(nrow(x)),x[[countcol]]) x[[countcol]]<-NULL x[idx,]}
a<-countsToCases(as.data.frame(play_decision))
head(a)
nrow(a)
model<-naiveBayes(Occur~.,data=a)
predict(model,a[sample(1:596,10,replace=FALSE),])
predict(model,a[sample(1:596,10,replace=FALSE),],type="raw")
m<-naiveBayes(Occur~.,data=traindata)
m
newdata<-data.frame(Year="2009",Mo="2",Dy="25",Hr="9",Mm="45",Sec="43",Latitude="36.123",Longitude="21.897",Magnitude="2.9",MMI="5")
newdata
predict(model,newdata,type="class")

#Random Forest algorithm code:
library(randomForest)
a<-read.table ("C:/Users/Hi/Documents/R/ppd.csv", header=TRUE, sep=",")
names(a)
set.seed(1234)
dec<-sample(2,nrow(a),replace=TRUE,prob=c(0.7,0.3))
train<-a[dec==1,]
validate<-a[dec==2,]
fit.forest<-randomForest(Occur~Latitude+Longitude, data=train, na.action=na.roughfix, importance=TRUE)
fit.forest
importance(fit.forest,type=2)
forest.pred<-predict(fit.forest,validate)
forest.perf<-table(validate$Occur,forest.pred,dnn=c("Actual","Predicted"))
forest.perf
performance<-function(table,n=2){
if(!all(dim(table)==c(2,2)))
stop("Must be a 2*2 matrix")
tn=table[1,1]
fp=table[1,2]
fn=table[2,1]
tp=table[2,2]
sensitivity=tp/(tp+fn)
specificity=tn/(tn+fp)
ppp=tp/(tp+fp)
npp=tn/(tn+fn)
hitrate=(tp+tn)/(tp+tn+fp+fn)
result<- paste("sensitivity=",round(sensitivity,n), "\nspecificity=",round(specificity,n), "\nPositive Predictive Value=",round(ppp,n), "\nNegative Predictive Value=",round(npp,n), "\nAccuracy=",round(hitrate,n),"\n",sep="") 
cat(result)
performance(forest.perf)
plot(fit.forest, ylim=c(0.0,0.1))
importance(fit.forest)
plot(importance(fit.forest),lty=2,pch=16)
newdata<-data.frame(Latitude="36.123",Longitude="21.897")
newdata
m<-randomForest(Occur~.,data=train)
m
predict(m,newdata,type="class")
hist(a$Latitude)
 
