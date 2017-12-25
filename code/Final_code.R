setwd('C:/Users/Jagan/Desktop/studies/sem3/stats/workspace')
library(dplyr)

# function to assign zero's to a passed column
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}
factors.removal<-function(df){
  l <- sapply(df, function(x) !is.factor(x))
  return (l)
}

# function to create a data frame by reading all the CSV files from the start year to end year.
createDF<-function(startyr, endyr)
{
  csv <-list()
  i<-1
  trow<-0
  for (year in startyr:endyr){
    f3 <-paste("atp_matches",year,sep ="_")
    f4 <-paste(f3,"csv",sep=".")
    csv[[i]]<-data.frame(read.csv(f4))
    trow<-trow+nrow(csv[[i]])
    i<-i+1
  }
  print(trow)
  return (csv)
}
# function to calculate no of aces hit by a player on a given year.
stats.aces<-function(x)
{
  agg1<-aggregate(w_ace ~winner_id,data=x,FUN = sum)
  agg2<-aggregate(l_ace ~loser_id,data=x,FUN = sum)
  total<-full_join(agg1,agg2,by=c("winner_id"="loser_id"))
  total[,2][is.na(total[,2])]<-0
  total[,3][is.na(total[,3])]<-0
  return (total)
}
# function to calculate no of defense points gained by a player on a given year. 
stats.df<-function(x)
{
  agg1<-aggregate(w_df ~winner_id,data=x,FUN = sum)
  agg2<-aggregate(l_df ~loser_id,data=x,FUN = sum)
  total<-full_join(agg1,agg2,by=c("winner_id"="loser_id"))
  total[,2][is.na(total[,2])]<-0
  total[,3][is.na(total[,3])]<-0
  return (total)
}
#function to calculate no of serve pt defended by a player on a given year.
stats.svpt<-function(x)
{
  agg1<-aggregate(w_svpt ~winner_id,data=x,FUN = sum)
  agg2<-aggregate(l_svpt ~loser_id,data=x,FUN = sum)
  total<-full_join(agg1,agg2,by=c("winner_id"="loser_id"))
  total[,2][is.na(total[,2])]<-0
  total[,3][is.na(total[,3])]<-0
  return (total)
}
#function to calculate no of 1st service point won by a player on a given year.
stats.1stwon<-function(x)
{
  agg1<-aggregate(w_1stWon ~winner_id,data=x,FUN = sum)
  agg2<-aggregate(l_1stWon ~loser_id,data=x,FUN = sum)
  total<-full_join(agg1,agg2,by=c("winner_id"="loser_id"))
  total[,2][is.na(total[,2])]<-0
  total[,3][is.na(total[,3])]<-0
  return (total)
}
#function to calculate no of 2nd service point won by a player on a given year.
stats.2ndwon<-function(x)
{
  agg1<-aggregate(w_2ndWon ~winner_id,data=x,FUN = sum)
  agg2<-aggregate(l_2ndWon ~loser_id,data=x,FUN = sum)
  total<-full_join(agg1,agg2,by=c("winner_id"="loser_id"))
  total[,2][is.na(total[,2])]<-0
  total[,3][is.na(total[,3])]<-0
  return (total)
}
#function to calculate no of breakpoint saved by a player on a given year.
stats.bpSaved<-function(x)
{
  agg1<-aggregate(w_bpSaved ~winner_id,data=x,FUN = sum)
  agg2<-aggregate(l_bpSaved ~loser_id,data=x,FUN = sum)
  total<-full_join(agg1,agg2,by=c("winner_id"="loser_id"))
  total[,2][is.na(total[,2])]<-0
  total[,3][is.na(total[,3])]<-0
  return (total)
}
#function which splits the tournament id from the year-tournamentid column.
tournamentId<-function(x)
{
  tourney_id <- data.frame(do.call('rbind', strsplit(as.character(x$tourney_id),'-',fixed=TRUE)))
  x<-cbind(x,tourney_id)
  x <- subset( x, select = -tourney_id )
}

extracted.list<-createDF(2000,2017)
# removing davis cup matches since it is not useful.
for(i in  1:length(extracted.list))
{
  extracted.list[[i]]=extracted.list[[i]][extracted.list[[1]][5]!='D',]
}
df.list<-extracted.list
stats.total<-list()
#calculate the stats for a player by using the above functions for all the years.
for(i in 1:length(df.list))
{
  total.stats<-stats.aces(data.frame(df.list[i]))
  total.stats<-full_join(total.stats,stats.df(data.frame(df.list[i])),by=c("winner_id"="winner_id"))
  total.stats<-full_join(total.stats,stats.bpSaved(data.frame(df.list[i])),by=c("winner_id"="winner_id"))
  total.stats<-full_join(total.stats,stats.1stwon(data.frame(df.list[i])),by=c("winner_id"="winner_id"))
  total.stats<-full_join(total.stats,stats.2ndwon(data.frame(df.list[i])),by=c("winner_id"="winner_id"))
  total.stats<-full_join(total.stats,stats.svpt(data.frame(df.list[i])),by=c("winner_id"="winner_id"))
  win.match.count<-count(df.list[[i]], winner_id)
  loss.match.count<-count(df.list[[i]], loser_id)
  total.stats<-full_join(total.stats,win.match.count,by=c("winner_id"="winner_id"))
  total.stats<-full_join(total.stats,loss.match.count,by=c("winner_id"="loser_id"))
  colnames(total.stats)[ncol(total.stats)-1]<-"win.matches.count"
  colnames(total.stats)[ncol(total.stats)]<-"loss.matches.count"
  #since it is a full join lot of columns expected to be NA. Hence marking it as a zero.
  for(j in 2:15)
  {
    total.stats[,j][is.na(total.stats[,j])]<-0
  }
  # Calculating the stats per match.
  total.stats$acePerc<-((total.stats$w_ace+total.stats$l_ace)/(total.stats$win.matches.count+total.stats$loss.matches.count))
  total.stats$dfPerc<-((total.stats$w_df+total.stats$l_df)/(total.stats$win.matches.count+total.stats$loss.matches.count))
  total.stats$svptPerc<-((total.stats$w_svpt+total.stats$l_svpt)/(total.stats$win.matches.count+total.stats$loss.matches.count))
  total.stats$s1stwonPerc<-((total.stats$w_1stWon+total.stats$l_1stWon)/(total.stats$win.matches.count+total.stats$loss.matches.count))
  total.stats$s2ndwonPerc<-((total.stats$w_2ndWon+total.stats$l_2ndWon)/(total.stats$win.matches.count+total.stats$loss.matches.count))
  total.stats$bpSavedPerc<-((total.stats$w_bpSaved+total.stats$l_bpSaved)/(total.stats$win.matches.count+total.stats$loss.matches.count))
  for(j in 16:21)
  {
    total.stats[,j]<-round(total.stats[,j],2)
  }
  # renaming the fist column as p_id.
  colnames(total.stats)[1]<-"p_id"
  stats.total[[i]]<-total.stats
}

df.list<-extracted.list
set.seed(5)
for(i in 1:length(df.list))
{
  colnames(df.list[[i]])[1]<-"tourney_id" #2016 is causing a problem. Hence this line is included.
  df.list[[i]]<-tournamentId(data.frame(df.list[i]))
  df.list[[i]]$rid<-seq.int(nrow(df.list[[i]]))
  #swapping the player1 and player2 to break the pattern of always the first player will be the winner..
  c<-sample(nrow(df.list[[i]]),nrow(df.list[[i]])/2)
  t1<-df.list[[i]][c,c("rid","winner_id","loser_id")]
  t2<-df.list[[i]][-c,c("rid","winner_id","loser_id")]
  colnames(t1)[2]<- "p1"
  colnames(t1)[3]<- "p2"
  colnames(t2)[2]<- "p2"
  colnames(t2)[3]<- "p1"
  t3<-rbind(t1,t2)
  
  df.list[[i]]$rankDiff<-df.list[[i]]$winner_rank - df.list[[i]]$loser_rank
  df.list[[i]]$ageDiff<-round(df.list[[i]]$winner_age - df.list[[i]]$loser_age)
  t4<-full_join(t3,df.list[[i]][,c("rid","winner_id","surface","X2","rankDiff","ageDiff")],by=c("rid"="rid"))
  #check if the winner_id and pl id is true then set it to true. otherwise false.
  t4$winner<-(t4$winner_id == t4$p1)
  t4<-t4[!is.na(t4$winner),]
  t4$rankDiff[t4$winner==FALSE]<- - t4$rankDiff[t4$winner==FALSE]
  t4$ageDiff[t4$winner==FALSE]<- - t4$ageDiff[t4$winner==FALSE]
  #combining the previous year stats of the player for current form of this year's tournament.
  if(i>1)
  {
    t4<-left_join(t4,stats.total[[i-1]][,c("p_id","acePerc","dfPerc","svptPerc","s1stwonPerc","s2ndwonPerc","bpSavedPerc")],by=c("p1"="p_id"))
    # limitation- converting NA to 0 doesn't differentiate from actual zero.
    for(j in 10:15)
    {
      colnames(t4)[j] <-paste("p1",colnames(t4)[j],sep=".")
      t4[,j]<-na.zero(t4[,j])
    }
    t4<-left_join(t4,stats.total[[i-1]][,c("p_id","acePerc","dfPerc","svptPerc","s1stwonPerc","s2ndwonPerc","bpSavedPerc")],by=c("p2"="p_id"))
    for(j in 16:21)
    {
      colnames(t4)[j] <-paste("p2",colnames(t4)[j],sep=".")
      t4[,j]<-na.zero(t4[,j])
    }
  }
  #No stats for the first year hence setting it as zero in all the columns.
  else
  {
    for (j in 1:12)
    {
      t4<-cbind(t4,rep(0,nrow(t4)))
      colnames(t4)[ncol(t4)]<-paste("pl.stats",j,sep=".")
    }
  }
  t4<-t4[,c(1,2,3,5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,4,9)]
  df.list[[i]]<-t4
}
#renaming the column names for the first year.
for(i in 8:19)
colnames(df.list[[1]])[i]<-colnames(df.list[[2]])[i]
trainDS<-df.list[[1]]
for(i in 2:(length(df.list)-1))
{
  trainDS<-rbind(trainDS,df.list[[i]])
}
testDS<-df.list[[length(df.list)]]
# View(testDS)
# nrow(trainDS) 
# nrow(testDS)
# View(trainDS)
trainDS$acdDiff=trainDS$p1.acePerc - trainDS$p2.acePerc
trainDS$dfDiff=trainDS$p1.dfPerc - trainDS$p2.dfPerc
trainDS$svptDiff=trainDS$p1.svptPerc - trainDS$p2.svptPerc
trainDS$s1stwonDiff=trainDS$p1.s1stwonPerc - trainDS$p2.s1stwonPerc
trainDS$s2ndWonDiff=trainDS$p1.s2ndwonPerc - trainDS$p2.s2ndwonPerc
trainDS$bpDiff=trainDS$p1.bpSavedPerc- trainDS$p2.bpSavedPerc

testDS$acdDiff=testDS$p1.acePerc - testDS$p2.acePerc
testDS$dfDiff=testDS$p1.dfPerc - testDS$p2.dfPerc
testDS$svptDiff=testDS$p1.svptPerc - testDS$p2.svptPerc
testDS$s1stwonDiff=testDS$p1.s1stwonPerc - testDS$p2.s1stwonPerc
testDS$s2ndWonDiff=testDS$p1.s2ndwonPerc - testDS$p2.s2ndwonPerc
testDS$bpDiff=testDS$p1.bpSavedPerc- testDS$p2.bpSavedPerc
trainDS$winner<-as.factor(trainDS$winner)
testDS$winner<-as.factor(testDS$winner)
#write.csv(testDS,"Test_Dataset_withRankAndAge.csv")
#write.csv(trainDS,"Train_Dataset_withRankAndAge.csv")
trainDS<-trainDS[,c(2:4,6:7,21:27)]
testDS<-testDS[,c(2:4,6:7,21:27)]
trainDS[,4]<-na.zero(trainDS[,4])
testDS[,4]<-na.zero(testDS[,4])
trainDS[,5]<-na.zero(trainDS[,5])
testDS[,5]<-na.zero(testDS[,5])
View(testDS)
# View(trainDS)
# View(testDS)
## designing models


library(tree) 
library(randomForest)
#Random Forest.
testMSE <-rep(NA, 6)
trainMSE <-rep(NA, 6)
j<-1
for(i in seq(300, 800, 100))
{  
model.rf <- randomForest(winner~rankDiff+ageDiff+acdDiff+svptDiff+s1stwonDiff+s1stwonDiff+s2ndWonDiff
                         +bpDiff, data = trainDS, mtry = 3, ntree = i, importance = TRUE,na.action = na.exclude)
#training error calculation
pred<-predict(model.rf,newdata=trainDS)
trainMSE[j]=mean(pred != trainDS$winner)
#test error calculation
pred<-predict(model.rf,newdata=testDS)
testMSE[j]=mean(pred != testDS$winner)
print(testMSE[j])
j<-j+1
}
testMSE
rf.vis<-cbind(seq(300, 800, 100),cbind(round(trainMSE,2),round(testMSE,2)))
rf.vis
write.csv(rf.vis,"Random_forest.csv")
#lines(seq(300, 800, 100), trainMSE[!is.na(trainMSE)], type = "l", col="red")
#legend("topright", c("trainMSE", "testMSE"), col = c("green", "red"), cex = 1, lty = 1)
#plot(seq(300, 800, 100), testMSE[!is.na(testMSE)], type = "b", xlab = "Trees", ylab = "Test MSE")
#BOOSTING
library(gbm)
set.seed(1)
trainDS$winner<-ifelse(trainDS$winner==TRUE,1,0)
testDS$winner<-ifelse(testDS$winner==TRUE,1,0)
pows <- seq(-2, 1, by = 0.2)
lambdas <- 10^pows
train.err <- rep(NA, length(lambdas))
test.err <- rep(NA, length(lambdas))
i<-1
for (i in 1:length(lambdas)) {
  boost.winners <- gbm(winner~rankDiff+ageDiff+acdDiff+svptDiff+s1stwonDiff+s1stwonDiff+s2ndWonDiff
                       +bpDiff, data = trainDS, n.trees = 1000, shrinkage = lambdas[i])
  pred.train <- predict(boost.winners, trainDS, n.trees = 1000,type="response")
  pred.test <- predict(boost.winners, testDS, n.trees = 1000)
  pred.train <- ifelse(pred.train > 0.5,1,0)
  pred.test <- ifelse(pred.test > 0.5,1,0)
  train.err[i]<-mean(pred.train != trainDS$winner)
  test.err[i]<-mean(pred.test != testDS$winner)
  print(train.err[i])
  print(test.err[i])
}
boosting.vis<-cbind(seq(-2,1,by=0.2),cbind(round(train.err,2),round(test.err,2))
boosting.vis
train.err
write.csv(boosting.vis,"boosting.csv")


# one time code to be run
#------------------------------
# csv.2016<-data.frame(read.csv("atp_matches_2017.csv"))
# colnames(csv.2016)[1]<-"tourney_id"
# View(csv.2016)
# new.df<-csv.2016[(csv.2016$tourney_level!=''),]
# View(new.df)
# new.df<-new.df[(new.df$tourney_level!='D'),]
# write.csv(new.df,"atp_matches_2017_mod.csv")
#multiple linear regression
model.lm=lm(winner~.,data=trainDS)
#only the important predictors are taken.
model.lm=lm(winner~rankDiff+ageDiff+acdDiff+svptDiff+s1stwonDiff+s1stwonDiff+s2ndWonDiff
            +bpDiff, data=trainDS)
summary(model.lm)
confint(model.lm)
pred.train <- predict(model.lm, newdata = trainDS)
pred.test <- predict(model.lm, newdata = testDS)
pred.train <- ifelse(pred.train > 0.5,1,0)
pred.test <- ifelse(pred.test > 0.5,1,0)
train.err<-mean(pred.train != trainDS$winner)
test.err<-mean(pred.test != testDS$winner)
lm.vis<-cbind(c(rep("lm",2)),c("Train","Test"),c(train.err,test.err))
lm.vis

#logistic linear regression
#----------------------------
summary(model.glm)
#other values which has a p-value less than 0.05 is removed.
model.glm=glm(winner~rankDiff+ageDiff+acdDiff+svptDiff+s1stwonDiff+s1stwonDiff+s2ndWonDiff
             +bpDiff, data=trainDS)
summary(model.glm)
confint(model.glm)
pred.train <- predict(model.glm, newdata = trainDS)
pred.test <- predict(model.glm, newdata = testDS)
pred.train <- ifelse(pred.train > 0.5,1,0)
pred.test <- ifelse(pred.test > 0.5,1,0)
mean(pred.train != trainDS$winner)
mean(pred.test != testDS$winner)
train.err<-mean(pred.train != trainDS$winner)
test.err<-mean(pred.test != testDS$winner)
lm.vis<-rbind(lm.vis,cbind(c(rep("glm",2)),c("Train","Test"),c(train.err,test.err)))
lm.vis
write.csv(lm.vis,"logistic_model_vis.csv")
#10-fold cross-validation
library(boot)
cv.error=rep (0,4)
poly.vis<-data.frame()
for(j in 4:12)
{
  if(j!=6)
  {
    print(j)
    print("------")
    for (i in 1:4){
      fm<-as.formula(paste(colnames(trainDS)[6], "~ poly(",colnames(trainDS)[j],",",i,")"))
      glm.fit=glm(fm,data=trainDS)
      cv.error[i]=cv.glm (trainDS ,glm.fit,K=10)$delta [1]
      print(cv.error[i])
    }
  }
  poly.vis<-rbind(poly.vis,cbind(rep(colnames(trainDS)[j],4),seq(1,4,1),cv.error))
}
write.csv(poly.vis,"poly_vis.csv")

#General additive model
library (gam)
model.gam=gam(winner~p1+p2+surface+s(rankDiff,4)+s(ageDiff ,5)+s(acdDiff,4)+s(dfDiff,4)
           +s(svptDiff,5)+s(s1stwonDiff,5)+s(s2ndWonDiff,5)+s(bpDiff,5),data=trainDS)
pred.gam.train<-predict(model.gam,trainDS)
pred.gam.test<-predict(model.gam,testDS)
pred.gam.test <- ifelse(pred.gam.test > 0.5,1,0)
pred.gam.train <- ifelse(pred.gam.train > 0.5,1,0)
train.err<-mean(pred.gam.train != trainDS$winner)
test.err<-mean(pred.gam.test != testDS$winner)
gam.vis<-cbind(c("train","test"),c(train.err,test.err))
write.csv(gam.vis,"gam.csv")

##Ridge and Lasso Regression
#-------------------------------
l <- sapply(testDS, function(x) !is.factor(x))
library(glmnet)
train.mat<-model.matrix(winner ~.,data=trainDS[,l] )
test.mat<-model.matrix(winner ~.,data=testDS[,l] )

grid =10^ seq (10,-2, length =100)
ridge.mod =glmnet(train.mat,trainDS$winner,alpha =0, lambda=grid ,thresh =1e-12)
cv.out =cv.glmnet (train.mat,trainDS$winner,alpha =0, lambda=grid ,thresh =1e-12)
bestlam =cv.out$lambda.min
bestlam
ridge.pred.train=predict(ridge.mod,s=bestlam,newx=train.mat)
ridge.pred.test=predict(ridge.mod,s=bestlam,newx=test.mat)

ridge.pred.train <- ifelse(ridge.pred.train > 0.5,1,0)
ridge.pred.test<- ifelse(ridge.pred.test > 0.5,1,0)
misclasification.error.train.ridge<-mean(ridge.pred.train != trainDS$winner)
misclasification.error.test.ridge<-mean(ridge.pred.test != testDS$winner)
ridge.vis<-cbind(c("train","test"),c(misclasification.error.train.ridge,misclasification.error.test.ridge))
write.csv(ridge.vis,"ridge.csv")


lasso.mod =glmnet(train.mat,trainDS$winner,alpha =1, lambda=grid ,thresh =1e-12)
cv.out =cv.glmnet (train.mat,trainDS$winner,alpha =1, lambda=grid ,thresh =1e-12)
bestlam =cv.out$lambda.min
bestlam
lasso.pred.train=predict(lasso.mod,s=bestlam,newx=train.mat)
lasso.pred.train <- ifelse(lasso.pred.train > 0.5,1,0)
lasso.pred.test=predict(lasso.mod,s=bestlam,newx=test.mat)
lasso.pred.test <- ifelse(lasso.pred.test > 0.5,1,0)
misclasification.error.train.lasso<-mean(lasso.pred.train != trainDS$winner)
misclasification.error.test.lasso<-mean(lasso.pred.test != testDS$winner)
lasso.vis<-cbind(c("train","test"),c(misclasification.error.train.lasso,misclasification.error.test.lasso))
write.csv(lasso.vis,"lasso.csv")

##LDA and QDA Model
##------------
library(MASS)
ldaModelNew=lda(winner ~.,data=trainDS[,l] ) 
ldaTrain=predict(ldaModelNew,trainDS[,l])
ldaclass.train <- ldaTrain$class
misclasification.error.train.lda<-mean(ldaclass.train != trainDS$winner)
ldaTest=predict(ldaModelNew,testDS[,l])
ldaclass.test <- ldaTest$class
misclasification.error.test.lda<-mean(ldaclass.test != testDS$winner)
misclasification.error.test.lda
lda.vis<-cbind(c("train","test"),c(misclasification.error.train.lda,misclasification.error.test.lda))
write.csv(lda.vis,"lda.csv")

qdaModelNew=qda(winner ~.,data=trainDS[,l] ) 
qdaTrain=predict(qdaModelNew,trainDS[,l])
qdaclass.train <- qdaTrain$class
misclasification.error.train.qda<-mean(qdaclass.train != trainDS$winner)
qdaTest=predict(qdaModelNew,testDS[,l])
qdaclass.test <- qdaTest$class
misclasification.error.test.qda<-mean(qdaclass.test != testDS$winner)
misclasification.error.test.qda
qda.vis<-cbind(c("train","test"),c(misclasification.error.train.qda,misclasification.error.test.qda))
write.csv(qda.vis,"qda.csv")

##KNN
##---------
library(class)
trainPred=trainDS[,names(trainDS)!="winner"]
trainPred<-trainPred[,factors.removal(trainPred)]
testPred=testDS[,names(testDS)!="winner"]
testPred<-testPred[,factors.removal(testPred)]
trainWinner=trainDS$winner
testWinner=testDS$winner
set.seed(1)
knn.vis<-data.frame()
for(i in 1:4)
{
pred=knn (as.matrix(trainPred),as.matrix(testPred),trainWinner ,k=i)
table(pred ,testWinner)
knn.vis<-rbind(knn.vis,cbind(i,mean(pred !=testWinner)))
}
write.csv(knn.vis,"Knn.csv")
