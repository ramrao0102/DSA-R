

library(dplyr)
library(ggplot2)
library(car)
library(caret)
library(gmodels)
library(pROC)
library(ROCR) 
library(PRROC)
library(ggplotify)

file <- 'c:/Data Science and Analytics/DSA5103/heartFailure.csv' #you will need to import your data
heartFailure <-read.csv(file)
glimpse(heartFailure)

#we will use a simple model for the example
fit2 <- glm(data=heartFailure, death~age +creatinine_phosphokinase+ serum_creatinine + ejection_fraction, family="binomial")
summary(fit2)

fit1 <- glm(data=heartFailure, death~.   , family="binomial")
summary(fit1)

fit3 <- glm(data=heartFailure, death~age + serum_creatinine + ejection_fraction, family="binomial")
summary(fit3)



#the "fit" glm object has a lot of useful information
names(fit3)

head(fit3$data)             # all of the data is stored
head(fit3$y)                # the "true" value of the binary target  
head(fit3$fitted.values,100)    # the predicted probabilities
fit3$deviance               # the residual deviance


#let's look at logisitic regression residuals 

plot(predict(fit3),residuals(fit3))  #plot predicted value vs residuals
abline(h=0,lty=2,col="grey")

plot(predict(fit3),residuals(fit3),col=c("blue","red")[1+heartFailure$death])
abline(h=0,lty=2,col="grey")



rl=loess(residuals(fit3)~predict(fit3))
X<-data.frame(yHat=rl$x,predictedR=rl$fitted)
X<-X[order(X[,1]),]
lines(X,col="black",lwd=1)

#adding error bars

rl=loess(residuals(fit3)~predict(fit3))
y=predict(rl,se=TRUE)
segments(predict(fit3),y$fit+2*y$se.fit,predict(fit3),y$fit-2*y$se.fit,col="green")


#standard plots available for logistic regression
plot(fit1)


#now let's look at leverage and influence
barplot(cooks.distance(fit1))

influence.measures(fit1)


influencePlot(fit3)

vif(fit3)


#Obtain from Developed Function

# Includes all Predictor Variables
results_Model1 <-Model_Function1(fit1, fit1$y, fit1$fitted.values)
results_Model1$D_statistic

# Includes Select Variables
results_Model2 <-Model_Function1(fit2, fit2$y, fit2$fitted.values)
results_Model2$D_statistic

# Includes Variables Idenified in Final Model
results_Model3 <-Model_Function1(fit3, fit3$y, fit3$fitted.values)
results_Model3$D_statistic

# Decision Tree Modeling

# check other page, need to bring it in to cleanup!

library(rpart)         # CART algorithm
library(party)         # to print trees
library(partykit)      # to print trees using "party"
library(rattle)        # for graphics
library(adabag)        # for boosting
library(ipred)         # for bagging and error estimation
library(randomForest)  # for Random Forests
library(caret)         # for training and modeling

dim(heartFailure)[1]

#Point #1 --> Tree are unstable

#run the following bit of code -- it will produce 7 different trees
#based on slightly different samples of heartFailure data
#make sure to use the "back arrows" in the Plots window to view 
#all of the trees ---> they are VERY different from each other!

seeds <- c(3,67,71,67,78,2,1)

for (i in 1:length(seeds)){
  
  set.seed(seeds[i])                        #try different seeds
  s <- sample(dim(heartFailure)[1], 200)
  train <- heartFailure[s, ]
  test <- heartFailure[-s, ]
  
  #use CART tree
  fitCart <- rpart(data=train, death ~.)
  fitTreeParty<-as.party(fitCart)
  plot(fitTreeParty)
  print (fitCart$variable.importance)
}

#not only are the tree structures different, but the above code also 
#printed out the "Variable importance" measures -->
#the results were different each time.



#Now,fix the seed so we can all take about the same example
set.seed(1)  
s <- sample(dim(heartFailure)[1], 150)
train <- heartFailure[s, ]
test <- heartFailure[-s, ]


#use CART to build a single tree
fitCart <- rpart(data=train, death ~.)
fitTreeParty<-as.party(fitCart)
plot(fitTreeParty)


names(fitCart)

#how well did our tree do?  (on training data?)
predtrain = predict(fitCart, newdata = train)
predtrain[150]

fitCart$pred = predtrain


pred1 <- as.factor(pred1)
pred1<-as.numeric(predtrain>0.5)

death1<- as.factor (train$death)

confusionMatrix(pred1, death1)



#how well did our tree do?  (on test data?)
pred = predict(fitCart, newdata=test)
pred1 <- as.factor(pred)
pred<-as.numeric(pred>0.5)

death2<- as.factor (test$death)


confusionMatrix(pred1, death2)



#examine variable importance
fitCart$variable.importance
barplot(fitCart$variable.importance, main = "CART Variable Importance")


varImp(fitCart)   #from caret package -- a slighter different calculation


#examine relative error and CV error 
#(note to get actual error scale by root misclassification rate)
#different error at each cost parameter level

# note the cost values shown are related to the "alpha" value from the lecture

fitCart$cptable  #this extracts the complexity paramater cost 
#larger values of CP are associated with less complex trees


#next look at complexity for pruning
plotcp(fitCart)


#we can prune the tree accordingly

#prune tree
pfitCart1<-prune(fitCart,cp=0.077)
fancyRpartPlot(pfitCart1)

predCart1 = predict(pfitCart1, newdata=test)
predCart1<-as.numeric(predCart1>0.5)
predCart1<-as.factor(predCart1)
death2 <-as.factor(test$death)

confusionMatrix(predCart1, death1)


pfitCart2<-prune(fitCart,cp=0.026)  

fancyRpartPlot(pfitCart2)

predCart2 = predict(pfitCart2, newdata=train)
pfitCart2$pred = predCart2
predCart2<-as.numeric(predCart2>0.5)
predCart2<-as.factor(predCart2)



confusionMatrix(predCart2 , death1)


#now for BAGGING!

#we will use the bagging from "ipred" package




fitbag <- bagging(death ~ ., data = train, coob = T)  #coob=T --> compute oob error estimate

fitbag$mtrees[1]

predBag = predict(fitbag, newdata=train)
fitbag$pred = predBag

predBag<-as.numeric(predBag>0.5)
predBag = as.factor(predBag)


# Accuracy is better than for Trees

#Random Forest 

?randomForest
#notice some parameters: 
# -- ntree: number of trees
# -- mtry: the value "m" from lecture


rfFit <- randomForest(death ~ ., data = train, importance = T, ntrees=1500, mtry=3)
rfFit$y


predRF = predict(rfFit, newdata=train)
rfFit$pred = predRF

predtestRf = predict(rfFit, newdata = test)


predRF = as.numeric(predRF >0.5)
predRF = as.factor(predRF)

confusionMatrix(predRF, death1)





#The plot method traces the error rates (out-of-bag, and by each response
#category) as the number of trees increases. 

plot(rfFit)

rfFit$importance

#The importance option in the randomForest function requests the assessment of 
#predictor importances. There are two global measures: 
#one is the mean descrease in accuracy over all classes,
#the other is the mean decrease in Gini index. 


par(mfrow = c(2, 1))
barplot(rfFit$importance[, 1], main = "Importance (Dec.Accuracy)")
barplot(rfFit$importance[, 2], main = "Importance (Gini Index)")
par(mfrow = c(1, 1))

varImpPlot(rfFit)



# Random Forest with Caret

rfGrid <-  expand.grid(mtry = 2:9)  #let's tune across mtry = 2,3,...,9

rf_model<-train(death~., data=heartFailure,
                method="rf",             #random forest
                trControl=trainControl(method="cv",number=5),  #cross-validation 
                tuneGrid=rfGrid,   #hyper-parameter tuning
                allowParallel=TRUE)

#prints out the CV accuracy and kappa for each mtry value
print(rf_model)

#the best model (based on tuning grid)
print(rf_model$finalModel)
rf_model$finalModel$predicted

rf_model$finalModel$importance

rf_model$finalModel$y




predRFCaret <- as.factor(predRFCaret)
confusionMatrix(predRFCaret, death2)




# RandomForest with Caret

rfGrid <-  expand.grid(mtry = 2:9)  #let's tune across mtry = 2,3,...,9

rf_model<-train(death~., data=heartFailure1,
                method="rf",             #random forest
                trControl=trainControl(method="cv",number=5),  #cross-validation 
                tuneGrid=rfGrid,   #hyper-parameter tuning
                allowParallel=TRUE)

#prints out the CV accuracy and kappa for each mtry value
print(rf_model)

#the best model (based on tuning grid)
print(rf_model$finalModel)
rf_model$finalModel$predicted

rf_model$finalModel$importance



predRFCaret<-as.numeric(rf_model$finalModel$predicted>0.5)
predRFCaret <- as.factor(predRFCaret)
confusionMatrix(predRFCaret, death2)

death2 = heartFailure1$death
death2 = as.factor(death2)




# Checking for # of Correct on TreeCorrect, BaggingCorrect, and ForestCorect


df<-data.frame(Truth = test$death, 
               Tree = predCart2,
               Bagging = predBag, 
               Forest = predRF)

#for each observation, you can look at the true Glass type,
#and the classifications from each competing technique
df

# Model Function for Logit Regression Analysis


Model_Function1 = function(model, trueVal, predProb)
{
  
  # Confusion Matrix and Statistics
  
  a1 <- as.factor(trueVal)
  a2 <- as.factor(as.numeric(predProb>.5))
  
  conf_matrix<- confusionMatrix(a2, a1, positive="1")
  
  r1<-roc(trueVal, predProb)
  
  
  
  pred <- prediction(predProb, trueVal)    #ROC curve for training data
  perf <- performance(pred,"tpr","fpr") 
  
  ROCCURVE= as.ggplot(function()plot(perf,  main = "Plot of ROC Curve", print.cutoffs.at = c(0.25,0.5,0.75))+ abline(0, 1, col="red")) 
  
  pr_curve = pr.curve(predProb, trueVal, curve = T)
  plot(pr_curve)
  
  PRCURVE = as.ggplot(function() plot(pr_curve))
  
  TN <- conf_matrix$table[1,1]
  TP <- conf_matrix$table[2,2]
  FN <- conf_matrix$table[1,2]
  FP <- conf_matrix$table[2,1]
  
  Precision <- TP/(TP+FP)
  
  Recall <- TP/(TP+FN)
  
  F1_score = 2*Precision*Recall/(Precision+Recall)
  
  mcc_num <- (TP*TN - FP*FN)
  mcc_den <- 
    as.double((TP+FP))*as.double((TP+FN))*as.double((TN+FP))*as.double((TN+FN))
  
  mcc_final <- mcc_num/sqrt(mcc_den)
  
  
  Con_Dis_Data = cbind(trueVal, predProb) 
  
  ones = Con_Dis_Data[Con_Dis_Data[,1] == 1,]
  zeros = Con_Dis_Data[Con_Dis_Data[,1] == 0,]
  
  conc=matrix(0, dim(zeros)[1], dim(ones)[1])   #build a matrix of 0's 
  disc=matrix(0, dim(zeros)[1], dim(ones)[1])
  ties=matrix(0, dim(zeros)[1], dim(ones)[1])
  
  for (j in 1:dim(zeros)[1])
  {
    for (i in 1:dim(ones)[1])
    {
      if (ones[i,2]>zeros[j,2])
      {conc[j,i]=1}
      
      else if (ones[i,2]<zeros[j,2])
      {disc[j,i]=1}
      
      else if (ones[i,2]==zeros[j,2])
      {ties[j,i]=1}
    }
  }
  
  Pairs=dim(zeros)[1]*dim(ones)[1]              #total number of pairs
  PercentConcordance=(sum(conc)/Pairs)*100
  PercentDiscordance=(sum(disc)/Pairs)*100
  PercentTied=(sum(ties)/Pairs)*100
  AUC=PercentConcordance +(0.5 * PercentTied)
  
  # Additional Diagnostics
  
  pearsonRes <-residuals(model,type="pearson")
  devianceRes <-residuals(model,type="deviance")
  rawRes <-residuals(model,type="response")
  studentDevRes<-rstudent(model)
  fv<-fitted(model)
  predVals <-  data.frame(trueVal=trueVal, predClass=predProb, predProb=fv, 
                          rawRes, pearsonRes, devianceRes, studentDevRes)  
  class.1<-predVals[predVals$trueVal==1,]
  class.0<-predVals[predVals$trueVal==0,]
  
  
  D_statistic <- mean(class.1$predProb) - mean(class.0$predProb)
  
  predVals$group<-cut(predVals$predProb,seq(1,0,-.1),include.lowest=T)
  xtab<-table(predVals$group,predVals$trueVal)
  
  xtab
  
  plot_studentDevRes <- as.ggplot(function() plot(studentDevRes))
  plot_barplotstudentDevRes <- as.ggplot(function()barplot(studentDevRes))
  
  #make empty dataframe
  KS<-data.frame(Group=numeric(10),
                 CumPct0=numeric(10),
                 CumPct1=numeric(10),
                 Dif=numeric(10))
  
  #fill data frame with information: Group ID, 
  #Cumulative % of 0's, of 1's and Difference
  for (i in 1:10) {
    KS$Group[i]<-i
    KS$CumPct0[i] <- sum(xtab[1:i,1]) / sum(xtab[,1])
    KS$CumPct1[i] <- sum(xtab[1:i,2]) / sum(xtab[,2])
    KS$Dif[i]<-abs(KS$CumPct0[i]-KS$CumPct1[i])
  }
  
  
  maxGroup<-KS[KS$Dif==max(KS$Dif),][1,1]
  
  #and the K-S chart
  KS_chart<- ggplot(data=KS)+
    geom_line(aes(Group,CumPct0),color="blue")+
    geom_line(aes(Group,CumPct1),color="red")+
    geom_segment(x=maxGroup,xend=maxGroup,
                 y=KS$CumPct0[maxGroup],yend=KS$CumPct1[maxGroup])+
    labs(title = "K-S Chart", x= "Deciles", y = "Cumulative Percent")
  
  
  #Gains Chart
  
  GAINSCHART = as.ggplot(function() plot(performance(pred,"tpr","rpp"), main = "Plot of Gains Chart"))
  
  #Lift Chart
  
  LIFTCHART = as.ggplot(function() plot(performance(pred,"lift","rpp"), main = "Plot of Lift Chart"))
  
  # Plot of Probability Distributions for 2 Class Problem
  
  plot(0,0,type="n", xlim= c(0,1), ylim=c(0,4),     
       xlab="Prediction", ylab="Density",  
       main="How well do the predictions separate the classes?")+
    
    for (runi in 1:length(pred@predictions)) {
      lines(density(pred@predictions[[runi]][pred@labels[[runi]]==1]), col= "blue")
      lines(density(pred@predictions[[runi]][pred@labels[[runi]]==0]), col="green")
    }
  
  
  
  return(list( "LIFTCHART" = LIFTCHART, "GAINSCHART" = GAINSCHART, "KS_chart" = KS_chart,
               "D_statistic" = D_statistic, "Percent Concordance"=PercentConcordance,
               "Percent Discordance"=PercentDiscordance,
               "Percent Tied"=PercentTied,
               "Pairs"=Pairs,
               "F1_score" = F1_score, "mcc_final" = mcc_final, "PRCURVE" = PRCURVE, 
               "ROCCURVE" = ROCCURVE, "conf_matrix" = conf_matrix, "r1" = r1,
               "pearsonRes" = pearsonRes, "devianceRes" = devianceRes, "rawRes" = rawRes,
               "studentDevRes" = studentDevRes,  "plot_studentDevRes" = plot_studentDevRes,
               "plot_barplot_studentDevRes" = plot_barplotstudentDevRes))
  
}  

results <-Model_Function1(fit3, fit3$y, fit3$fitted.values)
results$plot_studentDevRes


# Model Prediction for Decision Tree Analysis

Model_Function2 = function(trueVal, predProb)
{
  
  # Confusion Matrix and Statistics
  
  a1 <- as.factor(trueVal)
  a2 <- as.factor(as.numeric(predProb>.5))
  
  conf_matrix<- confusionMatrix(a2, a1, positive="1")
  
  r1<-roc(trueVal, predProb)
  
  
  
  pred <- prediction(predProb, trueVal)    #ROC curve for training data
  perf <- performance(pred,"tpr","fpr") 
  
  ROCCURVE= as.ggplot(function()plot(perf,  main = "Plot of ROC Curve", print.cutoffs.at = c(0.25,0.5,0.75))+ abline(0, 1, col="red")) 
  
  pr_curve = pr.curve(predProb, trueVal, curve = T)
  plot(pr_curve)
  
  PRCURVE = as.ggplot(function() plot(pr_curve))
  
  TN <- conf_matrix$table[1,1]
  TP <- conf_matrix$table[2,2]
  FN <- conf_matrix$table[1,2]
  FP <- conf_matrix$table[2,1]
  
  Precision <- TP/(TP+FP)
  
  Recall <- TP/(TP+FN)
  
  F1_score = 2*Precision*Recall/(Precision+Recall)
  
  mcc_num <- (TP*TN - FP*FN)
  mcc_den <- 
    as.double((TP+FP))*as.double((TP+FN))*as.double((TN+FP))*as.double((TN+FN))
  
  mcc_final <- mcc_num/sqrt(mcc_den)
  
  
  Con_Dis_Data = cbind(trueVal, predProb) 
  
  ones = Con_Dis_Data[Con_Dis_Data[,1] == 1,]
  zeros = Con_Dis_Data[Con_Dis_Data[,1] == 0,]
  
  conc=matrix(0, dim(zeros)[1], dim(ones)[1])   #build a matrix of 0's 
  disc=matrix(0, dim(zeros)[1], dim(ones)[1])
  ties=matrix(0, dim(zeros)[1], dim(ones)[1])
  
  for (j in 1:dim(zeros)[1])
  {
    for (i in 1:dim(ones)[1])
    {
      if (ones[i,2]>zeros[j,2])
      {conc[j,i]=1}
      
      else if (ones[i,2]<zeros[j,2])
      {disc[j,i]=1}
      
      else if (ones[i,2]==zeros[j,2])
      {ties[j,i]=1}
    }
  }
  
  Pairs=dim(zeros)[1]*dim(ones)[1]              #total number of pairs
  PercentConcordance=(sum(conc)/Pairs)*100
  PercentDiscordance=(sum(disc)/Pairs)*100
  PercentTied=(sum(ties)/Pairs)*100
  AUC=PercentConcordance +(0.5 * PercentTied)
  
  predVals <-  data.frame(trueVal=trueVal, predClass=predProb)  
  
  predVals$group<-cut(predVals$predClass,seq(1,0,-.1),include.lowest=T)
  xtab<-table(predVals$group,predVals$trueVal)
  
  xtab
  
  
  #make empty dataframe
  KS<-data.frame(Group=numeric(10),
                 CumPct0=numeric(10),
                 CumPct1=numeric(10),
                 Dif=numeric(10))
  
  #fill data frame with information: Group ID, 
  #Cumulative % of 0's, of 1's and Difference
  for (i in 1:10) {
    KS$Group[i]<-i
    KS$CumPct0[i] <- sum(xtab[1:i,1]) / sum(xtab[,1])
    KS$CumPct1[i] <- sum(xtab[1:i,2]) / sum(xtab[,2])
    KS$Dif[i]<-abs(KS$CumPct0[i]-KS$CumPct1[i])
  }
  
  
  maxGroup<-KS[KS$Dif==max(KS$Dif),][1,1]
  
  #and the K-S chart
  KS_chart<- ggplot(data=KS)+
    geom_line(aes(Group,CumPct0),color="blue")+
    geom_line(aes(Group,CumPct1),color="red")+
    geom_segment(x=maxGroup,xend=maxGroup,
                 y=KS$CumPct0[maxGroup],yend=KS$CumPct1[maxGroup])+
    labs(title = "K-S Chart", x= "Deciles", y = "Cumulative Percent")
  
  
  #Gains Chart
  
  GAINSCHART = as.ggplot(function() plot(performance(pred,"tpr","rpp"), main = "Plot of Gains Chart"))
  
  #Lift Chart
  
  LIFTCHART = as.ggplot(function() plot(performance(pred,"lift","rpp"), main = "Plot of Lift Chart"))
  
  # Plot of Probability Distributions for 2 Class Problem
  
  plot(0,0,type="n", xlim= c(0,1), ylim=c(0,7),     
       xlab="Prediction", ylab="Density",  
       main="How well do the predictions separate the classes?")+
    
    for (runi in 1:length(pred@predictions)) {
      lines(density(pred@predictions[[runi]][pred@labels[[runi]]==1]), col= "blue")
      lines(density(pred@predictions[[runi]][pred@labels[[runi]]==0]), col="green")
    }
  
  
  
  return(list( "LIFTCHART" = LIFTCHART, "GAINSCHART" = GAINSCHART, "KS_chart" = KS_chart,
               "Percent Concordance"=PercentConcordance,
               "Percent Discordance"=PercentDiscordance,
               "Percent Tied"=PercentTied,
               "Pairs"=Pairs,
               "F1_score" = F1_score, "mcc_final" = mcc_final, "PRCURVE" = PRCURVE, 
               "ROCCURVE" = ROCCURVE, "conf_matrix" = conf_matrix, "r1" = r1))
  
}  

results <-Model_Function2(fit1$y,fit1$fitted.values)
results$F1_score



