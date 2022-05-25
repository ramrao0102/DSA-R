
# HW7 Group 6, Lin and Rao's Homework

library(dplyr)
library(ggplot2)
library(car)
library(caret)
library(gmodels)
library(pROC)
library(ROCR) 
library(PRROC)
library(ggplotify)
library(forcats)
library(outliers)
library(mice)
library(VIM)
library(stats)
library(MASS)
library(forecast)
library(glmnet)
library(earth)

library(rpart)         # CART algorithm
library(party)         # to print trees
library(partykit)      # to print trees using "party"
library(rattle)        # for graphics
library(adabag)        # for boosting
library(ipred)         # for bagging and error estimation
library(randomForest)  # for Random Forests
library(caret)         # for training and modeling
library(kernlab)




file <- 'c:/Data Science and Analytics/DSA5103/hm7-Train.csv' #you will need to import your data
file1 <- 'c:/Data Science and Analytics/DSA5103/hm7-Test.csv'

hospitaltrain <-read.csv(file)
hospitaltest <-read.csv(file1)
hospitaltrain1<-hospitaltrain
hospitaltrain1$readmitted <-  as.factor(hospitaltrain1$readmitted)

glimpse(hospitaltrain)

glimpse(hospitaltest)

# convert characters to factors in train dataset

for (i in 1:45){
  if (is.character(hospitaltrain[,i])==TRUE){
    hospitaltrain[,i] = factor(hospitaltrain[,i])
  }
}

for (i in 1:45){
  if (is.character(hospitaltrain[,i])==TRUE){
    hospitaltrain1[,i] = factor(hospitaltrain[,i])
  }
}

# convert characters to factors in test dataset

for (i in 1:44){
  if (is.character(hospitaltest[,i])==TRUE){
    hospitaltest[,i] = factor(hospitaltest[,i])
  }
}


#  Creation of Histograms from Train DataSet

par(mfrow=c(3,2))

hist(hospitaltrain$discharge_disposition,  xlab = "Discharge Disposition", ylab = "Frequency", main = "Discharge Histogram", col ="blue")  # distribution before transformation
hist(hospitaltrain$time_in_hospital, xlab = "Time in Hospital", ylab = "Frequency", main = "Time in Hospital Histogram", col ="yellow")       
hist(hospitaltrain$num_lab_procedures, xlab = "No of Lab Procedures", ylab = "Frequency", main = "Lab Procedures Histogram", col ="green")
hist(hospitaltrain$num_procedures, xlab = "No of Procedures", ylab = "Frequency", main = "Number of Procedures Histogram", col ="orange")
hist(hospitaltrain$num_medications , xlab = "No of Medications", ylab = "Frequency", main = "Number of Medications Histogram", col ="orange")
hist(hospitaltrain$number_emergency, xlab = "No of Emergency Visits", ylab = "Frequency", main = "Number of Emergency Visits Histogram", col ="red")

# boxplot before transformation from Train DataSet


boxplot(hospitaltrain$discharge_disposition, xlab = "Discharge Disposition", ylab = "Values", 
        main = "Discharge Disposition Box Plot")    
boxplot(hospitaltrain$time_in_hospital, xlab = "Time in Hospital", ylab = "Values", 
        main = "Time in Hospital")  
boxplot(hospitaltrain$num_lab_procedures, xlab = "No. of Lab Procedures", ylab = "Values", 
        main = "No. of Lab Procedures")  
boxplot(hospitaltrain$discharge_disposition, xlab = "Number of Procedures", ylab = "Values", 
        main = "Number of Procedures")  
boxplot(hospitaltrain$num_medications, xlab = "Number of Medications", ylab = "Values", 
        main = "Number of Medications")   
boxplot(hospitaltrain$number_emergency, xlab = "Number of Emergency Visits", ylab = "Values", 
        main = "Number of Emergency Visits") 

# Check for Transformations

BoxCox.lambda(hospitaltrain$discharge_disposition) 

hist((hospitaltrain$discharge_disposition**-0.3125-1)/-0.3125,xlab = "PageViews/w Lambda = -0.3125", 
     ylab = "Frequency", main = "Pageviews Histogram", col ="blue")  

logdischarge_disposition = log(hospitaltrain$discharge_disposition)
logtimeinhospital = log(hospitaltrain$time_in_hospital)
logdmnumoflabprocedures = log(hospitaltrain$time_in_hospital)
lognumofmedications = log(hospitaltrain$num_medications)
lognumofemergency = log(hospitaltrain$number_emergency)


hist(logdischarge_disposition,xlab = "logDischarge_Disposition", 
     ylab = "Frequency", main = "Discharge Disposition", col ="blue")

BoxCox.lambda(hospitaltrain$time_in_hospital) 

BoxCox.lambda(hospitaltrain$num_lab_procedures) 

BoxCox.lambda(hospitaltrain$num_medications) 

BoxCox.lambda(hospitaltrain$number_emergency) 




# Estimation of Outliers in Select Variables

outlier(hospitaltrain$discharge_disposition)
grubbs.test(hospitaltrain$discharge_disposition)
hospitaltrain[hospitaltrain$discharge_disposition==outlier(hospitaltrain$discharge_disposition),]
meandischarge_disposition<- mean(hospitaltrain$discharge_disposition, na.rm = TRUE)
sddischarge_disposition<-3*sd(hospitaltrain$discharge_disposition, na.rm =TRUE)
count(filter(hospitaltrain, discharge_disposition >(meandischarge_disposition+sddischarge_disposition)))


par(mfrow=c(1,1))

# Check for Missing Values in Train DataSet

md.pairs(hospitaltrain)
md.pattern(hospitaltrain)    
a<-aggr(hospitaltrain)
summary(a)


# Check for Missing Values in Test DataSet

md.pairs(hospitaltest)
md.pattern(hospitaltest)    
a1<-aggr(hospitaltest)
summary(a1)




# Logistic Regression Models

# all variables

fit1 <- glm(data=hospitaltrain, readmitted ~ race+gender+age+admission_type+admission_source+
              time_in_hospital+num_lab_procedures +num_procedures+num_medications
              +number_outpatient+number_emergency+number_diagnoses+max_glu_serum +A1Cresult+metformin+
              repaglinide+nateglinide+glimepiride+glipizide+glyburide+tolbutamide+pioglitazone
              +rosiglitazone +acarbose +miglitol +troglitazone+tolazamide +insulin+glyburide.metformin
              +metformin.pioglitazone +diabetesMed,
               family="binomial")


# removing variables that have NA in them.

fit2 <- glm(data=hospitaltrain, readmitted ~ gender+age+admission_type+admission_source+
              time_in_hospital+num_lab_procedures +num_procedures+num_medications
            +number_outpatient+number_emergency+number_diagnoses+max_glu_serum +A1Cresult+metformin+
              repaglinide+nateglinide+glimepiride+glipizide+glyburide+tolbutamide+pioglitazone
            +rosiglitazone +acarbose +miglitol +troglitazone+tolazamide +insulin+glyburide.metformin
            +metformin.pioglitazone +diabetesMed,
            family="binomial")


# removing variables that have high p values


fit3 <- glm(data=hospitaltrain, readmitted ~ gender+age+admission_type+admission_source+
             time_in_hospital+num_lab_procedures +num_procedures+num_medications+number_inpatient 
           +number_outpatient+number_emergency+number_diagnoses+max_glu_serum +A1Cresult+
           +rosiglitazone +troglitazone+tolazamide +insulin+diabetesMed,
           family="binomial")



#transformed to log for select variables

fit4 <- glm(data=hospitaltrain, readmitted ~ gender+age+admission_type+admission_source+
             log(time_in_hospital+1)+log(num_lab_procedures+1) +log(num_procedures+1)+log(num_medications+1)
            +log(number_outpatient+1)+log(number_emergency+1)+log(number_diagnoses+1)+max_glu_serum +A1Cresult+
              +rosiglitazone +troglitazone+tolazamide +insulin+diabetesMed,
            family="binomial")


# Taking a random sample to assess model diagnostics! Otherwise, it takes too long to run the function!
# Also to use for Bagged Tree and RDA Analyses

RandomSample<-hospitaltrain[sample(nrow(hospitaltrain),10000), ]
RandomSample2<-hospitaltrain[sample(nrow(hospitaltrain),5000), ]
RandomSample1<-hospitaltrain1[sample(nrow(hospitaltrain1), 5000), ]

glimpse(RandomSample)


fitControl <- trainControl(method='repeatedcv',
                           number=5,
                           repeats=5,
                           search='random',
                          )

fit <- train(readmitted ~ age+num_lab_procedures+number_inpatient+number_outpatient+number_diagnoses+number_emergency+admission_type,
             data=hospitaltrain,
             method='glm',
             family = 'binomial',
             trControl=fitControl)

#Accuracy and Kappa Calculations, Resides in Confusion Matrix Output

readmitttedfactored <-as.factor(hospitaltrain$readmitted)
predfactored <- as.factor(as.numeric(fit$finalModel$fitted.values>.5))


conf_matrix<- confusionMatrix(predfactored , readmitttedfactored, positive="1")



fitlog <- train(readmitted ~ age+log(num_lab_procedures+1)+log(number_inpatient+1)+log(number_outpatient+1)+log(number_diagnoses+1)+log(number_emergency+1)+admission_type,
             data=hospitaltrain,
             method='glm',
             family = 'binomial',
             trControl=fitControl)

#Accuracy and Kappa Calculations, Resides in Confusion Matrix Output

readmitttedfactored <-as.factor(hospitaltrain$readmitted)


predlogfactored <- as.factor(as.numeric(fitlog$finalModel$fitted.values>.5))

conf_matrixlog<- confusionMatrix(predlogfactored  , readmitttedfactored, positive="1")



#MARS Classification Regression


firstearth <- earth(readmitted ~ age++num_lab_procedures +num_procedures+num_medications
            +number_outpatient+number_emergency+number_diagnoses, 
            data = hospitaltrain,
            glm=list(family=binomial),
            degree = 2,       
            nprune = 5)



secondearth <-earth(readmitted ~ gender+age+admission_type+admission_source+
                      time_in_hospital+num_lab_procedures +num_procedures+num_medications
                    +number_outpatient+number_emergency+number_diagnoses+max_glu_serum +A1Cresult+
                      +rosiglitazone +troglitazone+tolazamide +insulin+diabetesMed, 
                    data = hospitaltrain,
                    glm=list(family=binomial),
                    degree = 3,       
                    nprune = 5)

#thirdearth used for analyses

thirdearth <- earth(readmitted ~ age+num_lab_procedures+number_inpatient+number_outpatient+number_diagnoses+number_emergency+admission_type,
             data=hospitaltrain,
             glm=list(family=binomial),
             degree = 3,       
             nprune = 5)

thirdearth$coefficients

thirdearthlog <- earth(readmitted ~ age+log(num_lab_procedures+1)+log(number_inpatient+1)+log(number_outpatient+1)+log(number_diagnoses+1)+log(number_emergency+1)+admission_type,
                    data=hospitaltrain,
                    glm=list(family=binomial),
                    degree = 3,       
                    nprune = 5)

#Accuracy and Kappa Calculations uing thirdearth


readmitttedfactored <-as.factor(hospitaltrain$readmitted)

predthirdearth<-predict(thirdearth, hospitaltrain, type = "response")

predthirdearthdataframe <-as.data.frame(predthirdearth)


predthirdearthfactored <- as.factor(as.numeric(predthirdearthdataframe>.5))

conf_matrixthirdearth<- confusionMatrix(predthirdearthfactored, readmitttedfactored, positive="1")


#thirdearth1 used for model diagnostics using function provided below, otherwise computationally requires too much time!

thirdearth1 <- earth(readmitted ~ age+num_lab_procedures+number_inpatient+number_outpatient+number_diagnoses+number_emergency+admission_type,
                    data=RandomSample,
                    glm=list(family=binomial),
                    degree = 3,       
                    nprune = 5)


predthirdearth1<-predict(thirdearth1, RandomSample, type = "response")

predagain<-as.numeric(predthirdearth1)

class(predagain)


# Decision Tree Regression Using Caret Package

model_rpart <- train(readmitted~ gender+age+admission_type+admission_source+
                       time_in_hospital+num_lab_procedures +num_procedures+num_medications++number_inpatient
                     +number_outpatient+number_emergency+number_diagnoses+max_glu_serum +A1Cresult+
                       +rosiglitazone +troglitazone+tolazamide +insulin+diabetesMed,
                         data=hospitaltrain, method='rpart')

model_rpart$finalModel$variable.importance

model_rpart$finalModel$y

rpartpred <- predict(model_rpart, hospitaltrain)

#Accuracy and Kappa for rpart Model

rpartobserved <-as.factor(model_rpart$finalModel$y)
rpartpredicted <- as.factor(as.numeric(rpartpred>.5))

conf_matrixrpart<- confusionMatrix(rpartpredicted, rpartobserved, positive="1")


# Bagged Model Predictions

bagmodel <- train(
  readmitted ~ age+num_lab_procedures+number_inpatient+number_outpatient+number_diagnoses+number_emergency,
  data = RandomSample2,
  method = "treebag",
  trControl = trainControl(method = "cv", number = 5),
  nbagg = 50,  
  control = rpart.control(minsplit = 2, cp = 0)
)


# Accuracy and Kappa

predbagfactored <- as.factor(as.numeric(predbag>.5))
observed <-as.factor(RandomSample1$readmitted)

conf_matrixbagtree<- confusionMatrix(predbagfactored, observed, positive="1")

predbag<- predict(bagmodel, new = RandomSample2)

#RDA Analyses

set.seed(825)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 5
  )
rdaFit <- train(readmitted ~ age+num_lab_procedures+number_inpatient+number_outpatient+number_diagnoses+number_emergency,
                data = RandomSample1,
                method = "rda", 
                trControl = fitControl, 
                tuneLength = 4,
                metric = "Accuracy")

plot(rdaFit)

# Accuracy and Kappa for the RDA Model

rdaFit$finalModel
A <- predict(rdaFit , new = RandomSample1)

conf_matrixRDAModel<- confusionMatrix(A , observed, positive="1")

# Prediction on Test Data Set

# Using Fit2

finalresult <- predict(fit, new = hospitaltest)


pred<-as.data.frame(finalresult)


submissionDf<-data.frame(patientID=hospitaltest$patientID, predReadmit= pred )
head(submissionDf)

write.csv(submissionDf, "c:/Data Science and Analytics/DSA5103/Homeworks/HW7/submission3.csv", row.names=FALSE)


# prediction with MARS


finalresultthirdearth<- predict(thirdearth, hospitaltest, type = "response")

pred<-as.data.frame(finalresultthirdearth)
hist(finalresultthirdearth, xlab = "Predicted Readmittance", ylab = "Frequency", main = "Readmittance Histogram", col ="grey")

submissionDf<-data.frame(patientID=hospitaltest$patientID, predReadmit= pred )
head(submissionDf)

write.csv(submissionDf, "c:/Data Science and Analytics/DSA5103/Homeworks/HW7/submissionlast.csv", row.names=FALSE)


#Model Diagnostics

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

results1 <-Model_Function2( x_numlogregyeartest3  , predmarsyear3test   )
results1$ROCCURVE

results1$r1
results1$conf_matrix
results1$F1_score




