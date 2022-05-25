#Libraries Used
install.packages("sos")
install.packages("RWeka")
library(RWeka)
library(sos)
findFn("arff")
library(foreign)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggbiplot)
library(lubridate)
library(mice)
library(lattice)
library(visdat)
library(DMwR)
library(VIM)
library(Amelia)
library(funModeling)
library(PerformanceAnalytics)
library(EnvStats)
library(corrplot)
library(Hmisc)
library(mice)
library(dlookr)
library(caret)
library(car)
library(randomForest)
library(rpart)         
library(party)         
library(partykit)   
library(rattle)        
library(adabag)        
library(ipred) 
library(missForest)
require(ROCR)
library(ggplotify)
library(pROC)
library(Rtsne)
library(vctrs)
library(earth)
library(gbm)
library(MASS)
library(gmodels)
library(xgboost)

##############################Read the data file#################################
year2_data <- read.arff("C:\\Users\\risha\\Documents\\Data_Science_and_Analytics_Courses\\Intelligent Data Analytics\\Course_Project\\Bankrupcy_Data\\2year.arff")
year3_data <- read.arff("C:\\Users\\risha\\Documents\\Data_Science_and_Analytics_Courses\\Intelligent Data Analytics\\Course_Project\\Bankrupcy_Data\\3year.arff")

###############################Assigning appropriate column names to variables for Year 2 data##########
colnames(year2_data) <- c("net profit / total assets","total liabilities / total assets", "working capital / total assets","current assets / short-term liabilities",
                          "[(cash + short-term securities + receivables - short-term liabilities) / (operating expenses - depreciation)] * 365",
                          "retained earnings / total assets",
                          "EBIT / total assets",
                          "book value of equity / total liabilities",
                          "sales / total assets",
                          "equity / total assets",
                          "(gross profit + extraordinary items + financial expenses) / total assets",
                          "gross profit / short-term liabilities",
                          "(gross profit + depreciation) / sales",
                          "(gross profit + interest) / total assets",
                          "(total liabilities * 365) / (gross profit + depreciation)",
                          "(gross profit + depreciation) / total liabilities",
                          "total assets / total liabilities",
                          "gross profit / total assets",
                          "gross profit / sales",
                          "(inventory * 365) / sales",
                          "sales (n) / sales (n-1)",
                          "profit on operating activities / total assets",
                          "net profit / sales",
                          "gross profit (in 3 years) / total assets",
                          "(equity - share capital) / total assets",
                          "(net profit + depreciation) / total liabilities",
                          "profit on operating activities / financial expenses",
                          "working capital / fixed assets",
                          "logarithm of total assets",
                          "(total liabilities - cash) / sales",
                          "(gross profit + interest) / sales",
                          "(current liabilities * 365) / cost of products sold",
                          "operating expenses / short-term liabilities",
                          "operating expenses / total liabilities",
                          "profit on sales / total assets",
                          "total sales / total assets",
                          "(current assets - inventories) / long-term liabilities",
                          "constant capital / total assets",
                          "profit on sales / sales",
                          "(current assets - inventory - receivables) / short-term liabilities",
                          "total liabilities / ((profit on operating activities + depreciation) * (12/365))",
                          "profit on operating activities / sales",
                          "rotation receivables + inventory turnover in days",
                          "(receivables * 365) / sales",
                          "net profit / inventory",
                          "(current assets - inventory) / short-term liabilities",
                          "(inventory * 365) / cost of products sold",
                          " EBITDA (profit on operating activities - depreciation) / total assets",
                          "EBITDA (profit on operating activities - depreciation) / sales",
                          "current assets / total liabilities",
                          "short-term liabilities / total assets",
                          " (short-term liabilities * 365) / cost of products sold)",
                          "equity / fixed assets",
                          " constant capital / fixed assets",
                          " working capital",
                          " (sales - cost of products sold) / sales",
                          " (current assets - inventory - short-term liabilities) / (sales - gross profit - depreciation)",
                          " total costs /total sales",
                          " long-term liabilities / equity",
                          " sales / inventory",
                          " sales / receivables",
                          " (short-term liabilities *365) / sales",
                          " sales / short-term liabilities",
                          "sales / fixed assets",
                          "bankruptcy")
###############################Assigning appropriate column names to variables for Year 3 data############
colnames(year3_data) <- c("net profit / total assets","total liabilities / total assets", "working capital / total assets","current assets / short-term liabilities",
                    "[(cash + short-term securities + receivables - short-term liabilities) / (operating expenses - depreciation)] * 365",
                    "retained earnings / total assets",
                    "EBIT / total assets",
                    "book value of equity / total liabilities",
                    "sales / total assets",
                    "equity / total assets",
                    "(gross profit + extraordinary items + financial expenses) / total assets",
                    "gross profit / short-term liabilities",
                    "(gross profit + depreciation) / sales",
                    "(gross profit + interest) / total assets",
                    "(total liabilities * 365) / (gross profit + depreciation)",
                    "(gross profit + depreciation) / total liabilities",
                    "total assets / total liabilities",
                    "gross profit / total assets",
                    "gross profit / sales",
                    "(inventory * 365) / sales",
                    "sales (n) / sales (n-1)",
                    "profit on operating activities / total assets",
                    "net profit / sales",
                    "gross profit (in 3 years) / total assets",
                    "(equity - share capital) / total assets",
                    "(net profit + depreciation) / total liabilities",
                    "profit on operating activities / financial expenses",
                    "working capital / fixed assets",
                    "logarithm of total assets",
                    "(total liabilities - cash) / sales",
                    "(gross profit + interest) / sales",
                    "(current liabilities * 365) / cost of products sold",
                    "operating expenses / short-term liabilities",
                    "operating expenses / total liabilities",
                    "profit on sales / total assets",
                    "total sales / total assets",
                    "(current assets - inventories) / long-term liabilities",
                    "constant capital / total assets",
                    "profit on sales / sales",
                    "(current assets - inventory - receivables) / short-term liabilities",
                    "total liabilities / ((profit on operating activities + depreciation) * (12/365))",
                    "profit on operating activities / sales",
                    "rotation receivables + inventory turnover in days",
                    "(receivables * 365) / sales",
                    "net profit / inventory",
                    "(current assets - inventory) / short-term liabilities",
                    "(inventory * 365) / cost of products sold",
                    " EBITDA (profit on operating activities - depreciation) / total assets",
                    "EBITDA (profit on operating activities - depreciation) / sales",
                    "current assets / total liabilities",
                    "short-term liabilities / total assets",
                    " (short-term liabilities * 365) / cost of products sold)",
                    "equity / fixed assets",
                    " constant capital / fixed assets",
                    " working capital",
                    " (sales - cost of products sold) / sales",
                    " (current assets - inventory - short-term liabilities) / (sales - gross profit - depreciation)",
                    " total costs /total sales",
                    " long-term liabilities / equity",
                    " sales / inventory",
                    " sales / receivables",
                    " (short-term liabilities *365) / sales",
                    " sales / short-term liabilities",
                    "sales / fixed assets",
                    "bankruptcy")

#####################Looking over the summary of the Year 2 data####################
summary(year2_data)
glimpse(year2_data)

#another look at the data 
describe(year2_data)

head(year2_data)

Hmisc::describe(year2_data)

df_status(year2_data)

vis_dat(year2_data,sort_type = FALSE)

plot_num(year2_data[,1:15])
plot_num(year2_data[,16:30])
plot_num(year2_data[,31:45])
plot_num(year2_data[,46:60])
plot_num(year2_data[,61:65])

#####################Looking over the summary of the Year 3 data####################
summary(year3_data)
glimpse(year3_data)

#another look at the data 
describe(year3_data)

head(year3_data)

Hmisc::describe(year3_data)

statusDF <- df_status(year3_data)

dataVisual <- vis_dat(year3_data,sort_type = FALSE)

plot_num(year3_data[,1:15])
plot_num(year3_data[,16:30])
plot_num(year3_data[,31:45])
plot_num(year3_data[,46:60])
plot_num(year3_data[,61:65])


# Further Data Exploration with Year 3 Data

boxplot(` working capital` ~ Bankruptcy, data = year3_data)
plot(`gross profit / total assets` ~ Bankruptcy, data = year3_data)

ggplot(year3_data, aes(x=year3_data$`gross profit / short-term liabilities`, y=`net profit / total assets`, color=Bankruptcy)) + geom_point()

ggplot(year3_data, aes(x=year3_data$`gross profit / short-term liabilities`, y=data$`total assets / total liabilities`, color=Bankruptcy)) + geom_boxplot()

ggplot(year3_data, aes(x=data$`gross profit / sales`, y=year3_data$`book value of equity / total liabilities`, color=Bankruptcy)) + geom_area()

plot(x=year3_data$`net profit / total assets`,y=year3_data$`current assets / short-term liabilities`)

plot(x=year3_data$`EBIT / total assets`, y=year3_data$`profit on sales / total assets`)


hist(year3_data$`[(cash + short-term securities + receivables - short-term liabilities) / (operating expenses - depreciation)] * 365`, col = "blue")

hist(log(year3_data$`[(cash + short-term securities + receivables - short-term liabilities) / (operating expenses - depreciation)] * 365`), col = "green")

qplot(year3_data$` (current assets - inventory - short-term liabilities) / (sales - gross profit - depreciation)`, year3_data$` (sales - cost of products sold) / sales`)
#find and treat missing values

#######################################Check for the missing values in Year 2 data################
par(mar=c(2,1,1,1))
md.pairs(year2_data)

vis_miss(year2_data,sort_miss = TRUE)

aggr(year2_data)
#######################################Check for the missing values in Year 3 data############
par(mar=c(2,1,1,1))
missingDataInfo <- md.pairs(year3_data)

missVisual <- vis_miss(year3_data,sort_miss = TRUE)

missingness <- aggr(year3_data)

################Data Splitting into train and test dataset on raw dataset of Year 3####################
split <- createDataPartition(year3_data$bankruptcy, p = .8, list = FALSE)
trainDF <- transformedDataSet[split,]
testDF  <- transformedDataSet[-split,]

#########################Defining 5 fold cross validation and hyperparameter for tuning#########
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 5,
                     search = "random",
                     savePredictions = TRUE)

rfGrid <-  expand.grid(mtry = 10:15)

############################Random Forest Model before imputation for Year 3 data##############
fitRF_bef_imp <- caret::train(bankruptcy ~.,
                             data = trainDF,
                             method = "rf",
                             trControl = ctrl,
                             allowParallel=TRUE,
                             tuneGrid=rfGrid)

#####################Performance Evaluation before imputation for Year 3 data################
rfImpVar_bef_imp <- varImp(fitRF_bef_imp)

plot(rfImpVar_bef_imp, top = 30)

predict.rf_bef_imp=predict(fitRF_bef_imp,testDF, type ="prob")

forestpred_bef_imp = prediction(predict.rf_bef_imp[,2], testDF$bankruptcy)

perf_bef_imp <- performance(forestpred_bef_imp,"tpr","fpr") 

plot(perf_bef_imp,  main = "Plot of ROC Curve", colorize=TRUE, print.cutoffs.at = c(0.25,0.75)) + 
  abline(0, 1, col="red")

conf_matrix_bef_imp <- confusionMatrix(fitRF_bef_imp$finalModel$predicted, fitRF_bef_imp$finalModel$y, positive="1")
roc(as.numeric(fitRF_bef_imp$finalModel$predicted),as.numeric(fitRF_bef_imp$finalModel$y))

###################################
#Imputing outliers and missing values using missForest
outlierTreament <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- NA
  x[x > (qnt[2] + H)] <- NA
  return(x)
}

#########################Outlier Detection for Year 2 data#######################
diagnose_outlier(year2_data) 

plot_outlier(year2_data)

outlier_Data_year2 <- as.data.frame(sapply(year2_data[,-65],outlierTreament))
ImpDataSet_Year2 <- missForest(outlier_Data_year2)$ximp

summary(ImpDataSet_Year2)

diagnose_outlier(ImpDataSet_Year2)

plot_outlier(ImpDataSet_Year2)

outlier_Data_updated <- as.data.frame(sapply(ImpDataSet_Year2,outlierTreament))
imputedDS_Year2 <- knnImputation(outlier_Data_updated)

diagnose_outlier(imputedDS)
#########################Outlier Detection for Year 3 data######################
outLier <- diagnose_outlier(year3_data) 

plot_outlier(year3_data)

  
outlier_Data <- as.data.frame(sapply(year3_data[,-65],outlierTreament))
ImpDataSet <- missForest(outlier_Data)$ximp

summary(ImpDataSet)

diagnose_outlier(ImpDataSet)

outlier_Data_update <- as.data.frame(sapply(ImpDataSet,outlierTreament))
imputedDS <- knnImputation(outlier_Data_update)

diagnose_outlier(imputedDS)

var5 <- imputate_outlier(.data = imputedDS,xvar = "[(cash + short-term securities + receivables - short-term liabilities) / (operating expenses - depreciation)] * 365",
                 method = "capping")
var6 <- imputate_outlier(.data = imputedDS,xvar = "retained earnings / total assets",
                         method = "capping")
var7 <- imputate_outlier(.data = imputedDS,xvar = "sales (n) / sales (n-1)",
                         method = "capping")
var8 <- imputate_outlier(.data = imputedDS,xvar = " working capital",method = "capping")
var9 <- imputate_outlier(.data = imputedDS,xvar = " (sales - cost of products sold) / sales",method = "capping")

imputedDS$`[(cash + short-term securities + receivables - short-term liabilities) / (operating expenses - depreciation)] * 365` <- var5
imputedDS$`retained earnings / total assets` <- var6
imputedDS$`sales (n) / sales (n-1)` <- var7
imputedDS$` working capital` <- var8
imputedDS$` (sales - cost of products sold) / sales` <- var9

imputedDS <- cbind(imputedDS,bankruptcy = year3_data[,65])

diagnose_outlier(imputedDS)
plot_outlier(imputedDS)
###########################Transformation of the Year 2 data 
ImpDataSet_Year2 <- cbind(imputedDS_Year2,bankruptcy = year2_data[,65])
normalizeParams_Year2 <- preProcess(ImpDataSet_Year2,method = c("center","scale"))

transformedDataSet_year2 <- predict(normalizeParams_Year2,ImpDataSet_Year2)

summary(transformedDataSet_year2)

profiling_num(transformedDataSet)

plot_num(transformedDataSet_year2[,1:15])
plot_num(transformedDataSet_year2[,16:30])
plot_num(transformedDataSet_year2[,31:45])
plot_num(transformedDataSet_year2[,46:60])
plot_num(transformedDataSet_year2[,61:65])
###########################Transformation of the year 3 data ##############################
normalizeParams <- preProcess(imputedDS,method = c("center","scale","YeoJohnson"))

transformedDataSet <- predict(normalizeParams,imputedDS)

summary(transformedDataSet)

profiling_num(transformedDataSet)

plot_num(transformedDataSet[,1:15])
plot_num(transformedDataSet[,16:30])
plot_num(transformedDataSet[,31:45])
plot_num(transformedDataSet[,46:60])
plot_num(transformedDataSet[,61:65])

#########################Check for near zero variance for Year 2 data
nzv2 <- nearZeroVar(transformedDataSet_year2,saveMetrics = TRUE)
nzv2
#########################Check for near zero variance for year 3 data
nzv <- nearZeroVar(transformedDataSet,saveMetrics = TRUE)
nzv

#############################Correlation plot for year 2 data
tempDF_2 <- transformedDataSet_year2
as.numeric(levels(tempDF_2$bankruptcy))[tempDF_2$bankruptcy]
tempDF_2$bankruptcy <- as.numeric(levels(tempDF_2$bankruptcy))[tempDF_2$bankruptcy]
corMat <- cor(tempDF_2,method = "pearson")

corDF <- as.data.frame(corMat)

par(mfrow=c(2,2))
corrplot(cor(tempDF_2[,1:10],method = "pearson"))
corrplot(cor(tempDF_2[,11:20],method = "pearson"))
corrplot(cor(tempDF_2[,21:30],method = "pearson"))
corrplot(cor(tempDF_2[,31:40],method = "pearson"))
corrplot(cor(tempDF_2[,41:50],method = "pearson"))
corrplot(cor(tempDF_2[,51:60],method = "pearson"))
corrplot(cor(tempDF_2[,61:65],method = "pearson"))

#############################Correlation plot for year 3 data
tempDF <- transformedDataSet
as.numeric(levels(tempDF$bankruptcy))[tempDF$bankruptcy]
corMat <- cor(tempDF,method = "pearson")

corDF <- as.data.frame(corMat)

correlation_table(data = transformedDataSet,target = "bankruptcy")

par(mfrow=c(2,2))
corrplot(cor(tempDF[,1:10],method = "pearson"))
corrplot(cor(tempDF[,11:20],method = "pearson"))
corrplot(cor(tempDF[,21:30],method = "pearson"))
corrplot(cor(tempDF[,31:40],method = "pearson"))
corrplot(cor(tempDF[,41:50],method = "pearson"))
corrplot(cor(tempDF[,51:60],method = "pearson"))
corrplot(cor(tempDF[,61:65],method = "pearson"))

glimpse(tempDF)
#########################Data Splitting into train and test dataset before balancing dataset##########
inTraining_1 <- createDataPartition(transformedDataSet$bankruptcy, p = .8, list = FALSE)
training <- transformedDataSet[ inTraining_1,]
testing  <- transformedDataSet[-inTraining_1,]
summary(trainingDF)
############################Random Forest Model before balancing dataset################
fitRF_before <- caret::train(bankruptcy ~.,
                      data = training,
                      method = "rf",
                      trControl = ctrl,
                      allowParallel=TRUE,
                      tuneGrid=rfGrid)

#####################Performance Evaluation before balancing dataset##############
rfImpVar_bef <- varImp(fitRF_before)

plot(rfImpVar_bef, top = 30)


predict.rf_bef=predict(fitRF_before,testing, type ="prob")

forestpred_bef = prediction(predict.rf_bef[,2], testing$bankruptcy)

perf_bef <- performance(forestpred_bef,"tpr","fpr") 

plot(perf_bef,  main = "Plot of ROC Curve", colorize=TRUE, print.cutoffs.at = c(0.25,0.75)) + 
  abline(0, 1, col="red")

conf_matrix_bef <- confusionMatrix(fitRF_before$finalModel$predicted, fitRF_before$finalModel$y, positive="1")
roc(as.numeric(fitRF_before$finalModel$predicted),as.numeric(fitRF_before$finalModel$y))

##########################Check whether data is balanced or not for year 2#####################
summary(transformedDataSet_year2)
ggplot(data = tempDF_2,aes(x=bankruptcy)) + 
  geom_bar(aes(col=bankruptcy,fill = bankruptcy)) + 
  labs(title = "Frequency of Bankrupt variable")

bankruptDF_2 <- SMOTE(bankruptcy ~.,data = transformedDataSet_year2,perc.over = 1000,perc.under = 120,k = 10)


summary(bankruptDF_2)
Hmisc::describe(bankruptDF)

ggplot(data = bankruptDF_2,aes(x=bankruptcy)) + 
  geom_bar(aes(col=bankruptcy,fill = bankruptcy)) + 
  labs(title = "Frequency of Bankrupt variable")

##########################Check whether data is balanced or not for year 3###################
ggplot(data = transformedDataSet,aes(x=bankruptcy)) + 
  geom_bar(aes(col=bankruptcy,fill = bankruptcy)) + 
  labs(title = "Frequency of Bankrupt variable")

bankruptDF <- SMOTE(bankruptcy ~.,data = transformedDataSet,perc.over = 1000,perc.under = 120,k = 10)


summary(bankruptDF)
Hmisc::describe(bankruptDF)

ggplot(data = bankruptDF,aes(x=bankruptcy)) + 
  geom_bar(aes(col=bankruptcy,fill = bankruptcy)) + 
  labs(title = "Frequency of Bankrupt variable")

#########################Data Splitting into train and test dataset
#######after imputation and balancing for Year 2#####
inTraining_2 <- createDataPartition(bankruptDF_2$bankruptcy, p = .8, list = FALSE)
trainingDF_2 <- bankruptDF_2[inTraining_2,]
testingDF_2  <- bankruptDF_2[-inTraining_2,]
summary(trainingDF_2)

#########################Data Splitting into train and test dataset 
#######after imputation and balancing for Year 3#####
inTraining <- createDataPartition(bankruptDF$bankruptcy, p = .8, list = FALSE)
trainingDF <- bankruptDF[ inTraining,]
testingDF  <- bankruptDF[-inTraining,]
summary(trainingDF)

############################Random Forest Model for Year 2 data after all feature engineering######
fitRF_2 <- caret::train(bankruptcy ~.,
                        data = trainingDF_2,
                        method = "rf",
                        trControl = ctrl,
                        allowParallel=TRUE,
                        tuneGrid=rfGrid)

############################Random Forest Model for Year 3 data after all feature engineering######
fitRF <- caret::train(bankruptcy ~.,
                      data = trainingDF,
                      method = "rf",
                      trControl = ctrl,
                      allowParallel=TRUE,
                      tuneGrid=rfGrid)

#####################Performance Evaluation for Year 2 data after all feature engineering######
rfImpVar_2 <- varImp(fitRF_2)

summary(fitRF_2)

plot(rfImpVar_2, top = 30)

predict.rf_2 <- predict(fitRF_2,testingDF_2, type ="prob")

forestpred_2 = prediction(predict.rf_2[,2], testingDF_2$bankruptcy)

perf_2 <- performance(forestpred_2,"tpr","fpr") 

plot(perf_2,  main = "Plot of ROC Curve", colorize=TRUE, print.cutoffs.at = c(0.25,0.75)) + 
  abline(0, 1, col="red")

conf_matrix_2 <- confusionMatrix(fitRF_2$finalModel$predicted, fitRF_2$finalModel$y, positive="1")  

TN_2 <- conf_matrix_2$table[1,1]
TP_2 <- conf_matrix_2$table[2,2]
FN_2 <- conf_matrix_2$table[1,2]
FP_2 <- conf_matrix_2$table[2,1]

Precision_2  <- TP_2 /(TP_2 +FP_2 )

Recall_2  <- TP_2 /(TP_2 +FN_2 )

F1_score_2  <- 2*Precision_2 *Recall_2 /(Precision_2 +Recall_2)

GAINSCHART <- as.ggplot(function() plot(performance(forestpred_2,"tpr","rpp"), main = "Plot of Gains Chart",colorize=TRUE))

LIFTCHART <- as.ggplot(function() plot(performance(forestpred_2,"lift","rpp"), main = "Plot of Lift Chart", colorize=TRUE))

RF2_roc <- roc(as.numeric(fitRF_2$finalModel$predicted),as.numeric(fitRF_2$finalModel$y))
plot(RF2_roc)
auc(RF2_roc)

#####################Performance Evaluation for Year 3 data after all feature engineering######
rfImpVar <- varImp(fitRF)
summary(fitRF)

impDF <- as.data.frame(fitRF$finalModel$importance)
plot(rfImpVar, top = 30)

predict.rf=predict(fitRF,testingDF, type ="prob")

forestpred = prediction(predict.rf[,2], testingDF$bankruptcy)

perf <- performance(forestpred,"tpr","fpr") 

plot(perf,  main = "Plot of ROC Curve", colorize=TRUE, print.cutoffs.at = c(0.25,0.75)) + 
  abline(0, 1, col="red")

conf_matrix<- confusionMatrix(fitRF$finalModel$predicted, fitRF$finalModel$y, positive="1")  

TN <- conf_matrix$table[1,1]
TP <- conf_matrix$table[2,2]
FN <- conf_matrix$table[1,2]
FP <- conf_matrix$table[2,1]

Precision <- TP/(TP+FP)

Recall <- TP/(TP+FN)

F1_score <- 2*Precision*Recall/(Precision+Recall)

GAINSCHART = as.ggplot(function() plot(performance(forestpred,"tpr","rpp"), main = "Plot of Gains Chart",colorize=TRUE))

LIFTCHART = as.ggplot(function() plot(performance(forestpred,"lift","rpp"), main = "Plot of Lift Chart", colorize=TRUE))

RF3_roc <- roc(as.numeric(fitRF$finalModel$predicted),as.numeric(fitRF$finalModel$y))
auc(RF3_roc)

################################decision Tree of year 2 data############################
fitCart_2 <- rpart(data=trainingDF_2, 
                   bankruptcy ~.)
fitTreeParty_2 <-as.party(fitCart_2)
plot(fitTreeParty_2)
plotcp(fitCart_2)

fitCart_2$variable.importance
predict.cart_2 <- predict(fitCart_2,testingDF_2, type ="class")
predict.cart_2_prob <- predict(fitCart_2,testingDF_2, type ="prob")

#prune tree
pfitCart1_2 <-prune(fitCart_2,cp=0.011)
fancyRpartPlot(pfitCart1_2)

Cartpred_2 <- prediction(predict.cart_2_prob[,2], testingDF_2$bankruptcy)

Cartperf_2 <- performance(Cartpred_2,"tpr","fpr") 

plot(Cartperf_2,  main = "Plot of ROC Curve", colorize=TRUE, print.cutoffs.at = c(0.25,0.75)) + 
  abline(0, 1, col="red")

conf_matrix_cart_2 <- confusionMatrix(predict.cart_2,testingDF_2$bankruptcy, positive="1")  

TN_cart_2 <- conf_matrix_cart_2$table[1,1]
TP_cart_2 <- conf_matrix_cart_2$table[2,2]
FN_cart_2 <- conf_matrix_cart_2$table[1,2]
FP_cart_2 <- conf_matrix_cart_2$table[2,1]

Precision_cart_2  <- TP_cart_2 /(TP_cart_2 +FP_cart_2 )

Recall_cart_2  <- TP_cart_2 /(TP_cart_2 +FN_cart_2 )

F1_score_cart_2  <- 2*Precision_cart_2 *Recall_cart_2 /(Precision_cart_2 + Recall_cart_2)
cart2_roc <- roc(as.numeric(predict.cart_2),as.numeric(testingDF_2$bankruptcy))
plot(cart2_roc)
auc(cart2_roc)


################################decision Tree of year 3 data############################
dev.off()
fitCart <- rpart(data=trainingDF, bankruptcy ~.)
fitTreeParty<-as.party(fitCart)
plot(fitTreeParty)
plotcp(fitCart)

#prune tree
pfitCart1<-prune(fitCart,cp=0.012)
fancyRpartPlot(pfitCart1)

predict.cart <- predict(fitCart,testingDF, type ="class")
predict.cart_prob <- predict(fitCart,testingDF, type ="prob")

Cartpred <- prediction(predict.cart_prob[,2], testingDF$bankruptcy)

Cartperf <- performance(Cartpred,"tpr","fpr") 

plot(Cartperf,  main = "Plot of ROC Curve", colorize=TRUE, print.cutoffs.at = c(0.25,0.75)) + 
  abline(0, 1, col="red")

conf_matrix_cart <- confusionMatrix(predict.cart,testingDF$bankruptcy, positive="1")  

TN_cart <- conf_matrix_cart$table[1,1]
TP_cart <- conf_matrix_cart$table[2,2]
FN_cart <- conf_matrix_cart$table[1,2]
FP_cart <- conf_matrix_cart$table[2,1]

Precision_cart  <- TP_cart /(TP_cart +FP_cart )

Recall_cart  <- TP_cart /(TP_cart +FN_cart )

F1_score_cart  <- 2*Precision_cart *Recall_cart /(Precision_cart + Recall_cart)

cart3_roc <- roc(as.numeric(predict.cart),as.numeric(testingDF$bankruptcy))
auc(cart3_roc)

#######################NNet for year 2 data###############
nnetGrid <- expand.grid(.size = 1:10, .decay = c(0, .1, 1, 2))
maxSize <- max(nnetGrid$.size)
reducedSet <- names(transformedDataSet_year2)
reducedSet <- reducedSet[-65]
reducedSet
numWts <- 1*(maxSize * (length(reducedSet) + 1) + maxSize + 1)

set.seed(476)

ctrl <- trainControl(method = "repeatedcv",
                     repeats = 10,
                     search = "random",
                     savePredictions = TRUE)

nnetFit <- train(x = trainingDF_2[,reducedSet],
                 y = trainingDF_2$bankruptcy,
                 method = "nnet",
                 tuneGrid = nnetGrid,
                 trace = FALSE,
                 maxit = 20,
                 MaxNWts = numWts,
                 trControl = ctrl)

predNN <- predict(nnetFit,testingDF_2,type = "prob")
predNN_class <- predict(nnetFit,testingDF_2,type = "raw")

nnImpVar <- varImp(nnetFit)

summary(nnetFit)
nnetFit$finalModel

plot(nnImpVar, top = 30)

nnetpred <- prediction(predNN[,2], testingDF_2$bankruptcy)

nnetperf <- performance(nnetpred,"tpr","fpr") 

plot(nnetperf,  main = "Plot of ROC Curve", colorize=TRUE, print.cutoffs.at = c(0.25,0.75)) + 
  abline(0, 1, col="red")

conf_matrix_nnet <- confusionMatrix(as.factor(predNN_class), as.factor(testingDF_2$bankruptcy), positive="1")  

TN_nnet <- conf_matrix_nnet$table[1,1]
TP_nnet <- conf_matrix_nnet$table[2,2]
FN_nnet <- conf_matrix_nnet$table[1,2]
FP_nnet <- conf_matrix_nnet$table[2,1]

Precision_nnet <- TP_nnet/(TP_nnet+FP_nnet)

Recall_nnet <- TP_nnet/(TP_nnet+FN_nnet)

F1_score_nnet <- 2*Precision_nnet*Recall_nnet/(Precision_nnet+Recall_nnet)

plot(nnetFit, rep = "best")


######################################################################################
# generating train and test datasets from the 2nd and 3rd year data
set.seed(1)  
syear2 <- sample(dim(year2_data )[1], 7500)
datayear2train <- year2_data [syear2, ]
datayear2test <- year2_data [-syear2, ]


syear3 <- sample(dim(year3_data)[1], 7500)
datayear3train <- year3_data[syear3, ]
datayear3test <- year3_data[-syear3, ]



#generating dataset for boosting

dataforboostingyear2 <-datayear2train[,-65]
datamatrixyear2 <- as.matrix(dataforboostingyear2)
datmatrixyear2test <-as.matrix(datayear2test[,-65])

x_numyear2 <- as.numeric(as.character(datayear2train$bankruptcy))
as.factor(x_numyear2)

x_numyeartest2 <- as.numeric(as.character(datayear2test$bankruptcy))
as.factor(x_numyeartest2)

dataforboostingyear3 <-datayear3train[,-65]
datamatrixyear3 <- as.matrix(dataforboostingyear3)
datmatrixyear3test <-as.matrix(datayear3test[,-65])

x_numyear3 <- as.numeric(as.character(datayear3train$bankruptcy))
as.factor(x_numyear3)

x_numyeartest3 <- as.numeric(as.character(datayear3test$bankruptcy))
as.factor(x_numyeartest3)


#Extreme Gradident Boosting Model
library(xgboost)

xgboost_model <- xgboost(data = datamatrixyear2, 
                         label = x_numyear2,
                         max_depth = 3, 
                         objective = "binary:logistic", 
                         nrounds = 10, 
                         verbose = FALSE,
                         prediction = TRUE)


predxgboost1<-predict(xgboost_model, 
                      datamatrixyear2)


dtrainyear2 <- xgb.DMatrix(datamatrixyear2, 
                           label = x_numyear2)

params <- list(max_depth = 10, 
               objective = "binary:logistic",
               silent = 0)


bst_modelyear2 <- xgb.train(params = params, 
                            data = dtrainyear2, 
                            nrounds = 10, 
                            nfold=5, metrics = 
                              list("rmse","auc"),
                            verbose = FALSE,
                            prediction = TRUE)



predbst_modelyear2<-predict(bst_modelyear2, 
                            datamatrixyear2)


predbst_modeltestyear2<-predict(bst_modelyear2, 
                                datmatrixyear2test)

#Year 3 Results

dtrainyear3 <- xgb.DMatrix(datamatrixyear3, 
                           label = x_numyear3)

params <- list(max_depth = 10, 
               objective = "binary:logistic",
               silent = 0)


bst_modelyear3 <- xgb.train(params = params, 
                            data = dtrainyear3, 
                            nrounds = 10, 
                            nfold=5, metrics = 
                              list("rmse","auc"),
                            verbose = FALSE,
                            prediction = TRUE)



predbst_modelyear3<-predict(bst_modelyear3, 
                            datamatrixyear3)


predbst_modeltestyear3<-predict(bst_modelyear3, 
                                datmatrixyear3test)


#Data with Only Complete Cases

dataformarsyear2<- datayear2train[complete.cases(datayear2train), ]
dataformarsyear3<- datayear3train[complete.cases(datayear3train), ]
x_numlogregyear2 <- as.numeric(as.character(dataformarsyear2$bankruptcy))
x_numlogregyear3 <- as.numeric(as.character(dataformarsyear3$bankruptcy))

dataformarstestyear2<- datayear2test[complete.cases(datayear2test), ]
dataformarstestyear3<- datayear3test[complete.cases(datayear3test), ]
x_numlogregyeartest2 <- as.numeric(as.character(dataformarstestyear2$bankruptcy))
x_numlogregyeartest3 <- as.numeric(as.character(dataformarstestyear3$bankruptcy))


#Logistic Regression Model

fityear2 <- glm(data=dataformarsyear2, bankruptcy~.,
                family="binomial")

summary(fityear2)
vif(fityear2)

predlogregyear2 <- predict(fityear2, new= dataformarsyear2, type = "response")

predlogregyeartest2 <- predict(fityear2, new= dataformarstestyear2, type = "response")

fityear3 <- glm(data=dataformarsyear3, bankruptcy~.,
                family="binomial")

predlogregyear3 <- predict(fityear3, new= dataformarsyear3, type = "response")

predlogregyeartest3 <- predict(fityear3, new= dataformarstestyear3, type = "response")


summary(fityear3)
vif(fityear3)

# Mars Models

marsyear2 <-earth(bankruptcy ~., 
                  data = dataformarsyear2,
                  glm=list(family=binomial),
                  degree = 3,       
                  nprune = 5)


summary(marsyear2)

predmarsyear2 <- predict(marsyear2, dataformarsyear2, type = "response")
predmarsyear2 <-as.numeric(predmarsyear2)

predmarsyear2test <- predict(marsyear2, new= dataformarstestyear2, type = "response")

predmarsyear2test <-as.numeric(predmarsyear2test )

marsyear3 <-earth(bankruptcy ~., 
                  data = dataformarsyear3,
                  glm=list(family=binomial),
                  degree = 3,       
                  nprune = 5)

predmarsyear3 <- predict(marsyear3, dataformarsyear3, type = "response")
predmarsyear3 <-as.numeric(predmarsyear3)

predmarsyear3test <- predict(marsyear3, new= dataformarstestyear3, type = "response")

predmarsyear3test <-as.numeric(predmarsyear3test )


#LDA Modeling

#Year2 Results

preproc.param <- dataformarsyear2%>% preProcess(method = c("center", "scale")) 
preproc.param
transformed2 <- preproc.param %>% predict(dataformarsyear2)  
head (transformed2,5)
lda.model <- lda(bankruptcy~., data = transformed2) 
lda.model

predictions2 <- lda.model %>% predict(transformed2) 

names(predictions2)

head(predictions2$x,100)
tail(predictions2$posterior,5) 
tail(predictions2$class,5)


table(Original=dataformarsyear2$bankruptcy,Predicted=predictions2$class)
conf_matrix<- confusionMatrix(predictions2$class, dataformarsyear2$bankruptcy, positive="1")

r1<-roc(dataformarsyear3$bankruptcy,predictions$posterior[,2]) 


LDApred = prediction(predictions2$posterior[,2], dataformarsyear2$bankruptcy)

perf <- performance(LDApred,"tpr","fpr") 


plot(perf,  main = "Plot of ROC Curve", print.cutoffs.at = c(0.25,0.75))+ abline(0, 1, col="red")


TN <- conf_matrix$table[1,1]
TP <- conf_matrix$table[2,2]
FN <- conf_matrix$table[1,2]
FP <- conf_matrix$table[2,1]

Precision <- TP/(TP+FP)

Recall <- TP/(TP+FN)

F1_score = 2*Precision*Recall/(Precision+Recall)


#Year3 Results

preproc.param <- dataformarsyear3%>% preProcess(method = c("center", "scale")) 
preproc.param
transformed3 <- preproc.param %>% predict(dataformarsyear3)  
head (transformed3,5)
lda.model <- lda(bankruptcy~., data = transformed3) 
lda.model

predictions3 <- lda.model %>% predict(transformed3) 

names(predictions3)

head(predictions3$x,100)
tail(predictions3$posterior,5) 
tail(predictions3$class,5)


table(Original=dataformarsyear3$bankruptcy,Predicted=predictions3$class)
conf_matrix<- confusionMatrix(predictions3$class, dataformarsyear3$bankruptcy, positive="1")




r1<-roc(dataformarsyear3$bankruptcy,predictions3$posterior[,2]) 


LDApred = prediction(predictions$posterior[,2], dataformarsyear3$bankruptcy)

perf <- performance(LDApred,"tpr","fpr") 


plot(perf,  main = "Plot of ROC Curve", print.cutoffs.at = c(0.25,0.75))+ abline(0, 1, col="red")


TN <- conf_matrix$table[1,1]
TP <- conf_matrix$table[2,2]
FN <- conf_matrix$table[1,2]
FP <- conf_matrix$table[2,1]

Precision <- TP/(TP+FP)

Recall <- TP/(TP+FN)

F1_score = 2*Precision*Recall/(Precision+Recall)


#  PCA, Year 3 Provided

data2PCA <- dataformarsyear3

glimpse(data2PCA)

data2PCA <-data2PCA [,-64]

data2PCA.pca <- prcomp(data2PCA,scale=TRUE)

ggbiplot(data2PCA.pca,obs.scale = 1, var.scale = 1, 
         varname.size = 2, groups = dataformarsyear3$bankruptcy, labels.size=2, alpha = 0.1, ellipse = FALSE, circle = TRUE)+
  scale_x_continuous( limits = c(-16,12))+
  scale_y_continuous(limits = c(-16,12))+
  ggtitle("PCA Analyses for Full Dataset")+
  geom_point(aes(colour=dataformarsyear3$bankruptcy), size = 1)


ggscreeplot(data2PCA.pca, type = c("pev"))+
  ggtitle("Variance vs Principle Component No.")+
  theme_bw()+
  theme(panel.grid = element_blank())+ geom_point(size = 3) 

#source codes for Class Project

#ggbiplot source code

ggbiplot <- function(pcobj, choices = 1:2, scale = 1, pc.biplot = TRUE, 
                     obs.scale = 1 - scale, var.scale = scale, 
                     groups = NULL, ellipse = FALSE, ellipse.prob = 0.68, 
                     labels = NULL, labels.size = 3, alpha = 1, 
                     var.axes = TRUE, 
                     circle = FALSE, circle.prob = 0.69, 
                     varname.size = 3, varname.adjust = 1.5, 
                     varname.abbrev = FALSE, ...)
{
  library(ggplot2)
  library(plyr)
  library(scales)
  library(grid)
  
  stopifnot(length(choices) == 2)
  
  # Recover the SVD
  if(inherits(pcobj, 'prcomp')){
    nobs.factor <- sqrt(nrow(pcobj$x) - 1)
    d <- pcobj$sdev
    u <- sweep(pcobj$x, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$rotation
  } else if(inherits(pcobj, 'princomp')) {
    nobs.factor <- sqrt(pcobj$n.obs)
    d <- pcobj$sdev
    u <- sweep(pcobj$scores, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$loadings
  } else if(inherits(pcobj, 'PCA')) {
    nobs.factor <- sqrt(nrow(pcobj$call$X))
    d <- unlist(sqrt(pcobj$eig)[1])
    u <- sweep(pcobj$ind$coord, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- sweep(pcobj$var$coord,2,sqrt(pcobj$eig[1:ncol(pcobj$var$coord),1]),FUN="/")
  } else if(inherits(pcobj, "lda")) {
    nobs.factor <- sqrt(pcobj$N)
    d <- pcobj$svd
    u <- predict(pcobj)$x/nobs.factor
    v <- pcobj$scaling
    d.total <- sum(d^2)
  } else {
    stop('Expected a object of class prcomp, princomp, PCA, or lda')
  }
  
  # Scores
  choices <- pmin(choices, ncol(u))
  df.u <- as.data.frame(sweep(u[,choices], 2, d[choices]^obs.scale, FUN='*'))
  
  # Directions
  v <- sweep(v, 2, d^var.scale, FUN='*')
  df.v <- as.data.frame(v[, choices])
  
  names(df.u) <- c('xvar', 'yvar')
  names(df.v) <- names(df.u)
  
  if(pc.biplot) {
    df.u <- df.u * nobs.factor
  }
  
  # Scale the radius of the correlation circle so that it corresponds to 
  # a data ellipse for the standardized PC scores
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)
  
  # Scale directions
  v.scale <- rowSums(v^2)
  df.v <- r * df.v / sqrt(max(v.scale))
  
  # Change the labels for the axes
  if(obs.scale == 0) {
    u.axis.labs <- paste('standardized PC', choices, sep='')
  } else {
    u.axis.labs <- paste('PC', choices, sep='')
  }
  
  # Append the proportion of explained variance to the axis labels
  u.axis.labs <- paste(u.axis.labs, 
                       sprintf('(%0.1f%% explained var.)', 
                               100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))
  
  # Score Labels
  if(!is.null(labels)) {
    df.u$labels <- labels
  }
  
  # Grouping variable
  if(!is.null(groups)) {
    df.u$groups <- groups
  }
  
  # Variable Names
  if(varname.abbrev) {
    df.v$varname <- abbreviate(rownames(v))
  } else {
    df.v$varname <- rownames(v)
  }
  
  # Variables for text label placement
  df.v$angle <- with(df.v, (180/pi) * atan(yvar / xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar)) / 2)
  
  # Base plot
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
    xlab(u.axis.labs[1]) + ylab(u.axis.labs[2]) + coord_equal()
  
  if(var.axes) {
    # Draw circle
    if(circle) 
    {
      theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
      circle <- data.frame(xvar = r * cos(theta), yvar = r * sin(theta))
      g <- g + geom_path(data = circle, color = muted('white'), 
                         size = 1/2, alpha = 1/3)
    }
    
    # Draw directions
    g <- g +
      geom_segment(data = df.v,
                   aes(x = 0, y = 0, xend = xvar, yend = yvar),
                   arrow = arrow(length = unit(1/2, 'picas')), 
                   color = muted('red'))
  }
  
  # Draw either labels or points
  if(!is.null(df.u$labels)) {
    if(!is.null(df.u$groups)) {
      g <- g + geom_text(aes(label = labels, color = groups), 
                         size = labels.size)
    } else {
      g <- g + geom_text(aes(label = labels), size = labels.size)      
    }
  } else {
    if(!is.null(df.u$groups)) {
      g <- g + geom_point(aes(color = groups), alpha = alpha)
    } else {
      g <- g + geom_point(alpha = alpha)      
    }
  }
  
  # Overlay a concentration ellipse if there are groups
  if(!is.null(df.u$groups) && ellipse) {
    theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
    circle <- cbind(cos(theta), sin(theta))
    
    ell <- ddply(df.u, 'groups', function(x) {
      if(nrow(x) <= 2) {
        return(NULL)
      }
      sigma <- var(cbind(x$xvar, x$yvar))
      mu <- c(mean(x$xvar), mean(x$yvar))
      ed <- sqrt(qchisq(ellipse.prob, df = 2))
      data.frame(sweep(circle %*% chol(sigma) * ed, 2, mu, FUN = '+'), 
                 groups = x$groups[1])
    })
    names(ell)[1:2] <- c('xvar', 'yvar')
    g <- g + geom_path(data = ell, aes(color = groups, group = groups))
  }
  
  # Label the variable axes
  if(var.axes) {
    g <- g + 
      geom_text(data = df.v, 
                aes(label = varname, x = xvar, y = yvar, 
                    angle = angle, hjust = hjust), 
                color = 'darkred', size = varname.size)
  }
  # Change the name of the legend for groups
  # if(!is.null(groups)) {
  #   g <- g + scale_color_brewer(name = deparse(substitute(groups)), 
  #                               palette = 'Dark2')
  # }
  
  # TODO: Add a second set of axes
  
  return(g)
}

#ggscreeplot source code

ggscreeplot <- function(pcobj, type = c('pev', 'cev')) 
{
  type <- match.arg(type)
  d <- pcobj$sdev^2
  yvar <- switch(type, 
                 pev = d / sum(d), 
                 cev = cumsum(d) / sum(d))
  
  yvar.lab <- switch(type,
                     pev = 'proportion of explained variance',
                     cev = 'cumulative proportion of explained variance')
  
  df <- data.frame(PC = 1:length(d), yvar = yvar)
  
  ggplot(data = df, aes(x = PC, y = yvar)) + 
    xlab('principal component number') + ylab(yvar.lab) +
    geom_point() + geom_path()
}

# performance metrics for model evaluation - generic model function

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

results1 <-Model_Function2( x_numyeartest2  , predbst_modeltestyear2)

results1$conf_matrix
