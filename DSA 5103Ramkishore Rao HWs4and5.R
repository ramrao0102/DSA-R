
# Ramkishore Rao:  HWs 4 and 5

library(scatterplot3d)
library(rgl)
library(geometry)
library(MASS)
library(caret)

library(ggplot2)
library(tidyverse)
library(dplyr)
library(vctrs)
library(mlbench)
library("Rtsne")
library(forcats)
library(InformationValue)
library(lubridate)
library(outliers)
library(mice)
library(VIM)
library(EnvStats)
library(glmnet)
library(car)
library(stats)
library(earth)
library(htmlTable)

# Read Train File
file <- 'c:/Data Science and Analytics/DSA5103/Homeworks/2020-5013-hw4/Train.csv'

train <-read.csv(file)
glimpse(train)
dfTrain <- train # creation of a data frame for use in analysis

#Missing Values Visualization

md.pairs(dfTrain)
md.pattern(dfTrain)    
a<-aggr(dfTrain)
summary(a)

#Visualization with Complete Cases

trialtrain<-dfTrain[!complete.cases(dfTrain),]
trainfull<-na.omit(dfTrain)
class(trainfull)
nrow(trainfull)

# Conversion of character variables to factor varaibles, train set

dfTrain$channelGrouping = as.factor(dfTrain$channelGrouping)
dfTrain$broswer = as.factor(dfTrain$browser)
dfTrain$operatingSystem = as.factor(dfTrain$operatingSystem)
dfTrain$deviceCategory = as.factor(dfTrain$deviceCategory)
dfTrain$continent = as.factor(dfTrain$continent)
dfTrain$subContinent = as.factor(dfTrain$subContinent)
dfTrain$country = as.factor(dfTrain$country)
dfTrain$medium = as.factor(dfTrain$medium)

# Creation of ggplots for Target Variable vs Predictor Variables

ggplot(data = dfTrain) +
  geom_point(mapping = aes (x = pageviews, y= revenue, color = continent))+
  ggtitle("Plot of Customer Revenue vs Customer Page Views")

ggplot(data = dfTrain) +
  geom_point(mapping = aes (x = continent, y= revenue, color = medium))+
  ggtitle("Plot of Customer Revenue vs Continent Continent")

dfTrain$newVisits1 <- as.factor(dfTrain$newVisits)
ggplot(data = dfTrain) +
  geom_point(mapping = aes (x = pageviews, y= revenue, color = newVisits1 ))+
  ggtitle("Plot of Customer Revenue vs Customer Page Views")

ggplot(data = dfTrain) +
  geom_point(mapping = aes (x = pageviews, y= revenue, color = medium))+
  ggtitle("Plot of Customer Revenue vs Customer Page Views")

# Creation of Histograms and Box Plots

par(mfrow=c(3,2))

hist(dfTrain$revenue, xlab = "Revenue", ylab = "Frequency", main = "Revenue Histogram", col ="blue")  # distribution before transformation
hist(dfTrain$pageviews, xlab = "Pageviews", ylab = "Frequency", main = "Page Views Histogram", col ="yellow")       
hist(dfTrain$newVisits, xlab = "New Visits", ylab = "Frequency", main = "New Visits Histogram", col ="green")
hist(dfTrain$bounces, xlab = "Bounces", ylab = "Frequency", main = "Bounces Histogram", col ="orange")

# boxplot before transformation

     
boxplot(dfTrain$revenue, xlab = "Revenue", ylab = "Values", 
        main = "Revenue Box Plot")    
boxplot(dfTrain$pageviews, xlab = "Pageviews", ylab = "Values", 
        main = "Pageview Box Plot") 


# Estimation of Outliers in Select Variables

outlier(dfTrain$revenue)
grubbs.test(dfTrain$revenue)
dfTrain[dfTrain$revenue==outlier(dfTrain$revenue),]
meanrevenue <- mean(dfTrain$revenue, na.rm = TRUE)
sdrevenue<-3*sd(dfTrain$revenue, na.rm =TRUE)
count(filter(dfTrain, revenue >(meanrevenue+sdrevenue)))

outlier(dfTrain$pageviews)
grubbs.test(dfTrain$pageviews)
dfTrain[dfTrain$pageviews==outlier(dfTrain$pageviews),] 
dfTrain[dfTrain$custId == 25964 & dfTrain$pageviews == 469,]
pageviews <-na.omit(dfTrain$pageviews)
meanpageviews <- mean(pageviews)
sdpageviews<-3*sd(pageviews)
count(filter(dfTrain, pageviews >(meanpageviews+sdpageviews)))

# Imputing Bounces and New Visits, Train Dataset

dfTrain$newVisits[is.na(dfTrain$newVisits)] <-0
dfTrain$bounces[is.na(dfTrain$bounces)] <-0

# Imputing PageViews, Train Dataset

missing <- is.na(dfTrain$pageviews)
sum(missing)
dfTrain$missing <- missing

dfTrainMean.imp<-dfTrain  #copy of the data with missings
class(dfTrainMean.imp$pageviews)
dfTrainMean.imp[missing,"pageviews"]<-mean(dfTrainMean.imp$pageviews,na.rm=T)
missing1 <- is.na(dfTrainMean.imp$pageviews)
sum(missing1)

#Impute Based on missing on categories
#Imputing values for continent, Only Imputing if found for that customer
dfTrain$continent[dfTrain$custId == 61056] <- 'Asia'
dfTrain$continent[dfTrain$custId == 86024] <- 'Asia'

#Imputing values for subContinent, , Only Imputing if found for that customer
dfTrain$subContinent[dfTrain$custId == 61056] <- 'Eastern Asia'
dfTrain$subContinent[dfTrain$custId == 86024] <- 'Southeast Asia'

#Imputing values for country, , Only Imputing if found for that customer
dfTrain$country[dfTrain$custId == 61056] <- 'Japan'
dfTrain$country[dfTrain$custId == 86024] <- 'Indonesia'

#Check to confirm if imputing worked for continent, 
# subcontinent, and country for Select Factor Variables

df <-filter(dfTrain, continent == "")
select(df,custId, continent)
b<-semi_join(dfTrain, df, by='custId')
select(b,custId, continent)

df <-filter(dfTrain, subContinent == "")
b<-semi_join(dfTrain, df, by='custId')
select(b,custId, subContinent)

df <-filter(dfTrain, country == "")
b<-semi_join(dfTrain, df, by='custId')
select(b,custId, country)

df <-filter(dfTrain, medium == "")
b<-semi_join(dfTrain, df, by='custId')
select(b,custId, medium)

df <-filter(dfTrain, operatingSystem  == "")
b<-semi_join(dfTrain, df, by='custId')
select(b,custId, operatingSystem)


ModelTrain<-dfTrainMean.imp # train dataset with imputations completed

# Aggregations at Customer Level

ModelTrain %>% group_by(country) %>% summarize(n=n()) %>% arrange(desc(n))

ModelTrain <- mutate(ModelTrain, country1 = fct_lump(fct_explicit_na(country), n=4))

SummaryTable <-ModelTrain %>% mutate(country1 = fct_lump(fct_explicit_na(country), n=4)) %>%
  group_by(country1) %>%
  summarize(n = n(),
            meanRev = round(mean(revenue),2),
            stdevRev = round(sd(revenue),2),
            meanpageviews = round(mean(pageviews),2),
            sdpageviews = round(sd(pageviews),2),
            totbounces = sum(bounces),
            totnewvisits = sum(newVisits))%>%
  arrange(desc(n))
knitr::kable(head(SummaryTable,5), "simple")

#Creating Aggregated Data Frame to be Used for Regression Modeling

abc1<-ModelTrain %>% group_by(custId) %>%
  summarise(sumRevenue = sum(revenue), sumviews = sum(pageviews), medium = last(medium), 
            device = last(deviceCategory), isTrueDirect = last(isTrueDirect),
            isMobile = last(isMobile), op = last(operatingSystem), bounces = last(bounces), 
            newvisit= last(newVisits), country = last(country1))

glimpse(abc1)

# facet wrapped histograms at the aggregated level

TrainforHist <-filter(abc1, sumRevenue <1000)
ggplot(data = TrainforHist, aes(sumRevenue)) +
  geom_histogram(col= "red", fill="blue", alpha =0.2, binwidth=50 )+
  facet_wrap(~device)

ggplot(data = TrainforHist, aes(sumRevenue)) +
  geom_histogram(col= "red", fill="blue", alpha =0.2, binwidth=50 )+
  facet_wrap(~country)

ggplot(data = TrainforHist, aes(sumRevenue)) +
  geom_histogram(col= "red", fill="blue", alpha =0.2, binwidth=50 )+
  facet_wrap(~op)

ggplot(data = TrainforHist, aes(sumRevenue)) +
  geom_histogram(col= "red", fill="blue", alpha =0.2, binwidth=50 )+
  facet_wrap(~newvisit)

ggplot(data = TrainforHist, aes(sumRevenue)) +
  geom_histogram(col= "red", fill="blue", alpha =0.2, binwidth=50 )+
  facet_wrap(~medium)

ggplot(data = TrainforHist, aes(sumRevenue)) +
  geom_histogram(col= "red", fill="blue", alpha =0.2, binwidth=50 )+
  facet_wrap(~bounces)

#Plots of Revenue vs Page Views, Aggregated Level

ggplot(data = abc1) +
  geom_point(mapping = aes (x = sumviews, y= log(sumRevenue+1), color = country), alpha = 0.6)+
  ggtitle("Plot of Customer Revenue vs Customer Page Views")

ggplot(data = abc1) +
  geom_point(mapping = aes (x = sumviews, y= log(sumRevenue+1), color = medium), alpha = 0.6)+
  ggtitle("Plot of Customer Revenue vs Customer Page Views")

# Feature Selection with PCA


abc1PCA<-abc1[sample(nrow(abc1), 2000), ]
abc1PCA <-abc1PCA[,c(2:11)]
abc1PCA2 <- select (abc1PCA, sumRevenue, sumviews, isTrueDirect, isMobile,bounces,newvisit)
head(abc1PCA) 
abc1PCA2.pca <- prcomp(abc1PCA2,scale=TRUE)

ggbiplot(abc1PCA2.pca,obs.scale = 1, var.scale = 1, 
         varname.size = 2, groups = abc1PCA$country, labels.size=2, alpha = 0.1, ellipse = FALSE, circle = TRUE)+
  scale_x_continuous( limits = c(-5,10))+
  scale_y_continuous(limits = c(-4,4))+
  ggtitle("PCA Analyses for Train Dataset")+
  geom_point(aes(colour=abc1PCA$country), size = 1)


ggscreeplot(abc1PCA2.pca, type = c("pev"))+
  ggtitle("Variance vs Principle Component No.")+
  theme_bw()+
  theme(panel.grid = element_blank())+ geom_point(size = 3) 



#Feature Selection Continued with T-SNE Analyses


# TSNE Analyses

abc1TSNE<-abc1[sample(nrow(abc1),4000),]
abc1TSNE <-select(abc1TSNE, sumRevenue, sumviews, isTrueDirect, isMobile,bounces,newvisit,country)
abc1TSNE$country = as.numeric(abc1TSNE$country)
abc1TSNE_unique <-unique(abc1TSNE)
abc1TSNE_matrix <- as.matrix (abc1TSNE_unique)
tsne_out <- Rtsne(abc1TSNE_matrix ,pca=FALSE,perplexity=30,theta=0.0,max_iter = 2000, num_threads=6) # Run TSNE
abc1TSNE_unique$country = as.factor(abc1TSNE_unique$country)

f<-abc1TSNE_unique$country

f<-fct_recode(f,"Canada" = "1", "India" ="2", "United Kingdom" ="3", "United States" = "4", "other" = "5") 
abc1TSNE_unique$country<-f
df<-data.frame(x=tsne_out$Y[,1],y=tsne_out$Y[,2], country = abc1TSNE_unique$country)

ggplot(data=df,aes(x=x,y=y, group = country, color = country))+geom_point() +
  geom_point(aes(colour=country), size = 2) +
  ggtitle("T-SNE, Points - Country")


T-SNE #w/ocountry

abc2TSNE<-abc1[sample(nrow(abc1),4000),]
abc2TSNE <-select(abc2TSNE, sumRevenue, sumviews, isTrueDirect, isMobile,bounces,newvisit)
abc2TSNE_unique <-unique(abc2TSNE)
abc2TSNE_matrix <- as.matrix (abc2TSNE_unique)
tsne1_out <- Rtsne(abc2TSNE_matrix ,pca=FALSE,perplexity=30,theta=0.0,max_iter = 2000, num_threads=6) # Run TSNE
abc1TSNE_unique$country = as.factor(abc1TSNE_unique$country)
df1<-data.frame(x=tsne1_out$Y[,1],y=tsne1_out$Y[,2], sumviews = abc2TSNE_unique$sumviews)
ggplot(data=df1,aes(x=x,y=y, group = sumviews, color = sumviews))+geom_point() +
  geom_point(aes(colour=sumviews), size = 2) +
  ggtitle("T-SNE, Points - Page Views")+
  scale_colour_gradientn(colors=rainbow(7))

f1<-as.factor(abc2TSNE_unique$newvisit)
abc2TSNE_unique$newvisit<-f1

df2<-data.frame(x=tsne1_out$Y[,1],y=tsne1_out$Y[,2], newvisit = abc2TSNE_unique$newvisit)
ggplot(data=df2,aes(x=x,y=y, group = newvisit, color = newvisit))+geom_point() +
  geom_point(aes(colour=newvisit), size = 2) +
  ggtitle("T-SNE, Points - New Visits")

#Transformation on Selected Variables

boxcox(dfTrain$pageviews) 
boxcox(dfTrain$pageviews, optimize = TRUE, lambda=c(-3,3))
hist((dfTrain$pageviews**-0.2193707-1)/-0.2193707,xlab = "PageViews/w Lambda = -0.219", 
     ylab = "Frequency", main = "Pageviews Histogram", col ="blue")  


dfTrainforHist <-filter(dfTrain, revenue <2000)
boxcox(dfTrainforHist$revenue+1)
hist(log(dfTrainforHist$revenue), xlab = "Log Revenue", 
     ylab = "Frequency", main = "Revenue Histogram", col ="green")

# OLS, Linear Regression on Train File

mdl01<-lm(data=abc1, log(sumRevenue+1) ~log(sumviews+1)+bounces*device*log(sumviews+1)+newvisit*log(sumviews+1)*device+country*log(sumviews+1)+device*country*log(sumviews+1))
summary(mdl01)
plot(mdl01)


mdl02<-lm(data=abc11, log(sumRevenue+1) ~log(sumviews+1)+bounces*device*log(sumviews+1)+newvisit*log(sumviews+1)*device+country*log(sumviews+1)+device*country*log(sumviews+1))
summary(mdl02)

plot(mdl02)



# Running Diagnostics on OLS Model

ncvTest(mdl01)
vif(mdl01)

plot(mdl01$fitted.values,mdl01$residuals, col = "black", pch = 21, bg = "red")
qqnorm(mdl01$resid)
qqline(mdl01$resid)

plot(hatvalues(mdl01),col = "black", pch = 21, bg = "red") #index plot of leverages
abline(h=2*15/47239)

plot(rstandard(mdl01),col = "black", pch = 21, bg = "red") 


outlierTest(mdl01)

plot(cooks.distance(mdl01),rstudent(mdl01),col = "black", pch = 21, bg = "red")
influencePlot(mdl01)

abc11<-abc1[-c(45126, 35658, 3424,5942,12841,29768,17312,11599,30541,26801),]
summary(abc11$sumRevenue)
summary(abc11$sumviews)

# Mars ModeL on Train Data Frame with Outliers Removed

# Model for Comparison with Other Models
marsFit1<- earth(log(sumRevenue+1) ~log(sumviews+1)+medium +device+ isTrueDirect+isMobile+op+bounces+newvisit+country,
                 data=abc11,
                 degree=3,nk=50,pmethod="cv",nfold=5,ncross=5)

# Model used for Final Prediction

marsFit2<- earth(log(sumRevenue+1) ~sumviews+medium +device+ isTrueDirect+isMobile+op+bounces+newvisit+country,
                 data=abc11,
                 degree=2,nk=50,pmethod="cv",nfold=5,ncross=5)

marsFit3<- earth(log(sumRevenue+1) ~sumviews+medium +device+ isTrueDirect+isMobile+op+bounces+newvisit+country,
                 data=abc11,
                 degree=4,nk=50,pmethod="cv",nfold=5,ncross=5)

summary(marsFit1)
plot(marsFit1)

summary(marsFit2)
plot(marsFit2)


# Calculation of RMSE for MARS Model

res1<- marsFit1$residuals^2
avgres1<-mean(res1)
RMSEMarsFit1<-sqrt(avgres1)

res2<- marsFit2$residuals^2
avgres2<-mean(res2)
RMSEMarsFit2<-sqrt(avgres2)

res3<- marsFit3$residuals^2
avgres3<-mean(res3)
RMSEMarsFit3<-sqrt(avgres3)


# Lasso, Ridge, and Elastic Net

# Ridge Model with Lambda as Tuning Parameter

fitControl <- trainControl(method="cv",number=5)

ridgeGrid <- expand.grid(lambda=seq(0,.8,length=20))


ridgefit <- train(log(sumRevenue+1) ~log(sumviews+1)+bounces*device*log(sumviews+1)+newvisit*log(sumviews+1)*device+country*log(sumviews+1)+device*country*log(sumviews+1),
                  data=abc11,
                  method="ridge",
                  trControl=fitControl,
                  tuneGrid=ridgeGrid)
ridgefit
plot(ridgefit)

plot(ridgefit, metric = "Rsquared")

fmridge<-predict(ridgefit$finalModel, type = "coef")

fmridge$coefficients[60,]

# Lasso Model  

lassoGrid <- expand.grid(fraction=seq(0.7,1.0,length=16))

lassofit <- train(log(sumRevenue+1) ~log(sumviews+1)+bounces*device*log(sumviews+1)+newvisit*log(sumviews+1)*device+country*log(sumviews+1)+device*country*log(sumviews+1),
                  data=abc11,
                  method="lasso",
                  trControl=fitControl,
                  tuneGrid=lassoGrid)
lassofit

plot(lassofit)

lassofit$finalModel$lambda

plot(lassofit, metric = "Rsquared")


fmlasso<-predict(lassofit$finalModel, type = "coef")
fmlasso$fraction

fmlasso$coefficients[60,]

# Elastic Net

enetGrid <- expand.grid(lambda=seq(0,.7,length=15),
                        fraction=seq(0.45,.9,length=15))


fitenet <- train(log(sumRevenue+1) ~log(sumviews+1)+bounces*device*log(sumviews+1)+newvisit*log(sumviews+1)*device+country*log(sumviews+1)+device*country*log(sumviews+1),
             data=abc11,
             method="enet",
             trControl=fitControl,
             tuneGrid=enetGrid)

fitenet$results$Rsquared[196]
fitenet$results$RMSE[196]
fitenet$results$MAE[196]

fmelasticnet<-predict(fitenet$finalModel, type = "coef")

plot(fitenet)

plot(fitenet, metric = "Rsquared")

#Models with Glmnet Method
# Ridge, Tuning on Lambda

ridgefit1 <- train(
  log(sumRevenue+1) ~log(sumviews+1)+bounces*device*log(sumviews+1)+newvisit*log(sumviews+1)*device+country*log(sumviews+1)+device*country*log(sumviews+1), data = abc11, method = "glmnet",
  trControl=fitControl,
  tuneGrid = expand.grid(alpha = 0, lambda = seq(0,.8,length=20))
)

coef(ridgefit1$finalModel, ridgefit1$bestTune$lambda)

qplot(ridgefit1$results$lambda,ridgefit1$results$RMSE) 

ridgefit1

plot(ridgefit1)

# Lasso, Tuning on Lambda

lassofit1<- train(
  log(sumRevenue+1) ~log(sumviews+1)+bounces*device*log(sumviews+1)+newvisit*log(sumviews+1)*device+country*log(sumviews+1)+device*country*log(sumviews+1),data = abc11, method = "glmnet",
  trControl=fitControl,
  tuneGrid = expand.grid(alpha = 1, lambda = seq(0,.8,length=20))
)

coef(lassofit1$finalModel, lassofit1$bestTune$lambda)

qplot(lassofit1$results$lambda,lassofit1$results$RMSE) 


plot(lassofit1)


#Elastic Net, Tuning on Alpha and Lambda

fitenet1 <- train(log(sumRevenue+1) ~log(sumviews+1)+bounces*device*log(sumviews+1)+newvisit*log(sumviews+1)*device+country*log(sumviews+1)+device*country*log(sumviews+1),
             data=abc11,
             method="glmnet",
             trControl=fitControl,
             tuneLength=10)


coef(fitenet1$finalModel, fitenet1$bestTune$lambda)

fitenet1

plot(fitenet1)

plot(fitenet1, plotType="level")

# Read Test File

file1 <- 'c:/Data Science and Analytics/DSA5103/Homeworks/2020-5013-hw4/Test.csv'

test <-read.csv(file1)
test1<-test

# Conversion of character variables to factor variables
test1$channelGrouping = as.factor(test1$channelGrouping)
test1$broswer = as.factor(test1$browser)
test1$operatingSystem = as.factor(test1$operatingSystem)
test1$deviceCategory = as.factor(test1$deviceCategory)
test1$continent = as.factor(test1$continent)
test1$subContinent = as.factor(test1$subContinent)
test1$country = as.factor(test1$country)
test1$medium = as.factor(test1$medium)
levels(test1$subContinent)

test1$newVisits[is.na(test1$newVisits)] <-0
test1$bounces[is.na(test1$bounces)] <-0

missing <- is.na(test1$pageviews)
sum(missing)
test1$missing <- missing
test1[missing,"pageviews"]<-mean(test1$pageviews,na.rm=T)

test1 %>% group_by(country) %>% summarize(n=n()) %>% arrange(desc(n))

test1 <- mutate(test1, country1 = fct_lump(fct_explicit_na(country), n=4))

#Model Prediction
test11<-test1 %>% group_by(custId) %>%
  summarise(sumviews = sum(pageviews), medium = last(medium), device = last(deviceCategory), isTrueDirect = last(isTrueDirect),
            isMobile = last(isMobile), op = last(operatingSystem), bounces = last(bounces), newvisit= last(newVisits), country = last(country1))

pred<-predict(marsFit2,test11)
pred<-as.data.frame(pred)
head(pred)
submissionDf<-data.frame(custId=test11$custId, predRevenue=pred$`log(sumRevenue + 1)`)
submissionDf$predRevenue[submissionDf$predRevenue<0]<-0
hist(submissionDf$predRevenue, xlab = "Log(Revenue+1)", ylab = "Frequency", main = "Revenue Histogram", col ="blue")

write.csv(submissionDf, "c:/Data Science and Analytics/DSA5103/Homeworks/2020-5013-hw4/submissionfinal2.csv", row.names=FALSE)


# source code for PCA Analyses

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
