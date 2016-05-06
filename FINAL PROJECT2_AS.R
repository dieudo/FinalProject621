
data2 <- read.csv("~/Downloads/cleaned_May012016.csv")
#data<- read.csv("~/Downloads/Alex_Clean_April24.csv")
library(pander)
library(psych)
library(moments)  # ... for Skewness
library(ggplot2)  #...  for Graphics
library(pROC)     #...  for ROC
library(Matrix)   # ... for matrix operations
library(car)      # ... for ellipse plots
library(stats)    # ... for statistical operations
library(MASS)     # ... for Multivariate Normal Distribution
library(graphics) # ... for arrows
library(moments)  # ... for Skewness
require(boot)
library(lars)
library (leaps)
library(glmnet)

#setwd("/Users/alexandersatz/Documents/Cuny/IS621/groupProject/May4")

#data2 <- read.csv("cleaned_May012016.csv", stringsAsFactors=TRUE)
head(data2)
attach(data2)
#pander::pander(describe(data2))
# attach(data2)
# detach(data2)
missingVals <- sapply(data2, function(x) sum(is.na(x)))
pander::pander(missingVals)
pander::pander(names(data2))
# data2$TARGET<-as.numeric(TARGET)
# data2$MedianAge<-as.numeric(MedianAge) 
# data2$AVG_TEMP<-as.numeric(AVG_TEMP) 
# data2$PER_CAP_INC<-as.numeric(PER_CAP_INC) 
# data2$LATITUDE<-as.numeric(LATITUDE)
# data2$LONGITUDE<-as.numeric(LONGITUDE) 
# data2$Avg_Per_Unemp<-as.numeric(Avg_Per_Unemp)
# data2$CHGENPCT<-as.numeric(CHGENPCT)
# data2$JDGENPCT<-as.numeric(JDGENPCT)
# data2$ISGENPCT<-as.numeric(ISGENPCT) 
# data2$BUGENPCT<-as.numeric(BUGENPCT)
# data2$ZOGENPCT<-as.numeric(ZOGENPCT) 
# data2$HIGENPCT<-as.numeric(HIGENPCT)
# data2$NORELPCT<-as.numeric(NORELPCT)
#data2$OtherRelPCT<-as.numeric(OtherRelPCT)
#str(data2)

#imputting Train data for missing observations with mean
data2[is.na(data2)] <- mean(data2$AVG_TEMP,na.rm=TRUE)
data2[is.na(data2)] <- mean(data2$PER_CAP_INC,na.rm=TRUE)
data2[is.na(data2)] <- mean(data2$JDGENPCT,na.rm=TRUE)
data2[is.na(data2)] <- mean(data2$CHGENPCT,na.rm=TRUE)
data2[is.na(data2)] <- mean(data2$ISGENPCT,na.rm=TRUE)
data2[is.na(data2)] <- mean(data2$BUGENPCT,na.rm=TRUE)
data2[is.na(data2)] <- mean(data2$ZOGENPCT,na.rm=TRUE)
data2[is.na(data2)] <- mean(data2$HIGENPCT,na.rm=TRUE)
data2[is.na(data2)] <- mean(data2$NORELPCT,na.rm=TRUE)
data2[is.na(data2)] <- mean(data2$OtherRelPCT,na.rm=TRUE)

corel<-cor(data2[,3:ncol(data2)])
pander::pander(corel)

#Median,Mean,Variance,Standard Deviation
apply(data2[,3:ncol(data2)], 2, function(x) mean(x, na.rm=TRUE))
apply(data2[,3:ncol(data2)], 2, function(x) median(x, na.rm=TRUE))
apply(data2[,3:ncol(data2)], 2, function(x) sd(x, na.rm=TRUE))
apply(data2[,3:ncol(data2)], 2, function(x) var(x, na.rm=TRUE))

#par(mfrow=c(2,2) , oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)
#pander::pander(describe(TARGET))
#hist(TARGET,col="red")
#mtext("TARGET", side=1, outer=F, line=2, cex=0.8)
#boxplot(TARGET, col="red", pch=19)
#mtext("target", cex=0.8, side=1, line=2)
#hist(PER_CAP_INC,col="blue")
#mtext("Per cap Inc", side=1, outer=F, line=2, cex=0.8)
#hist(MedianAge,col="blue")
#mtext("Median Age", side=1, outer=F, line=2, cex=0.8)
#hist(AVG_TEMP,col="blue")
#mtext("AVG TEMP", side=1, outer=F, line=2, cex=0.8)
#hist(LATITUDE,col="blue")
#mtext("LATITUDE ", side=1, outer=F, line=2, cex=0.8)
#hist(LONGITUDE,col="blue")
#mtext("LONGITUDE", side=1, outer=F, line=2, cex=0.8)
#hist(Avg_Per_Unemp,col="blue")
#mtext("Avg Per Unempl", side=1, outer=F, line=2, cex=0.8)
#hist(CHGENPCT,col="blue")
#mtext("CHGENPCT", side=1, outer=F, line=2, cex=0.8)
#hist(JDGENPCT,col="blue")
#mtext("JDGENPCT", side=1, outer=F, line=2, cex=0.8)
#hist(ISGENPCT,col="blue")
#mtext("ISGENPCT", side=1, outer=F, line=2, cex=0.8)
#hist(BUGENPCT,col="blue")
#mtext("BUGENPCT", side=1, outer=F, line=2, cex=0.8)
#hist(ZOGENPCT,col="blue")
#mtext("ZOGENPCT", side=1, outer=F, line=2, cex=0.8)
#hist(HIGENPCT,col="blue")
#mtext("HIGENPCT", side=1, outer=F, line=2, cex=0.8)
#hist(NORELPCT,col="blue")
#mtext("NORELPCT", side=1, outer=F, line=2, cex=0.8)
hist(OtherRelPCT,col="blue")
mtext("OtherRelPCT", side=1, outer=F, line=2, cex=0.8)
par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)
boxplot(MedianAge, col="green", pch=19)
mtext("Median Age", cex=0.8, side=1, line=2)
boxplot(AVG_TEMP, col="blue", pch=19)
mtext("Average temparature", cex=0.8, side=1, line=2)
boxplot(LATITUDE, col="green", pch=19)
mtext("LATITUDE", cex=0.8, side=1, line=2)
boxplot(LONGITUDE, col="green", pch=19)
mtext("Longitude", cex=0.8, side=1, line=2)
boxplot(Avg_Per_Unemp, col="green", pch=19)
mtext("Average per Unemployment", cex=0.8, side=1, line=2)
boxplot(CHGENPCT, col="green", pch=19)
mtext("CHGENPCT", cex=0.8, side=1, line=2)
boxplot(JDGENPCT, col="green", pch=19)
mtext("JDGENPCT", cex=0.8, side=1, line=2)
boxplot(ISGENPCT, col="green", pch=19)
mtext("ISGENPCT", cex=0.8, side=1, line=2)
boxplot(BUGENPCT, col="green", pch=19)
mtext("BUGENPCT", cex=0.8, side=1, line=2)
boxplot(ZOGENPCT, col="green", pch=19)
mtext("ZOGENPCT", cex=0.8, side=1, line=2)
boxplot(HIGENPCT, col="green", pch=19)
mtext("HIGENPCT", cex=0.8, side=1, line=2)
boxplot(NORELPCT, col="green", pch=19)
mtext("NORELPCT", cex=0.8, side=1, line=2)
boxplot(OtherRelPCT, col="green", pch=19)
mtext("OtherRelPCT", cex=0.8, side=1, line=2)
par(mfrow=c(2,2))
plot(TARGET~MedianAge,col="blue")
mtext("Target vs Median Age", side=1, outer=F, line=2, cex=0.8)
plot(TARGET~MedianAge,col="blue")
mtext("Target vs Median Age", side=1, outer=F, line=2, cex=0.8)
plot(TARGET~AVG_TEMP,col="blue")
mtext("Target vs Average Temp", side=1, outer=F, line=2, cex=0.8)
plot(TARGET~PER_CAP_INC,col="blue")
mtext("Target vs Per_Cap_Inc", side=1, outer=F, line=2, cex=0.8)
plot(TARGET~LATITUDE,col="blue")
mtext("Target vs LATITUDE", side=1, outer=F, line=2, cex=0.8)
plot(TARGET~LONGITUDE,col="blue")
mtext("Target vs LONGITUDE", side=1, outer=F, line=2,cex=0.8)
plot(TARGET~Avg_Per_Unemp,col="blue")
mtext("Target vs Avg per Unempl", side=1, outer=F, line=2,cex=0.8)
plot(TARGET~CHGENPCT,col="blue")
mtext("Target vs CHGENPCT", side=1, outer=F, line=2,cex=0.8)
plot(TARGET~JDGENPCT,col="blue")
mtext("Target vs JDGENPCT", side=1, outer=F, line=2,cex=0.8)
plot(TARGET~ISGENPCT,col="blue")
mtext("Target vs ISGENPCT", side=1, outer=F, line=2,cex=0.8)
plot(TARGET~BUGENPCT,col="blue")
mtext("Target vs BUGENPCT", side=1, outer=F, line=2,cex=0.8)
plot(TARGET~ZOGENPCT,col="blue")
mtext("Target vs ZOGENPCT", side=1, outer=F, line=2,cex=0.8)
plot(TARGET~HIGENPCT,col="blue")
mtext("Target vs HIGENPCT", side=1, outer=F, line=2,cex=0.8)
plot(TARGET~NORELPCT,col="blue")
mtext("Target vs NORELPCT", side=1, outer=F, line=2,cex=0.8)
plot(TARGET~OtherRelPCT,col="blue")
mtext("Target  vs OtherRelPCT", side=1, outer=F, line=2,cex=0.8)

#########################################################################################
## The model with all data and predictors
data2['absLatitude'] <- abs(data2$LATITUDE)
model1<-lm(TARGET~.,data=data2[,3:ncol(data2)])
summary(model1)  ## Not a single predictor is significant, including whether alcohol is legal!
plot(model1)
####################################################################################################################
###################################################################################################################
###################################################################################################################
##### below is modeling data after removing those countries with a high ISGENPCT.
summary(data2)
data3 <- data2[data2[12] <0.8,]  ## data without large islam
data4 <- data2[data2[12] >0.8,]  ## data with only large islam

# function will split the data set into  training and testing sets
splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/2))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

#appling the function
splits <- splitdf(data3, seed=1306)

#Returns two data frames called trainset and testset
str(splits)

# There are 68 observation in the train and 68 observations in the test data frame
lapply(splits,nrow)

#view the first couple columns in each data frame
lapply(splits,head)

# save the training and testing sets as data frames
training <- splits$trainset
testing <- splits$testset



#Regression Model using all 17 predictors from Training Data Set without transformation
mod1<-lm(TARGET~MedianAge+AVG_TEMP+PER_CAP_INC
               +Avg_Per_Unemp+CHGENPCT+JDGENPCT
               +ISGENPCT+BUGENPCT+ZOGENPCT+HIGENPCT
               +NORELPCT+OtherRelPCT+Prohibited
               +SunniPCT+ShiaPCT+LATITUDE+LONGITUDE,data=training)
summary(mod1)  ## medianage, avgtemp, income, higen, and shia are all sign

mod.t<-lm(TARGET~MedianAge+AVG_TEMP+PER_CAP_INC
         +Avg_Per_Unemp+CHGENPCT+JDGENPCT
         +ISGENPCT+BUGENPCT+ZOGENPCT+HIGENPCT
         +NORELPCT+OtherRelPCT+Prohibited
         +SunniPCT+ShiaPCT+LATITUDE+LONGITUDE,data=testing)
summary(mod.t)  ## only unemployment is sig

### test and training set are too different.  Cannot have a test set.##################################

splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/1))  ## alter so that there is only a training set
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

#appling the function
splits <- splitdf(data3, seed=1306)


training <- splits$trainset
mod1<-lm(TARGET~MedianAge+AVG_TEMP+PER_CAP_INC
         +Avg_Per_Unemp+CHGENPCT+JDGENPCT
         +ISGENPCT+BUGENPCT+ZOGENPCT+HIGENPCT
         +NORELPCT+OtherRelPCT+Prohibited
         +SunniPCT+ShiaPCT+LATITUDE+LONGITUDE+absLatitude,data=training)
summary(mod1)  ## avgtemp, higen, and sunni, and shia are sign

coef(mod1)
confint(mod1)
par(mfrow = c(3,3))
plot(mod1);cor(training[,3:ncol(training)])


#Box Cox Transformation
## considering how weak the data is, and the box cox is near 1, I don't think a variable transformation is required.
Ac1=boxcox(glm(TARGET~MedianAge+AVG_TEMP+MedianAge+AVG_TEMP
               +LONGITUDE+Avg_Per_Unemp+CHGENPCT+JDGENPCT+ISGENPCT
               +BUGENPCT+ZOGENPCT+HIGENPCT+PER_CAP_INC+LATITUDE
               +NORELPCT+OtherRelPCT+Prohibited,data=data2), 
           plotit = TRUE,family="poisson",MLEQ=TRUE, 
           interp= TRUE, eps = 1/50, xlab = expression(lambda),
           ylab = "log-Likelihood")
 title(main="BoxCox Transformation")





#Scale and Center variables for Lasso.  TARGET is sqrt
ldata<-data.frame(
TARGET=scale(sqrt(training$TARGET)-mean(sqrt(training$TARGET)))/sd(sqrt(training$TARGET)),
MedianAge=scale(training$MedianAge-mean(training$MedianAge))/sd(training$MedianAge),
AVG_TEMP=scale(training$AVG_TEMP-mean(training$AVG_TEMP))/sd(training$AVG_TEMP),
PER_CAP_INC=scale(training$PER_CAP_INC-mean(training$PER_CAP_INC))/sd(training$PER_CAP_INC),
Avg_Per_Unemp=scale(training$Avg_Per_Unemp-mean(training$Avg_Per_Unemp))/sd(training$Avg_Per_Unemp),
CHGENPCT=scale(training$CHGENPCT-mean(training$CHGENPCT))/sd(training$CHGENPCT),
JDGENPCT=scale(training$JDGENPCT-mean(training$JDGENPCT))/sd(training$JDGENPCT),
ISGENPCT=scale(training$ISGENPCT-mean(training$ISGENPCT))/sd(training$ISGENPCT),
BUGENPCT=scale(training$BUGENPCT-mean(training$BUGENPCT))/sd(training$BUGENPCT),
ZOGENPCT=scale(training$ZOGENPCT-mean(training$ZOGENPCT))/sd(training$ZOGENPCT),
HIGENPCT=scale(training$HIGENPCT-mean(training$HIGENPCT))/sd(training$HIGENPCT),
NORELPCT=scale(training$NORELPCT-mean(training$NORELPCT))/sd(training$NORELPCT),
OtherRelPCT=scale(training$OtherRelPCT-mean(training$OtherRelPCT))/sd(training$OtherRelPCT),
Prohibited=scale(training$Prohibited-mean(training$Prohibited))/sd(training$Prohibited),
SunniPCT=scale(training$SunniPCT-mean(training$SunniPCT))/sd(training$SunniPCT),
ShiaPCT=scale(training$ShiaPCT-mean(training$ShiaPCT))/sd(training$ShiaPCT),
LATITUDE=scale(training$LATITUDE-mean(training$LATITUDE))/sd(training$LATITUDE),
absLatitude=scale(training$absLatitude-mean(training$absLatitude))/sd(training$absLatitude),
LONGITUDE=scale(training$LONGITUDE-mean(training$LONGITUDE))/sd(training$LONGITUDE)
)


#Leaps

regsubsets.out <-regsubsets(TARGET~MedianAge+AVG_TEMP+PER_CAP_INC
                            +Avg_Per_Unemp+CHGENPCT+JDGENPCT
                            +ISGENPCT+BUGENPCT+ZOGENPCT+HIGENPCT
                            +NORELPCT+OtherRelPCT+Prohibited
                            +SunniPCT+ShiaPCT+LATITUDE+LONGITUDE+absLatitude,data=ldata,
                            nbest = 1,       # 1 best model for each number of predictors
                            nvmax = NULL,    # NULL for no limit on number of variables
                            force.in = NULL, force.out = NULL,
                            method = "exhaustive")
regsubsets.out
summary.out<-summary(regsubsets.out)
as.data.frame(summary.out$outmat)
par(mfrow = c(1,1))
plot(regsubsets.out, scale = "adjr2", main = "Adjusted R^2")
plot(regsubsets.out,scale="r2", main = " R2")
plot(regsubsets.out,scale="Cp", main = " Cp")
plot(regsubsets.out, scale="bic", main = "BIC")  #we now have 3 predictors, unemployment, HIGENPCT, and LATITUDE
coef(regsubsets.out,10)




# 10-fold Cross validation with sqrt Validation
set.seed(11)
folds = sample(rep(1:10, length = nrow(data2)))
table(folds)



## Part 3: Apply Best Subset Selection using 10-fold Cross-Validation to select the number
# of predictors and then fit the least squares regression model using the "best" subset.
k <- 10
set.seed(1306)
folds <- sample(1:k, nrow(ldata), replace = TRUE)
cv.errors <- matrix(NA, k, 10, dimnames = list(NULL, paste(1:10)))

# Let's write our own predict method
predict.regsubsets <- function(object, newdata, id,...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars]%*%coefi
}



for (j in 1:k) {
  best.fit <- regsubsets(TARGET ~ ., data = ldata[folds != j, ], nvmax = 10)  ##finds best predictors
  for (i in 1:10) {
    pred <- predict(best.fit, ldata[folds == j, ], id = i)
    cv.errors[j, i] = mean((ldata$TARGET[folds == j] - pred)^2)
  }
}

# This gives us a 10x10 matrix, of which the (i, j)th element corresponds
# to the test MSE for the ith cross-validation fold for the best j-variable model
cv.errors
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
mean.cv.errors[6]

par(mfrow = c(1,2))
plot(mean.cv.errors, type = 'b', xlab = "Number of Predictors", ylab = "Mean CV Errors",
     main = "Best Subset Selection (10-fold CV)")
points(6, mean.cv.errors[6], col = "brown", cex = 2, pch = 20)

rmse.cv = sqrt(apply(cv.errors, 2, mean))
rmse.cv[6]
plot(rmse.cv, pch = 19, type = "b", xlab = "Number of Predictors", ylab = "RMSE CV",
     main = "Best Subset Selection (10-fold CV)")
points(6, rmse.cv[6], col = "blue", cex = 2, pch = 20)

# The cross-validation selects a 5 or 6-variable model, so we perform best subset
# selection on the training data set to get the best 5-variable model, since it is slightly simpler
reg.best <- regsubsets(TARGET ~ ., data = ldata, nvmax = 10)
coef(reg.best, 5)  #I guess this give the best six



## Part 5: Lasso model using 10-fold cross-validation to select that largest
# value of lambda s.t. the CV error is within 1 s.e. of the minimum

x.train <- as.matrix(dplyr::select(ldata, -TARGET))
y.train <- ldata$TARGET


par(mfrow = c(1,1))
grid <- 10^seq(4, -5, length = 100)
lasso.mod <- glmnet(x.train, y.train, alpha = 1, lambda = grid, thresh = 1e-12)
plot(lasso.mod, xvar = "lambda", label = TRUE)

set.seed(1306)
cv.out <- cv.glmnet(x.train, y.train, alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam                                       # Lambda = 0.2026 (leads to smallest CV error)
log(bestlam)
lasso.mod <- glmnet(x.train, y.train, alpha = 1, lambda = bestlam)
lasso.coef <- predict(lasso.mod, type = "coefficients", s = bestlam)[1:19,]
lasso.coef[lasso.coef != 0]  ## gives only intercept


largelam <- cv.out$lambda.1se
largelam                                      # Lambda = 4.791278 (largest lambda w/in 1 SE)
lasso.mod <- glmnet(x.train, y.train, alpha = 1, lambda = largelam)



# Here are the estimated coefficients
lasso.coef <- predict(lasso.mod, type = "coefficients", s = largelam)[1:19,]
lasso.coef[lasso.coef != 0]

######################################################################
## we use the 5 predictor model taken from best subset 10-fold cross validation
save(training,file="training_NotScaled.Rda")
save(ldata,file="training_Scaled.Rda")
#Then load it with:
#load("data.Rda")
mod1<-lm(sqrt(TARGET)~Avg_Per_Unemp+ISGENPCT+HIGENPCT+ShiaPCT+LATITUDE,data=training)
summary(mod1)  ## R^2 is 0.137.  Only 3 predictors appear significant with all data.

#The model without sqrt tranformation looks just as good.
mod2<-lm((TARGET)~Avg_Per_Unemp+ISGENPCT+HIGENPCT+ShiaPCT+LATITUDE,data=training)
summary(mod2) #R2 = .15, adj R2 = .12

#box cox predicts sqrt, but no transformation, model2 looks better
library(MASS)
boxcox(mod2, plotit=T)
boxcox(mod1, plotit=T, lambda=seq(-0.1,1,by=0.1))


coef(mod2)
confint(mod2)
par(mfrow = c(1,3))
plot(mod1)
plot(mod2)
vif(mod2)  #no colinearity

############################################################
## we have a working model (mod2)  This can be used
## to make predictions with. Albiet, the model is not very good
file.e <- read.csv("~/Downloads/RE__project/cities_eval.csv")
#file.e <- read.csv("cities_eval.csv", stringsAsFactors=TRUE)
head(file.e)

##Calculate CI intervals for 5-95%
values = 0
high = 0
low = 0
for (x in 1:nrow(file.e)){
  values[x] = (predict(mod2,new=file.e[x,-1],interval="prediction")[1] )
  high[x] = (predict(mod2,new=file.e[x,-1],interval="prediction")[3] )
  low[x] = (predict(mod2,new=file.e[x,-1],interval="prediction")[2] )
}

values
high 
low
##NYC is 34.4 with a SE of 12.9
## Standard errors (standard deviations) for the predicted values
SE = c(12.87753571,  13.37338776,	12.91096429	,12.90167347,	12.88736224,	12.90756633	,12.91061735	,12.94225,	12.96783163	,16.19041327	,12.9119949)


## ration city/NY


getCIRatio <- function(SDY, SDX, Y, X, T1, COV){
  VY = SDY*SDY
  VX = SDX*SDX
  X2 = X*X
  Y2 = Y*Y
  V = Y/X
  T2 = T1*T1
  Q = 1-T2*VX/X2
  C = V/Q
  SE.R =sqrt(VY-2*Y/X*COV+Y2/X2*VX-T2*VX/X2*(VY-COV^2/VX))/X/Q
  CI1 = C-T1*SE.R
  CI2 = C+T1*SE.R
  l1 = c(CI1, CI2)
  return (l1) 
}


## calculate CI for ratios, assuming COV is 0
high.r = 0
low.r = 0
ratios.NY = 0
for (x in 1:length(values)){
  COV = 0
  l1 = getCIRatio(SE[x], 12.9, values[x], 34.4, 1.96, COV)
  high.r[x] = l1[2]
  low.r[x] = l1[1]
  ratios.NY[x] = values[x]/34.4
}

high.r
low.r
ratios.NY




## calculate CI for ratios, assuming COV is 1
high.r = 0
low.r = 0
ratios.NY = 0
for (x in 1:length(values)){
  COV = 1
  l1 = getCIRatio(SE[x], 12.9, values[x], 34.4, 1.96, COV)
  high.r[x] = l1[2]
  low.r[x] = l1[1]
  ratios.NY[x] = values[x]/34.4
}

high.r
low.r
ratios.NY


## calculate CI for ratios, assuming COV is -1
high.r = 0
low.r = 0
ratios.NY = 0
for (x in 1:length(values)){
  COV = -1
  l1 = getCIRatio(SE[x], 12.9, values[x], 34.4, 1.96, COV)
  high.r[x] = l1[2]
  low.r[x] = l1[1]
  ratios.NY[x] = values[x]/34.4
}

high.r
low.r
ratios.NY


## calculate CI for ratios, assuming COV is 0, at 70% confidence 
## at 70% confidence ratio, we can state the mumbai drinks more than NYC
##  see http://www.mapsofindia.com/my-india/india/alcohol-consumption-in-india for some info
high.r = 0
low.r = 0
ratios.NY = 0
for (x in 1:length(values)){
  COV = -1
  l1 = getCIRatio(SE[x], 12.9, values[x], 34.4, 1.036, COV)
  high.r[x] = l1[2]
  low.r[x] = l1[1]
  ratios.NY[x] = values[x]/34.4
}

high.r
low.r
ratios.NY

