###############Tanner Smith
############### Question 2
library(tidyverse)
library(broom)
library(readr)
library(dplyr)
library(zoo)
library(psych)
library(ROCR)
library(corrplot)
library(car)
library(InformationValue)
library(pbkrtest)
library(car)
library(leaps)
library(MASS)
library(corrplot)
library(glm2)
library(aod)
library(glmnet)



batting=read.csv("C:/Users/tanne/OneDrive/Documents/batting.csv",header=T)



############## Part 1: Cleaning and Examining the Data ##########################################################################

############## Initial Data Cleaning and Formatting ##########################################################################
str(batting)
summary(batting)

## Converting string values to numerical values and then into percentages

batting$MarApr_BB. <- as.numeric(sub("%", "", batting$MarApr_BB.))
batting$MarApr_BB. <- batting$MarApr_BB * 0.01
batting$MarApr_K. <- as.numeric(sub("%", "", batting$MarApr_K.))
batting$MarApr_K. <- batting$MarApr_K. * 0.01
batting$MarApr_LD. <- as.numeric(sub("%", "", batting$MarApr_LD.))
batting$MarApr_LD. <- batting$MarApr_LD. * 0.01
batting$MarApr_GB. <- as.numeric(sub("%", "", batting$MarApr_GB.))
batting$MarApr_GB. <- batting$MarApr_GB. * 0.01
batting$MarApr_FB. <- as.numeric(sub("%", "", batting$MarApr_FB.))
batting$MarApr_FB. <- batting$MarApr_FB. * 0.01
batting$MarApr_IFFB. <- as.numeric(sub("%", "", batting$MarApr_IFFB.))
batting$MarApr_IFFB. <- batting$MarApr_IFFB. * 0.01
batting$MarApr_HR.FB <- as.numeric(sub("%", "", batting$MarApr_HR.FB))
batting$MarApr_HR.FB <- batting$MarApr_HR.FB * 0.01
batting$MarApr_O.Swing. <- as.numeric(sub("%", "", batting$MarApr_O.Swing.))
batting$MarApr_O.Swing. <- batting$MarApr_O.Swing. * 0.01
batting$MarApr_Z.Swing. <- as.numeric(sub("%", "", batting$MarApr_Z.Swing.))
batting$MarApr_Z.Swing. <- batting$MarApr_Z.Swing. * 0.01
batting$MarApr_Swing. <- as.numeric(sub("%", "", batting$MarApr_Swing.))
batting$MarApr_Swing. <- batting$MarApr_Swing. * 0.01
batting$MarApr_O.Contact. <- as.numeric(sub("%", "", batting$MarApr_O.Contact.))
batting$MarApr_O.Contact. <- batting$MarApr_O.Contact. * 0.01
batting$MarApr_Z.Contact. <- as.numeric(sub("%", "", batting$MarApr_Z.Contact.))
batting$MarApr_Z.Contact. <- batting$MarApr_Z.Contact. * 0.01
batting$MarApr_Contact. <- as.numeric(sub("%", "", batting$MarApr_Contact.))
batting$MarApr_Contact. <- batting$MarApr_Contact. * 0.01


#Check to make sure all intended numerical values are numeric
str(batting)

##########Part 1B: Data Visualization and checking for Volume ################

# Full-Season OBP 
par(mfrow=c(1,2))
hist(batting$FullSeason_OBP, col = "#A71930", xlab = "Full-Season-OBP", main = "Histogram of Full-Season OBP")
boxplot(batting$FullSeason_OBP, col = "#A71930", main = "Boxplot of Full-Season OBP")
par(mfrow = c(1,1))
# Plate Appearances and At-Bats
par(mfrow=c(2,2))
hist(batting$MarApr_PA, col = "#A71930", xlab = "Plate Apperances", main = "Histogram of March-April PA")
hist(batting$MarApr_AB, col = "#09ADAD", xlab = "At Bats", main = "Histogram of March-April AB")
boxplot(batting$MarApr_PA, col = "#A71930", main = "Boxplot of March-April PA")
boxplot(batting$MarApr_AB, col = "#09ADAD", main = "Boxplot of March-April AB")
par(mfrow=c(1,1))
# Hits and Home Runs
par(mfrow=c(2,2))
hist(batting$MarApr_H, col = "#A71930", xlab = "Plate Apperances", main = "Histogram of March-April H")
hist(batting$MarApr_HR, col = "#09ADAD", xlab = "HRs", main = "Histogram of March-April HR")
boxplot(batting$MarApr_H, col = "#A71930", main = "Boxplot of March-April H")
boxplot(batting$MarApr_HR, col = "#09ADAD", main = "Boxplot of March-April HR")
par(mfrow=c(1,1))
# Runs and RBIs
par(mfrow=c(2,2))
hist(batting$MarApr_R, col = "#A71930", xlab = "Runs", main = "Histogram of March-April R")
hist(batting$MarApr_RBI, col = "#09ADAD", xlab = "RBIs", main = "Histogram of March-April RBIs")
boxplot(batting$MarApr_R, col = "#A71930", main = "Boxplot of March-April R")
boxplot(batting$MarApr_RBI, col = "#09ADAD", main = "Boxplot of March-April RBIs")
par(mfrow=c(1,1))
# Stolen Bases and Walks
par(mfrow=c(2,2))
hist(batting$MarApr_SB, col = "#A71930", xlab = "Stolen Bases", main = "Histogram of March-April SB")
hist(batting$MarApr_BB., col = "#DBCEAC", xlab = "Walks", main = "Histogram of March-April BB")
boxplot(batting$MarApr_SB, col = "#A71930", main = "Boxplot of March-April SB")
boxplot(batting$MarApr_BB., col = "#DBCEAC", main = "Boxplot of March-April BB")
par(mfrow=c(1,1))
#Strikeouts and ISO
par(mfrow=c(2,2))
hist(batting$MarApr_K., col = "#A71930", xlab = "Strikeouts", main = "Histogram of March-April SO")
hist(batting$MarApr_ISO, col = "#DBCEAC", xlab = "ISO", main = "Histogram of March-April ISO")
boxplot(batting$MarApr_K., col = "#A71930", main = "Boxplot of March-April SO")
boxplot(batting$MarApr_ISO, col = "#DBCEAC", main = "Boxplot of March-April ISO")
par(mfrow=c(1,1))
#BABIP and AVG
par(mfrow=c(2,2))
hist(batting$MarApr_BABIP, col = "#A71930", xlab = "BABIP", main = "Histogram of March-April BABIP")
hist(batting$MarApr_AVG, col = "#DBCEAC", xlab = "AVG", main = "Histogram of March-April AVG")
boxplot(batting$MarApr_BABIP, col = "#A71930", main = "Boxplot of March-April BABIP")
boxplot(batting$MarApr_AVG, col = "#DBCEAC", main = "Boxplot of March-April AVG")
par(mfrow=c(1,1))
#OBP and SLG
par(mfrow=c(2,2))
hist(batting$MarApr_OBP, col = "#A71930", xlab = "OBP", main = "Histogram of March-April OBP")
hist(batting$MarApr_SLG, col = "#DBCEAC", xlab = "SLG", main = "Histogram of March-April SLG")
boxplot(batting$MarApr_OBP, col = "#A71930", main = "Boxplot of March-April OBP")
boxplot(batting$MarApr_SLG, col = "#DBCEAC", main = "Boxplot of March-April SLG")
par(mfrow=c(1,1))
#LD% and GB%
par(mfrow=c(2,2))
hist(batting$MarApr_LD., col = "#A71930", xlab = "Line Drives", main = "Histogram of March-April LD")
hist(batting$MarApr_GB., col = "#DBCEAC", xlab = "Ground Balls", main = "Histogram of March-April GB")
boxplot(batting$MarApr_LD., col = "#A71930", main = "Boxplot of March-April LD")
boxplot(batting$MarApr_GB., col = "#DBCEAC", main = "Boxplot of March-April GB")
par(mfrow=c(1,1))
#FB% and IFFB%
par(mfrow=c(2,2))
hist(batting$MarApr_FB., col = "#A71930", xlab = "Fly Ball", main = "Histogram of March-April FB")
hist(batting$MarApr_IFFB., col = "#DBCEAC", xlab = "ISO", main = "Histogram of March-April IFFB")
boxplot(batting$MarApr_FB., col = "#A71930", main = "Boxplot of March-April FB")
boxplot(batting$MarApr_IFFB., col = "#DBCEAC", main = "Boxplot of March-April IFFB")
par(mfrow=c(1,1))
#HR/FB% and O-Swing%
par(mfrow=c(2,2))
hist(batting$MarApr_HR.FB, col = "#A71930", xlab = "HR/FB", main = "Histogram of March-April HR/FB")
hist(batting$MarApr_O.Swing., col = "#DBCEAC", xlab = "O-Swing%", main = "Histogram of March-April O-Swing%")
boxplot(batting$MarApr_HR.FB, col = "#A71930", main = "Boxplot of March-April HR/FB")
boxplot(batting$MarApr_O.Swing., col = "#DBCEAC", main = "Boxplot of March-April O-Swing%")
par(mfrow=c(1,1))
#Z-Swing% and Swing%
par(mfrow=c(2,2))
hist(batting$MarApr_HR.FB, col = "#A71930", xlab = "HR/FB", main = "Histogram of March-April Z-Swing%")
hist(batting$MarApr_O.Swing., col = "#DBCEAC", xlab = "Swing%", main = "Histogram of March-April Swing%")
boxplot(batting$MarApr_HR.FB, col = "#A71930", main = "Boxplot of March-April Z-Swing%")
boxplot(batting$MarApr_O.Swing., col = "#DBCEAC", main = "Boxplot of March-April Swing%")
par(mfrow=c(1,1))
#O-Contact% and Z-Contact%
par(mfrow=c(2,2))
hist(batting$MarApr_O.Contact., col = "#A71930", xlab = "O-Contact%", main = "Histogram of March-April O-Contact%")
hist(batting$MarApr_Z.Contact., col = "#DBCEAC", xlab = "Z-Contact%", main = "Histogram of March-April Z-Contact%")
boxplot(batting$MarApr_O.Contact., col = "#A71930", main = "Boxplot of March-April O-Contact%")
boxplot(batting$MarApr_Z.Contact., col = "#DBCEAC", main = "Boxplot of March-April Z-Contact%")
par(mfrow=c(1,1))
#Contact%
par(mfrow = c(1,2))
hist(batting$MarApr_Contact., col = "#A71930", xlab = "Contact%", main = "Histogram of March-April Contact%")
boxplot(batting$MarApr_Contact., col = "#A71930", xlab = "Contact%", main = "Boxplot of March-April Contact%")
par(mfrow=c(1,1))



#Scatterplot Matrix - for feature selection
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Box Score Counting Stats and Season OBP
pairs(~ batting$FullSeason_OBP + batting$MarApr_PA + batting$MarApr_AB + batting$MarApr_H + batting$MarApr_HR + batting$MarApr_R
      +batting$MarApr_RBI + batting$MarApr_SB + batting$MarApr_BB.+batting$MarApr_K., lower.panel=panel.smooth, upper.panel = panel.cor)

#Base Ratio Stats and Season OBP
pairs(~ batting$FullSeason_OBP + batting$MarApr_ISO + batting$MarApr_BABIP + batting$MarApr_AVG + batting$MarApr_OBP + batting$MarApr_SLG, 
      lower.panel=panel.smooth, upper.panel = panel.cor)

#Batted Ball Type Stats and Season OBP - Relationships are weaker
pairs(~ batting$FullSeason_OBP + batting$MarApr_LD. + batting$MarApr_GB. + batting$MarApr_IFFB. + batting$MarApr_HR.FB, 
      lower.panel=panel.smooth, upper.panel = panel.cor)

#Plate Discipline Stats and Season OBP
pairs(~ batting$FullSeason_OBP + batting$MarApr_O.Swing. + batting$MarApr_Z.Swing. + batting$MarApr_Swing. + batting$MarApr_O.Contact.
      + batting$MarApr_Z.Contact. + batting$MarApr_Contact., lower.panel=panel.smooth, upper.panel = panel.cor)

###################### Part 2 - Feature Selection and Alternative Regression Methods #########################################

####### Ridge Regression#######

y = batting$FullSeason_OBP
x <- model.matrix(FullSeason_OBP~MarApr_H + MarApr_HR + MarApr_BB. + MarApr_K. + MarApr_ISO + MarApr_OBP + MarApr_HR.FB + MarApr_O.Swing. + MarApr_Z.Contact.,
                    data=batting)


grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
predict(ridge.mod,s=50,type="coefficients")[1:11,]
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)

ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)

ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T,x=x[train,],y=y[train])
mean((ridge.pred-y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients",x=x[train,],y=y[train])[1:11,]

#find optimal lambda
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
#Rerun Ridge Regression with optimal lambda
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
out=glmnet(x,y,alpha=0)
RidgeModelCoef = predict(out,type="coefficients",s=bestlam)[1:11,]

####### Lasso ######
#x and y are the same as in ridge regression

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:11,]
lasso.coef
lasso.coef[lasso.coef!=0]

#Lasso method sets coefficient of all X variables to 0 except for MarApr_H and MarApr_OBP

####### Bootstrap #########
library(boot)
#Statistic of interest - estimate alpha for MarApr_OBP on FullSeason_OBP
alpha.fn=function(batting,index){
  X=batting$MarApr_OBP[index]
  Y=batting$FullSeason_OBP[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
alpha.fn(batting,1:100)
set.seed(1)
alpha.fn(batting,sample(100,100,replace=T))
boot(batting,alpha.fn,R=160)

#################### Part 3: Model Creation and Performance ############################################
#Function for calculating Mean Squared Error
mse <- function(sm) 
  mean(sm$residuals^2)

# Stepwise Approach by Akaike Information Criterion
stepwisemodel <- lm(formula = FullSeason_OBP ~ MarApr_PA + MarApr_H + MarApr_HR + MarApr_BB. + MarApr_K. + MarApr_ISO + MarApr_OBP + MarApr_O.Swing. + 
                      MarApr_Z.Contact., data = batting)
stepwise <- stepAIC(stepwisemodel, direction = "both")
summary(stepwise)

batting$StepwiseModelPrediction <- predict(stepwise, type = "response")

#Simple Model Based on Lasso
Simple <- lm(formula = FullSeason_OBP ~ MarApr_OBP + MarApr_H, data = batting)
summary(Simple)

batting$SimpleModelPrediction <- predict(Simple, type = "response")


######## Performance #######
mse(stepwise)
mse(Simple)
vif(stepwise)
vif(Simple)


#Evaluate results for Stepwise Model Prediction
summary(batting$StepwiseModelPrediction)
hist(batting$StepwiseModelPrediction)
boxplot(batting$StepwiseModelPrediction, col = "#A71930", main = "Boxplot of Predicted OBP")



#Evaluate results for Simple Model Prediction
summary(batting$SimpleModelPrediction)
hist(batting$SimpleModelPrediction)
boxplot(batting$SimpleModelPrediction, col = "#A71930", main = "Boxplot of Predicted OBP")


#Chosen Model - Stepwise Model
prediction <- batting[c("playerid", "Name", "Team", "StepwiseModelPrediction")]


#Prediction File 
write.csv(prediction, file = "Q2Prediction.csv")
