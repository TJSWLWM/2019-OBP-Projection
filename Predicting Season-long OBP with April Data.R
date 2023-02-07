###############Tanner Smith
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
library(leaps)
library(MASS)
library(corrplot)
library(glm2)
library(aod)
library(glmnet)
library(caret)
library(gbm)
library(jtools)
library(Rcpp)
library(stargazer)

batting=read.csv("C:/Users/tanne/OneDrive/Documents/batting.csv",header=T)


#General Approach: Create model to predict March/April OBP 
#and then test to see how well it performs at predicting full season OBP

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

# Box Score Counting Stats and March-April OBP
pairs( batting$MarApr_OBP ~ batting$MarApr_PA + batting$MarApr_AB + batting$MarApr_H + batting$MarApr_HR + batting$MarApr_R
      +batting$MarApr_RBI + batting$MarApr_SB + batting$MarApr_BB.+batting$MarApr_K., lower.panel=panel.smooth, upper.panel = panel.cor)

#Base Ratio Stats and March-April OBP
pairs(batting$MarApr_OBP ~ batting$MarApr_ISO + batting$MarApr_BABIP + batting$MarApr_AVG + batting$MarApr_SLG, 
      lower.panel=panel.smooth, upper.panel = panel.cor)

#Batted Ball Type Stats and March-April OBP - Relationships are weaker
pairs(batting$MarApr_OBP ~ batting$MarApr_LD. + batting$MarApr_GB. + batting$MarApr_IFFB. + batting$MarApr_HR.FB, 
      lower.panel=panel.smooth, upper.panel = panel.cor)

#Plate Discipline Stats and March-April OBP - Relationships are weaker
pairs(batting$MarApr_OBP ~  batting$MarApr_O.Swing. + batting$MarApr_Z.Swing. + batting$MarApr_Swing. + batting$MarApr_O.Contact.
      + batting$MarApr_Z.Contact. + batting$MarApr_Contact., lower.panel=panel.smooth, upper.panel = panel.cor)


###Creating two separate dataframes (one without March/April OBP, one without Full-Season OBP). This will allow us to cleanly output results later.

train <- subset(batting, select = -c(FullSeason_OBP) ) #Getting rid of Full Season OBP to allow MarApr OBP to be OBP in training
test <- subset(batting, select = -c(MarApr_OBP)) #Getting rid of MarApr OBP to allow full-season OBP to be OBP

names(train)[names(train) == 'MarApr_OBP'] <- 'OBP'
names(test)[names(test) == 'FullSeason_OBP'] <- 'OBP'

#################### Part 2: Model Creation ############################################


########## Creating two models - one linear regression model using stepwise feature selection and one gradient boosted model (GBM).
########## Will then compare results and pick best model

##### Model 1: Linear with stepwise feature selection - Adds and subtracts predictors (using both parameter) to discover the appropriate predictors

# Stepwise Approach by Akaike Information Criterion
fullmodel <- lm(formula = OBP ~ MarApr_PA + MarApr_HR + MarApr_BB. + MarApr_R 
                + MarApr_RBI + MarApr_SB + MarApr_K. + MarApr_ISO + MarApr_BABIP + MarApr_FB.+ MarApr_GB. + MarApr_IFFB. 
                + MarApr_HR.FB + MarApr_O.Swing. + MarApr_Z.Contact. + MarApr_Contact., data = train)
stepwise <- stepAIC(fullmodel, direction = "both", trace = FALSE)
summary(stepwise)

# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation, having it resample 10 times
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(OBP ~ MarApr_PA + MarApr_HR + MarApr_BB. + MarApr_R 
                    + MarApr_RBI + MarApr_SB + MarApr_K. + MarApr_ISO + MarApr_BABIP + MarApr_FB.
                    + MarApr_GB. + MarApr_IFFB. 
                    + MarApr_HR.FB + MarApr_O.Swing. + MarApr_Z.Contact. + MarApr_Contact., data = train,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:16),
                    trControl = train.control)   #Using leapBackward because we are concerned about collinearity and have a relatively small number of predictors.
step.model$results
step.model$bestTune
summary(step.model$finalModel)
coef(step.model$finalModel, 8)

LModel = lm(OBP ~ MarApr_PA + MarApr_BB. + MarApr_R + MarApr_K. + MarApr_ISO + MarApr_BABIP, 
   data = train)
summary(LModel)

test$POBP <- predict(LModel, newdata = test, type = "response")

######### Model 2: GBM Model

### Creates trees one at a time and adds them together (weak learners) to steadily improve results. 

## Working with small dataset, need to use cross-fold validation and many trees. 
#Tree number was initially higher, adjusted down after testing for optimal number below. Variables selected to be complete while avoiding multicollinearity. 
GBModel = gbm(train$OBP ~ MarApr_PA + MarApr_HR + MarApr_BB. + MarApr_R 
                  + MarApr_RBI + MarApr_SB + MarApr_K. + MarApr_ISO + MarApr_BABIP + MarApr_FB.
                  + MarApr_GB. + MarApr_IFFB. 
                  + MarApr_HR.FB + MarApr_O.Swing. + MarApr_Z.Contact. + MarApr_Contact., data = train, 
                  distribution = "gaussian", cv.folds = 10, shrinkage = 0.01, n.minobsinnode = 10, n.trees = 4480)

#Number of trees is later hyper-parameter optimized, and that got to 4480

print(GBModel)
summary(GBModel)

##Weights BABIP, BB, K, ISO highest


#################### Part 3: Model Validation and Result Output ############################################


#### Looking at variables with highest relative influences
plot(GBMModel, i = "MarApr_BABIP")
plot(GBModel, i = "MarApr_BB.")
plot(GBModel, i = "MarApr_K.")
plot(GBModel, i = "MarApr_ISO")

## These appear to have the proper relationships, as BABIP, BBs and ISO have a positive correlation with OBP, while Ks have a negative correlation.


##Testing for optimal number of trees
gbm.perf(GBModel) #Adjust to 4480

test$PROBP <- predict(GBModel, newdata = test, type = "response")


######## Performance #######

#Function for calculating Mean Squared Error
mse <- function(sm) 
  mean(sm$residuals^2)

mse(LModel)
mean((test$PROBP-test$OBP)^2) #MSE for GBM


### Testing for multicollinearity in model
vif(LModel) #ISO and HR/FB have moderately high levels of multicollinearity, which makes sense. 
### Best way to handle this is to try the model without HR/FB, which was found to be less important by GBM model and in baseball circles
### is considered less skill-based. In doing this, FB rate becomes statistically insignificant, leading to the new model below. 
### R^2 and Adjusted R^2 are reduced by less than 0.01


LModel = lm(OBP ~ MarApr_PA + MarApr_BB. + MarApr_R + MarApr_K. + MarApr_ISO + MarApr_BABIP, 
            data = train)
summary(LModel)

LModelHRFB = LModel = lm(OBP ~ MarApr_PA + MarApr_BB. + MarApr_R + MarApr_K. + MarApr_ISO + MarApr_BABIP + MarApr_HR.FB + MarApr_FB., 
                         data = train)
summary(LModelHRFB)

### Also wanted to test by AIC by creating separate model with HR/FB and FB rate included. 
### AIC is slightly better for model with HR/FB and FB rate, which makes sense why the stepwise model included them, 
### but not to such a large degree to change my decision
AIC(LModel)
AIC(LModelHRFB)


### Now we should re-test the mse of the respective models to make sure the linear model is still outperforming the GBM without the multicollinearity. 
mse(LModel)
mean((test$PROBP-test$OBP)^2) #MSE for GBM


#### Output Results for Champion Linear model

prediction <- test[c('Name', 'OBP', 'POBP')]
#Writing CSV file with predictions
write.csv(prediction, file = "LinearOBPresults.csv")

##Outputting predictions for GBM model

prediction <- test[c('Name', 'OBP', 'PROBP')]
#Writing CSV file with predictions
write.csv(prediction, file = "GBMOBPresults.csv")
