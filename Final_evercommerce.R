rm(list=ls())

setwd("C:/Users/SEONHEEJOO/Desktop/Marketing/Evercommerce")

### To create these variables you can run a series of ifelse commands
# Library  --------------------------------------------------------------

install.packages('neuralnet')
install.packages('psych')
install.packages("lolcat")


library(dplyr)
library(purrr)
library(stringr)
library(tidyverse)
library(corrr)
library(neuralnet)
library(caret)
library(tidyverse)
library(readr)
library(formattable)
library(plotly)
library(gt)
library(patchwork)
library(sjPlot)
library(glmnet)
library(randomForest)
library(varImp)
library(car)
library(psych)
library(caTools)
library(GGally)

# Function for pseudo R^2 (for logistic regression analysis)
PseudoR2<-function(glmModel){
  #if (length(class(object)) > 1)
  #if (class(glmModel)!="glm" | class(glmModel)!="lm"){
  #stop("Object not of class 'glm'")
  #}
  #else {
  #	glmModel<-glmModel
  #	}
  
  logLikN<-glmModel$null/-2  ##log likelihood, null model
  logLikF<-glmModel$dev/-2  ##log likelihood, full model
  G2<-glmModel$null - glmModel$deviance
  n <- length(glmModel$y)	
  ystar <- predict(glmModel, type="response") 
  class1<-ifelse(ystar >.5,1,0) 
  classtab<-table(class1, glmModel$y, dnn=c("Predicted", "Actual")) ; maxOut<-max(margin.table(classtab, 2))
  p<-glmModel$rank
  penaltyN<- 2*(p)*(p+1)  ; penaltyD<- n-p-1  ; penalty<-penaltyN/penaltyD
  MZystar <- predict(glmModel); sse <- sum((MZystar - mean(MZystar))^2) ; s2 <- switch(glmModel$family$link, "probit" = 1, "logit" = pi^2/3, NA)  #Needed for MZ R2
  Enum<-sum((glmModel$y - ystar)^2); Edenom<-sum((glmModel$y - mean(glmModel$y))^2) #Needed for Effron R2
  
  #R2s
  r2McF<-1-logLikF/logLikN  #Mcfadden's R2
  r2McFA<-1-(logLikF - p-1 )/logLikN #Mcfadden's Adj R2
  r2CS<-1-exp(-G2/n) #ML Cox/Snell R2
  r2N<-(1 - exp((glmModel$dev - glmModel$null)/n))/(1 - exp(-glmModel$null/n))# Nagelkerke/Cragg-Uhler R2
  r2MZ<-sse / (n * s2 + sse)  #McKelvey and Zavoina pseudo R^2, using either the logit or probit link
  r2E<-1-(Enum/Edenom) #Effron R2
  r2C<-(classtab[1] + classtab[4])/n##Count R2 (proportion correctly classified)
  r2CA<-(classtab[1] + classtab[4] - maxOut)/(n - maxOut) ##Adjusted Count R2 (proportion correctly classified)
  aic<-2*(p)+glmModel$dev # AIC
  Caic<-aic + penalty # AIC with a correction for finite sample size; useful with small sample sizes or a lot of predictors
  
  results<-c(McFadden=r2McF, Adj.McFadden=r2McFA, Cox.Snell=r2CS, Nagelkerke=r2N, McKelvey.Zavoina=r2MZ, Effron=r2E, Count=r2C, Adj.Count=r2CA, AIC=aic, Corrected.AIC=Caic)
  return(results)
  
}




# Function for performance metrics
prf <- function(predAct){
  ## predAct is two col dataframe of pred,act
  preds = predAct[,1]
  trues = predAct[,2]
  xTab <- table(preds, trues)
  clss <- as.character(sort(unique(preds)))
  r <- matrix(NA, ncol = 7, nrow = 1, 
              dimnames = list(c(),c('Acc',
                                    paste("P",clss[1],sep='_'), 
                                    paste("R",clss[1],sep='_'), 
                                    paste("F",clss[1],sep='_'), 
                                    paste("P",clss[2],sep='_'), 
                                    paste("R",clss[2],sep='_'), 
                                    paste("F",clss[2],sep='_'))))
  r[1,1] <- sum(xTab[1,1],xTab[2,2])/sum(xTab) # Accuracy
  r[1,2] <- xTab[1,1]/sum(xTab[,1]) # Miss Precision
  r[1,3] <- xTab[1,1]/sum(xTab[1,]) # Miss Recall
  r[1,4] <- (2*r[1,2]*r[1,3])/sum(r[1,2],r[1,3]) # Miss F
  r[1,5] <- xTab[2,2]/sum(xTab[,2]) # Hit Precision
  r[1,6] <- xTab[2,2]/sum(xTab[2,]) # Hit Recall
  r[1,7] <- (2*r[1,5]*r[1,6])/sum(r[1,5],r[1,6]) # Hit F
  r}

# Making new easy to work with data frame
# Load csv files
EC1 <- read.csv("First data set.csv",stringsAsFactors=TRUE)
# Code to restore zip codes
EC1$zip <- as.character(EC1$zip)
for(i in 1:length(EC1$zip)){
  if(as.numeric(EC1$zip[i]) < 10000){
    EC1$zip[i] <- paste0("0", EC1$zip[i])
  }
}
EC1 <- as.data.frame(unclass(EC1), stringsAsFactors = TRUE)
head(EC1$zip)
str(EC1)

EC2<- read.csv("Second data set.csv",stringsAsFactors=TRUE)
# Code to resotre zip codes
EC2$zip <- as.character(EC2$zip)
for(i in 1:length(EC2$zip)){
  if(as.numeric(EC2$zip[i]) < 10000){
    EC2$zip[i] <- paste0("0", EC2$zip[i])
  }
}
EC2 <- as.data.frame(unclass(EC2), stringsAsFactors = TRUE)
head(EC2$zip)
str(EC2)

# Adding NumberHH and MedianIncome
## First read data
us_zip <- read.csv("zip data.csv")
## Code to remove first six characters before zipcode
us_zip$NAME[ us_zip$NAME != 'Geographic Area Name'] <- (gsub("^.{0,6}", "", us_zip$NAME[ us_zip$NAME != 'Geographic Area Name']))
## Remove first row
us_zip = us_zip[-1,]
## Create groups by first four numbers of zip
us_zip$group <- as.factor(str_sub(us_zip$NAME,1,4))
## Convert to numeric and make rest factors
us_zip <- as.data.frame(unclass(us_zip), stringsAsFactors = TRUE)
us_zip <- transform(us_zip, 
                    S1901_C01_001E = as.numeric(S1901_C01_001E), 
                    S1901_C01_012E = as.numeric(S1901_C01_012E))
## Create new data frame for imputed values
us_zip_group <- aggregate(cbind(S1901_C01_001E, S1901_C01_012E) ~ group, data = us_zip, FUN = median)
## Check updates
nrow(us_zip)
head(us_zip$NAME)


## Now merge data by zip code and save as new EC1
EC1 <- left_join(EC1, us_zip[ , c('NAME', 'S1901_C01_001E', 'S1901_C01_012E')], by=c("zip" = "NAME"))
## Change names
colnames(EC1)[27] <- 'NumberHH'
colnames(EC1)[28] <- 'MedianIncome'
## Add group to EC1
EC1$group <- str_sub(EC1$zip,1,4)
## Join with imputed data frame
EC1 <- left_join(EC1, us_zip_group, by='group')
## Impute nulls
EC1$NumberHH <- ifelse(is.na(EC1$NumberHH), EC1$S1901_C01_001E, EC1$NumberHH)
EC1$MedianIncome <- ifelse(is.na(EC1$MedianIncome), EC1$S1901_C01_012E, EC1$MedianIncome)
## Remove group and last two columns
EC1$group <- NULL
EC1$S1901_C01_001E <- NULL
EC1$S1901_C01_012E <- NULL
## Review structure
str(EC1)
## Check nulls in each column 
colSums(is.na(EC1))
## Zip codes with missing values
EC1[rowSums(is.na(EC1)) > 0, 'zip']
## Remove nulls
EC1 <- na.omit(EC1)
nrow(EC1)


## Now merge data by zip code and save as new EC2
EC2 <- left_join(EC2, us_zip[ , c('NAME', 'S1901_C01_001E', 'S1901_C01_012E')], by=c("zip" = "NAME"))
## Change names
colnames(EC2)[24] <- 'NumberHH'
colnames(EC2)[25] <- 'MedianIncome'
## Add group to EC2
EC2$group <- str_sub(EC2$zip,1,4)
## Join with imputed data frame
EC2 <- left_join(EC2, us_zip_group, by='group')
## Impute nulls
EC2$NumberHH <- ifelse(is.na(EC2$NumberHH), EC2$S1901_C01_001E, EC2$NumberHH)
EC2$MedianIncome <- ifelse(is.na(EC2$MedianIncome), EC2$S1901_C01_012E, EC2$MedianIncome)
## Remove group and last two columns
EC2$group <- NULL
EC2$S1901_C01_001E <- NULL
EC2$S1901_C01_012E <- NULL
## Review structure
str(EC2)
## Check nulls in each column 
colSums(is.na(EC2))
## Zip codes with missing values
EC2[rowSums(is.na(EC2)) > 0, 'zip']
## Remove nulls
EC2 <- na.omit(EC2)
nrow(EC2)


# 1: run the script to make verticals into dummy variables
EC1$isFinance <- ifelse(EC1$vertical == "finance", 1, 0)
EC1$isFitness <- ifelse(EC1$vertical == "fitness", 1, 0)
EC1$isHealthC <- ifelse(EC1$vertical == "healthca", 1, 0)
EC1$isHomeImp <- ifelse(EC1$vertical == "homeimp", 1, 0)
EC1$isLegal <- ifelse(EC1$vertical == "legal", 1, 0)
EC1$isOnline <- ifelse(EC1$vertical == "online", 1, 0)
EC1$isRealEst <- ifelse(EC1$vertical == "realesta", 1, 0)
EC1$isSecurity <- ifelse(EC1$vertical == "security", 1, 0)
EC1$isTherapy <- ifelse(EC1$vertical == "therapy", 1, 0)


EC2$isFinance <- ifelse(EC2$vertical == "finance", 1, 0)
EC2$isFitness <- ifelse(EC2$vertical == "fitness", 1, 0)
EC2$isHealthC <- ifelse(EC2$vertical == "healthca", 1, 0)
EC2$isHomeImp <- ifelse(EC2$vertical == "homeimp", 1, 0)
EC2$isLegal <- ifelse(EC2$vertical == "legal", 1, 0)
EC2$isOnline <- ifelse(EC2$vertical == "online", 1, 0)
EC2$isRealEst <- ifelse(EC2$vertical == "realesta", 1, 0)
EC2$isSecurity <- ifelse(EC2$vertical == "security", 1, 0)
EC2$isTherapy <- ifelse(EC2$vertical == "therapy", 1, 0)

names(EC1)
names(EC2)


# 2: Think about predictors + examine correlations
## Removing Cust_Psim because sd is 0, all of them are psimp customers.
## Uneccessary to view correlation for Cust_MHW and Cust_L360 against Cust_Lobb

unwanted_col <- c("vertical", "zip", "Cust_Psim", "Cust_MHW", "Cust_L360")

EC1_cor <- correlate(EC1[ , !names(EC1) %in% unwanted_col],
                     method = 'pearson',
                     use = 'pairwise.complete.obs')
EC1_cor %>% rplot() # this turns correlation table into a visual


# 1) Build model with all variables (except for vertical,Cust_Psim, zip, Cust_MHW, Cust_L360)
## Cust_Psim & zip contribute nothing to the model, since they are all the same
EC1$Cust_Psim <- NULL
EC1$zip <- NULL
EC2$zip <- NULL
### Align other 2 for 2nd data set: drop cust_MHW, Cust_L360 (not in EC2)
#### drop 'vertical' and one dummies flag (regard it as the baseline to fit the model)
EC1 <- EC1 %>% select(-vertical,-isHealthC,-Cust_MHW,-Cust_L360) 
EC2 <- EC2 %>% select(-vertical,-isHealthC)

custid <- EC2$custid
EC2$custid <- NULL

names(EC1)
names(EC2)

# Check number of variables
length(EC1)
length(EC2)

glm1 <- glm(Cust_Lobb ~ ., family = "binomial", data=EC1)
summary(glm1)
PseudoR2(glm1)

# 2) Build model dropping highly correlated variables 
## Cust_Psim & zip contribute nothing to the model, since they are all the same
### Align other 2 for 2nd data set: drop ust_MHW, Cust_L360 (not in EC2)
#### drop 'vertical' since we already created those dummies
#### For multicollinearity: aggregate your Psim_rev (highly correlated, aggregated them all and drop the old variables)

# For multicollinearity ----------------
# aggregate your Psim_rev
EC1 <- EC1 %>% rowwise() %>% mutate(total_rev = sum(c_across(starts_with("PsimRev"))))
EC1 <- EC1 %>% select(-starts_with('PsimRev'))

# Same for EC2
EC2 <- EC2 %>% rowwise() %>% mutate(total_rev = sum(c_across(starts_with("PsimRev"))))
EC2 <- EC2 %>% select(-starts_with('PsimRev'))

# Check number of variables: 22 same 
length(EC1)
length(EC2)

# Build model -
lobb.model <- glm(formula = Cust_Lobb ~., data = EC1, family = 'binomial')
summary(lobb.model)
PseudoR2(lobb.model)

vif(lobb.model) # check for multicollinearity - should be lower than 10

# predict 
EC2_pred_glm <- predict(lobb.model,newdata = EC2, type = 'response')
EC2$custid <- custid
EC2$prob <- EC2_pred_glm
EC2_prediction_glm <- EC2 %>% arrange(desc(prob)) %>% head(1000)
write.csv(EC2_prediction_glm,
          "C:/Users/SEONHEEJOO/Desktop/Marketing/Evercommerce/EC2_check_glm2.csv")



#Find the best threshold to determine model accuracy
glm2.probs <- predict(lobb.model, type='response')
maxAcc <- 0;
maxThreshold <- 0; 
for (threshold in seq(1,100)) {
  glm2.pred <- rep(0, nrow(EC1))
  glm2.pred[glm2.probs > threshold/100 ] <- 1
  currentAcc <- mean(glm2.pred == EC1$Cust_Lobb)
  print(paste(threshold,currentAcc))
  if (currentAcc > maxAcc) {maxAcc <- currentAcc; maxThreshold <- threshold/100}
}

glm2.pred <- rep(0, nrow(EC1));
glm2.pred[glm2.probs > maxThreshold] <- 1;
table(glm2.pred, EC1$Cust_Lobb)
str(EC2)

EC2_pred = subset(EC2, select = -c(vertical, zip))

EC2_pred$cust_lobby <- predict(glm2, type='response', newdata = EC2_pred)

write.csv(EC2_pred,"/Users/gandharvdua/Desktop/final.csv")



## Convert multiple columns to factor(Just in case we need this, not applied to run the model above)
str(EC1)
cols <- c("referral", "latepay", "Psim_dsf", "Cust_MHW",'Cust_Lobb','Cust_L360')
EC1[cols] <- lapply(EC1[cols], factor)
EC1[,27:34] <- lapply(EC1[,27:34], factor)
str(EC1)


# How to call (use) the two functions above
## Note: you do not need to perform train, test split on EC1 for this function
PseudoR2(my_models_name)
PseudoR2(my_models_name)[1] # this will return just the first type of pseudo R^2 metric

# If you perform a train, test split on EC1, you can use this function
## Note: EC1_test is the name of the subset of EC1 you that set aside for testing, 
## since you know the labels from EC1 you use EC1 for train, test split, not EC2.
## Last thing, the predictions passed here need to be binary value (0 or 1), use a threshold of 0.5
## and convert your probability predictions into either class 1 or class 0 for Cust_Lobb
predAct_df <- data.frame(predictions_from_my_model, EC1_test$Cust_Lobb)
prf(predAct_df)

# Either of these functions above can help you understand the performance of your model

