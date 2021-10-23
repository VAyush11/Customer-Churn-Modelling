# ========================================================================================================
# Purpose:      Rscript for Logistic Regression on Customer Satisfaction Variables
# Authors:       BC0401 Seminar Group 7 Team 3
# Date:         31-10-2020
# Packages/Libraries:
#                   data.table
#                   rpart
#                   rpart.plot
#                   caret
#                   caTools
#                   dplyr
#                   e1071
# Data Sources:  
#   - "civilserv2.csv" derived from Civil Service Customer Satisfaction Survey 2017 full 
#   survey results 
#   - "Churn Modelling.csv" derived from Kaggle
#=========================================================================================================

#Loading all the required packages
library(data.table)
library(rpart)
library(rpart.plot)
library(caTools)
library(caret)
library(dplyr)
library(e1071)

#------------------------------------------------------
#Logistic Regression on Customer Satisfaction Variables
#------------------------------------------------------

# Import Civil Service Satisfaction Survey dataset
setwd("~/Documents/NBS Y2S1/BC2406 Analytics I/Team Assignment and Project")
civilserv<-read.csv("civilserv2.csv", stringsAsFactors = TRUE)
dim(civilserv)
summary(civilserv)

#remove values 6 and 7 for service and outcome satisfaction
civilserv2<-civilserv[(civilserv$service_satisfaction %in% 1:5) 
                      & (civilserv$outcome_satisfaction %in% 1:5),]
str(civilserv2)

# Discretization of overall satisfaction variables into binary categories
civilserv2[] <- lapply(civilserv2, function(x) as.numeric(as.character(x)))
cor(civilserv2$service_satisfaction,civilserv2$outcome_satisfaction)

civilserv2$b_service_satisfaction<-as.numeric(as.character(civilserv2$service_satisfaction))
civilserv2$b_service_satisfaction[civilserv2$service_satisfaction==1] = "N"
civilserv2$b_service_satisfaction[civilserv2$service_satisfaction==2] = "N"
civilserv2$b_service_satisfaction[civilserv2$service_satisfaction==3] = "N"
civilserv2$b_service_satisfaction[civilserv2$service_satisfaction==4] = "Y"
civilserv2$b_service_satisfaction[civilserv2$service_satisfaction==5] = "Y"
str(civilserv2)

civilserv2$b_outcome_satisfaction<-as.numeric(as.character(civilserv2$outcome_satisfaction))
civilserv2$b_outcome_satisfaction[civilserv2$outcome_satisfaction==1] = "N"
civilserv2$b_outcome_satisfaction[civilserv2$outcome_satisfaction==2] = "N"
civilserv2$b_outcome_satisfaction[civilserv2$outcome_satisfaction==3] = "N"
civilserv2$b_outcome_satisfaction[civilserv2$outcome_satisfaction==4] = "Y"
civilserv2$b_outcome_satisfaction[civilserv2$outcome_satisfaction==5] = "Y"
str(civilserv2)

# Remove demographic variable columns
civilserv2 <- civilserv2[ -c(36:44) ]
str(civilserv2)

#subset by communication medium - phone
civilservphone <- civilserv2[ , !grepl( "writing", names(civilserv2) ) ]
civilservphone <- civilservphone[ ,!grepl( "inperson", names(civilservphone) ) ]
civilservphone <- civilservphone[ ,!grepl( "email_", names(civilservphone) ) ]
newcivilservphone<-civilservphone[complete.cases(civilservphone), ]
newcivilservphone$b_service_satisfaction<-as.factor(newcivilservphone$b_service_satisfaction)
newcivilservphone$b_outcome_satisfaction<-as.factor(newcivilservphone$b_outcome_satisfaction)
str(newcivilservphone)

#subset by communication medium - writing
civilservwriting <- civilserv2[ , !grepl( "phone_", names(civilserv2) ) ]
civilservwriting <- civilservwriting[ ,!grepl( "inperson_", names(civilservwriting) ) ]
civilservwriting <- civilservwriting[ ,!grepl( "email_", names(civilservwriting) ) ]
newcivilservwriting<-civilservwriting[complete.cases(civilservwriting), ]
newcivilservwriting$b_service_satisfaction<-as.factor(newcivilservwriting$b_service_satisfaction)
newcivilservwriting$b_outcome_satisfaction<-as.factor(newcivilservwriting$b_outcome_satisfaction)
str(newcivilservwriting)

#subset by communication medium - inperson
civilservinperson <- civilserv2[ , !grepl( "phone_", names(civilserv2) ) ]
civilservinperson <- civilservinperson[ ,!grepl( "writing_", names(civilservinperson) ) ]
civilservinperson <- civilservinperson[ ,!grepl( "email_", names(civilservinperson) ) ]
newcivilservinperson<-civilservinperson[complete.cases(civilservinperson), ]
newcivilservinperson$b_service_satisfaction<-as.factor(newcivilservinperson$b_service_satisfaction)
newcivilservinperson$b_outcome_satisfaction<-as.factor(newcivilservinperson$b_outcome_satisfaction)
str(newcivilservinperson)

#subset by communication medium - email
civilservemail <- civilserv2[ , !grepl( "phone_", names(civilserv2) ) ]
civilservemail <- civilservemail[ ,!grepl( "inperson_", names(civilservemail) ) ]
civilservemail <- civilservemail[ ,!grepl( "writing_", names(civilservemail) ) ]
newcivilservemail<-civilservemail[complete.cases(civilservemail), ]
newcivilservemail$b_service_satisfaction<-as.factor(newcivilservemail$b_service_satisfaction)
newcivilservemail$b_outcome_satisfaction<-as.factor(newcivilservemail$b_outcome_satisfaction)
str(newcivilservemail)

#log reg for phone - service satisfaction
m1s<-glm(b_service_satisfaction ~ . -b_outcome_satisfaction -service_satisfaction -outcome_satisfaction -s.n -age,family=binomial, data= newcivilservphone)
summary(m1s)
m1s<-glm(b_service_satisfaction ~ phone_easeoffindingnumber + phone_manner + phone_holdingtime,family=binomial, data= newcivilservphone)

#log reg for phone - outcome satisfaction
m1o<-glm(b_outcome_satisfaction ~ . -b_service_satisfaction -service_satisfaction -outcome_satisfaction -s.n -age,family=binomial, data= newcivilservphone)
summary(m1o)

#log reg for writing - service satisfaction
m2s<-glm(b_service_satisfaction ~ . -b_outcome_satisfaction -service_satisfaction -outcome_satisfaction -s.n -age,family=binomial, data= newcivilservwriting)
summary(m2s)

#log reg for writing - outcome satisfaction
m2o<-glm(b_outcome_satisfaction ~ . -b_service_satisfaction -service_satisfaction -outcome_satisfaction -s.n -age,family=binomial, data= newcivilservwriting)
summary(m2o)

#log reg for inperson - service satisfaction
m3s<-glm(b_service_satisfaction ~ . -b_outcome_satisfaction -service_satisfaction -outcome_satisfaction -s.n -age,family=binomial, data= newcivilservinperson)
summary(m3s)

#log reg for inperson - outcome satisfaction
m3o<-glm(b_outcome_satisfaction ~ . -b_service_satisfaction -service_satisfaction -outcome_satisfaction -s.n -age,family=binomial, data= newcivilservinperson)
summary(m3o)
m3o<-glm(b_outcome_satisfaction ~ inperson_knowledge + inperson_queueing + inperson_privacy + inperson_helpwithforms,family=binomial, data= newcivilservinperson)

#log reg for email - service satisfaction
m4s<-glm(b_service_satisfaction ~ . -b_outcome_satisfaction -service_satisfaction -outcome_satisfaction -s.n -age,family=binomial, data= newcivilservemail)
summary(m4s)

#log reg for email - outcome satisfaction
m4o<-glm(b_outcome_satisfaction ~ . -b_service_satisfaction -service_satisfaction -outcome_satisfaction -s.n -age,family=binomial, data= newcivilservemail)
summary(m4o)

# Use OR CI to conclude on statistical significance of X variables.
OR.CI <- exp(confint(m1s))
OR.CI
OR.CI <- exp(confint(m1o))
OR.CI
OR.CI <- exp(confint(m2s))
OR.CI
OR.CI <- exp(confint(m2o))
OR.CI
OR.CI <- exp(confint(m3s))
OR.CI
OR.CI <- exp(confint(m3o))
OR.CI
OR.CI <- exp(confint(m4s))
OR.CI
OR.CI <- exp(confint(m4o))
OR.CI

#------------------------------------------------------
#Classification Tree on Customer Demographic Variables
#------------------------------------------------------

newcivilserv <- fread("civilserv2.csv", stringsAsFactors=T)
newcivilserv$service_satisfaction=factor(newcivilserv$service_satisfaction)
newcivilserv$number_of_dependents=factor(newcivilserv$number_of_dependents)
newcivilserv$tech_savviness=factor(newcivilserv$tech_savviness)

#To reproduce the results
set.seed(300)

#Creating the maximum tree
tree=rpart(service_satisfaction~age+gender+monthly_income+highest_education_level+occupation+marital_status+number_of_dependents+tech_savviness+communication, data=newcivilserv,method='class',control=rpart.control(minsplit=2,cp=0))
rpart.plot(tree, nn=T, main="Maximal Tree")      
printcp(tree)
plotcp(tree, main = "Subtrees in csv")
cp1=0.014

#Pruning to the optimal tree
m3 <- prune(tree, cp = cp1)
printcp(m3)

#Printing the optimal tree
rpart.plot(m3, nn= T, main = "Pruned Tree with cp = 0.014")

#Getting the variable importance factors
m3$variable.importance
summary(m3)

#------------------------------------------------------
#Logistic Regression for Customer Churn Model
#------------------------------------------------------

#Selecting the file
a = fread(file.choose())

#Cleaning Up the Balance Column
dt = a[!(a$Balance == 0),]
a$Balance = replace(a$Balance, a$Balance == 0, mean(dt$Balance))

#Setting the categorical variables to factor
a$Exited = factor(a$Exited)
a$IsActiveMember = factor(a$IsActiveMember)
a$HasCrCard = factor(a$HasCrCard)

levels(a$Exited)

#Cleaning the dataset
a[, 'Surname' := NULL]

#Conducting a train-test split
train = sample.split(a$Exited, SplitRatio = 0.7)
trainset = subset(a, train == T)
testset = subset(a, train == F)

#Modelling on trainset
m_train = glm(Exited~.-CustomerId -RowNumber, family = "binomial", data = trainset)
train_prob = predict(m_train, type = "response")
y_hat_train = ifelse(train_prob > 0.5, 1, 0)
matrix_train = table(Actual = trainset$Exited, Predicted = y_hat_train, deparse.level = 2)
matrix_train

#      Predicted
#Actual    0    1
#     0 5376  198
#     1 1113  313

#Testing the model on testset
test_prob = predict(m_train, newdata = testset, type = "response")
y_hat_test = ifelse(test_prob>0.5, 1, 0)
matrix_test = table(Actual = testset$Exited, Predicted = y_hat_test, deparse.level = 2)
matrix_test

#      Predicted
#Actual    0    1
#     0 2277   112
#     1  471   140

#Summary of variables
summary(m_train)

#Computing the odds ratio confidence intervals
exp(confint(m_train))

#Computing Confusion matrix
confusionMatrix(matrix_test)

#------------------------------------------------------
#Classification Tree for Customer Churn Model
#------------------------------------------------------

#Selecting the file
a = fread(file.choose())

#To reproduce the results
set.seed(1200)

#Cleaning Up the Balance Column
dt = a[!(a$Balance == 0),]
a$Balance = replace(a$Balance, a$Balance == 0, mean(dt$Balance))

#Removing the 'Surname' column
a[, Surname:= NULL]

#Setting all the categorical variables to factor
a$Exited = factor(a$Exited)
a$Geography = factor(a$Geography)
a$Tenure = factor(a$Tenure, ordered = T)
a$Gender = factor(a$Gender)
a$IsActiveMember = factor(a$IsActiveMember)
a$HasCrCard = factor(a$HasCrCard)
a$NumOfProducts = factor(a$NumOfProducts, ordered = T)

#Verifying if the change was successful
summary(a)

#Conducting a train-test split (70/30)
#This is outside of the 10-fold cross-validation error's
#auto train-test split

train = sample.split(a$Exited, 0.7)
trainset = subset(a, train == T)
testset = subset(a, train == F)

#Growing the tree to the maximum
cart1 = rpart(Exited~.-RowNumber-CustomerId, data = trainset, method = 'class', control = rpart.control(minsplit = 2, cp = 0))

#Associated results
print(cart1) #Prints the decision rules
rpart.plot(cart1) #Plots the tree
plotcp(cart1) #Plots the pruning sequence
printcp(cart1) #Prints the pruning sequence, cps and the associated errors

#Computing the (min CV-error + 1SE) in max tree - cart1.
CVerror.cap <- cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xerror"] + cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in max tree - cart1.
i <- 1; j<- 4
while (cart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

#Obtaining the geometric mean of the two identified CP values in the optimal region, if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cart1$cptable[i,1] * cart1$cptable[i-1,1]), 1)

#Obtaining the optimal tree by pruning it based on the above calculations
cart2 = prune(cart1, cp = cp.opt)

#Associated results (cart 2 - Optimal tree)
rpart.plot(cart2, nn = T) #Plots the tree
printcp(cart2) #Prints the pruning sequence, cps and the associated errors
print(cart2) #Prints the decision rules

#Testing the CART model on the 70% trainset
y_hat_train = predict(cart2, type = "class")
train_matrix = table(Actual = trainset$Exited, Predicted = y_hat_train, deparse.level = 2)
train_matrix

#Defining a function to easily compute the accuracy of the model's predictions
accuracy = function(Actual, Predicted){
  mean(Actual == Predicted)
}

accuracy(Actual = trainset$Exited, Predicted = y_hat_train)
#0.8658

#Testing the CART model on the 30% testset we kept at the start
y_hat_test = predict(cart2, newdata = testset, type = "class")
test_matrix = table(Actual = testset$Exited, Predicted = y_hat_test)
test_matrix
accuracy(Actual = testset$Exited, Predicted = y_hat_test)
#0.857

#Computing the importance of the different variables
cart2$variable.importance
#Age = 330.76, NumOfProducts = 232.02, IsActiveMember = 121.15,
#Geography = 33.70, Balance 15.78 

#We can get the same (fitted to comprise 100%) from the summary function
summary(cart2)

#Age = 45, NumOfProducts = 31, IsActiveMember = 16, Geography = 5
#Balance= 2

#Obtaining the confusion matrix
confusionMatrix(testset$Exited, y_hat_test)





