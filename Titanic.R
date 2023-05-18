library(tidyverse)
library(mosaic)
library(rsample)
library(modelr)



#If it is train dataset, set training subset to TRUE. if it is test set, training is False
train$Training <- TRUE
test$Training <- FALSE

#Check if it works
tail(train$Training)

#Make a column in test set for Survived
test$Survived <- NA

#Combine training and test
titanic.full <- rbind(train, test)
titanic.full[titanic.full$Embarked=='', "Embarked"] <- 'S'

#Clean missing values of age
#age.median <- median(titanic.full$Age, na.rm = TRUE)
#titanic.full[is.na(titanic.full$Age), "Age"] <- age.median
#table(is.na(titanic.full$Age))
######### BETTER PREDICTION FOR AGE ########

#Check for outliers
boxplot(titanic.full$Age)
boxplot.stats(titanic.full$Age)

upper.whiskherage <- boxplot.stats(titanic.full$Age)$stats[5]
outlier_filter_age <- titanic.full$Age < upper.whiskherage

##How we should predict age avoid overfitting
age.equation = "Age ~Pclass + Sex + Fare + SibSp + Parch + Embarked"
#Create linear regression model for prediction using age equation
age.model<-lm(
  formula = age.equation,
  data = titanic.full[outlier_filter_age, ]
)

##Query for missing age rows


age.rows<-titanic.full[
  is.na(titanic.full$Age),
  c("Pclass", "Sex", "Fare", "SibSp", "Parch", "Embarked")
]

age.prediction <- predict(age.model, newdata = age.rows)

titanic.full[is.na(titanic.full$Age), "Age"] <- age.prediction

###########################
#Clean missing values of fare
##fare.median <- median(titanic.full$Fare, na.rm = TRUE)
##titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.median
##table(is.na(titanic.full$Fare))
################ NEW WAY ##

#Create a predictive model for missing fare values excluding outliers
#Create a boxplot to find the outlier range
boxplot(titanic.full$Fare)
boxplot.stats(titanic.full$Fare)

## Store the maximum Quartile (upper.whisker) and then filter values
## In subcategory fare to include values that are less than max Q (Include TRUE Values)
upper.whiskher <- boxplot.stats(titanic.full$Fare)$stats[5]
outlier.filter <- titanic.full$Fare < upper.whiskher
##Run Check Test
titanic.full[outlier.filter,]

## Now, create the linear regression model to predict the fare value. Find value of fare using subcats.
## Using data that filters to only include non-outliers

fare.equation = "Fare ~Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.model<-lm(
  formula = fare.equation,
  data = titanic.full[outlier.filter,]
)

## We want to predict the fare model, but we have to isolate the things we want.
## Query only rows missing the value of Fare and only see subcats listed in c.
## Then return the result into fare.row
fare.row <- titanic.full[
  is.na(titanic.full$Fare), 
  c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked")
]
## Predict the Fare value using our linear regression model, using our fare.row data (1 row)
## Then store it in fare prediction
fare.prediction <-predict(fare.model, newdata = fare.row)

#Using the titanic.full data set, query rows of Fare that are missing and put the fare.prediction in
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.prediction



# Categorical Casting
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

#Split dataset back into train and test
train<-titanic.full[titanic.full$Training==TRUE,]
test<-titanic.full[titanic.full$Training==FALSE,]

#

train$Survived <- as.factor(train$Survived)

survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked "
survived.formula <-as.formula(survived.equation)

#Debugging - Turn regression into a binom
train$Survived <- as.character(train$Survived)
train$Survived <- as.factor(train$Survived)
#

install.packages("randomForest")
library(randomForest)


titanic.model <- randomForest(formula = survived.formula, data = train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(test))

features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"

Survived.2 <- predict(titanic.model, newdata = test)

PassengerId <- test$PassengerId
output.df<-as.data.frame(PassengerId)
output.df$Survived <- Survived.2

head(output.df)

write.csv(output.df, file='kaggle_submission.csv', row.names = FALSE)