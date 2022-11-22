#Requisite and optional libraries to conduct data analysis

library(tidyverse)
library(ggplot2)
library(knitr)
library(caret)
library(rpart)
library(randomForest)
library(httr)

#setting values at 5 decimal places

options(digits = 5)

#Setting working directory to utilize function to read the dataset into R

setwd("~/Capstone/Student performance")

performance <- read.csv("exams.csv")

#basic data analysis and inspection

str(performance)

summary(performance)

head(performance)

#Conversion of all characters into factors for further data analysis and graphing

performance$gender <- as.factor(performance$gender)
performance$race.ethnicity <- as.factor(performance$race.ethnicity)
performance$parental.level.of.education <- as.factor(performance$parental.level.of.education)
performance$lunch <- as.factor(performance$lunch)
performance$test.preparation.course <- as.factor(performance$test.preparation.course)

#Ensuring variables were indeed converted into factors

str(performance)

#Taking basic averages of integer variables to see if the average scores are close together or dispersed

avg.math.score <- mean(performance$math.score)
avg.reading.score <- mean(performance$reading.score)
avg.writing.score <- mean(performance$writing.score)

avg.overall.score <- mean(avg.math.score,avg.reading.score, avg.writing.score)

#Creating basic illustrations to visually inspect data

ggplot(performance, aes(gender)) + geom_bar()

ggplot(performance, aes(race.ethnicity)) + geom_bar()

ggplot(performance, aes(parental.level.of.education)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(performance, aes(lunch)) + geom_bar()

ggplot(performance, aes(test.preparation.course)) + geom_bar()

#Determining if gender leads to a significant difference in who receives subsidized lunch or who took the preparation course

ggplot(performance, aes(lunch)) + geom_bar(aes(fill = gender))

ggplot(performance, aes(test.preparation.course)) + geom_bar(aes(fill = gender))

#Creating illustrations to check if the test preparation course visibly leads to improved scores

ggplot(performance, aes(x = test.preparation.course, y = math.score)) + geom_boxplot()

ggplot(performance, aes(x = test.preparation.course, y = reading.score)) + geom_boxplot()

ggplot(performance, aes(x = test.preparation.course, y = writing.score)) + geom_boxplot()

#Creating illustrations to check if gender visibly leads to different outcomes

ggplot(performance, aes(x = gender, y = math.score)) + geom_boxplot()

ggplot(performance, aes(x = gender, y = reading.score)) + geom_boxplot()

ggplot(performance, aes(x = gender, y = writing.score)) + geom_boxplot()

#Creating illustrations to check if subsidized lunch visibly leads to better scores

ggplot(performance, aes(x = lunch, y = math.score)) + geom_boxplot()

ggplot(performance, aes(x = lunch, y = reading.score)) + geom_boxplot()

ggplot(performance, aes(x = lunch, y = writing.score)) + geom_boxplot()

#Creating our training and test sets

set.seed(99)
series <- createDataPartition(y = performance$test.preparation.course, times = 1, p = 0.3, list = FALSE)
train <- performance[-series,]
test <- performance[series,]

#Building the logistic model using training data

logistic.model <- glm(test.preparation.course ~ math.score + reading.score + writing.score, family = binomial(link = "logit"), data = train)

summary(logistic.model)

#Using the logistic model to make predictions against the test data
  
logistic.model.predictions <- predict(logistic.model, test, type = "response")

logistic.model.results <- ifelse(logistic.model.predictions > 0.5, "completed", "none")

#Using MSE to determine how accurate the model is

misclasserror.logit <- mean(logistic.model.results != test$test.preparation.course)

#Building the Decision Trees model using training data

tree <- rpart(test.preparation.course ~ math.score + reading.score + writing.score, method = "class", data = train)

summary(tree)

#Using decision trees to make predictions against the test data

tree.predictions <- predict(tree, test)

#Exporting predicted results into a data frame

tree.results <- as.data.frame(tree.predictions)

#Creating a decision rule that assigns an observation to a particular outcome  

tree.func <- function(y){
  if (y > 0.5){
    return("completed")
  }else{
    return("none")
  }
}

#Applying this rule to all elements in the tree.results variable

tree.results$completed <- sapply(tree.results$completed, tree.func)

misclasserror.tree <- mean(tree.results$completed != test$test.preparation.course)

#Building the random forest model

rf.model <- randomForest(test.preparation.course ~ math.score + reading.score + writing.score, data = train, importance = T)

rf.model$confusion

rf.model$importance

#Using random forests to make predictions against the test data

rf.predictions <- predict(rf.model, test)

misclasserror.rf <- mean(rf.predictions != test$test.preparation.course)

#final results from all three models

final.results <- data.frame(misclasserror.logit, misclasserror.tree, misclasserror.rf)

final.results




