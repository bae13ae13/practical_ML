

library(sm)
library(caret)
library(kernlab)
library(lattice)
library(ggplot2)
library(e1071)
library(Hmisc)
library(gridExtra)
library(RANN)
library(rpart)
library(rattle)
library(rpart.plot)
library(purrr)
library(tidyr)
library(magrittr)
library(dplyr)
library(ElemStatLearn)
library(stats)

## Analysis Plan: 0. transform data 1. make cluster 2. derive variable 3. do prediction 
## CRISP- DM: cleanning variable + feature selection + 

data_training <- read.csv("C:/Users/wkim/Downloads/pml-training.csv")

## to check missing value in variables from dataset
apply(data_training,2,function(x) sum(is.na(x)))



inTrain <- createDataPartition(y=data_training$classe, 
                               p=0.75, list=FALSE)

training <- data_training[inTrain, ]
testing <- data_training[-inTrain, ]
dim(training); dim(testing)

training %>%
  head

summary(data_training$classe)
summary(data_training$user_name)
names(data_training)


## Data Exploration 
qplot(magnet_dumbbell_z, total_accel_dumbbell, colour=classe,data=training)

qplot(max_yaw_belt, magnet_dumbbell_z, colour=classe,data=training)


# plot densities

sm.density.compare(training$magnet_dumbbell_z, training$classe, xlab="magnet_dumbbell_z")
title(main="magnet_dumbbell_z by classe")

sm.density.compare(training$magnet_arm_z, training$classe, xlab="magnet_dumbbell_z")
title(main="magnet_dumbbell_z by classe")


histogram(~magnet_dumbbell_z|classe,data=training,
          type="density",
          xlab="Speed of Motion",
          main="Magnet Dumbbell_z by classe",
          breaks=30)

histogram(~magnet_arm_z|classe,data=training,
          type="density",
          xlab="Speed of Motion",
          main="Magnet Dumbbell_z by classe",
          breaks=30)



################ TREE package
library(tree)
########## Selected variable based on plotting and missing value checking tasks
model_with_vars <- classe ~ total_accel_dumbbell + magnet_dumbbell_z + gyros_dumbbell_y 

tree_1 = tree(model_with_vars, data=training)
summary(tree_1)
plot(tree_1); text(tree_1)


s1 <- predict(tr, newdata=testing)
s2 <- predict(tr, newdata=data_testing)



