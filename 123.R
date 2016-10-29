install.packages(c("caret", "kernlab", "lattice", "ggplot2", "gridExtra", "rpart", "rattle", "rpart.plot", "dplyr", "magrittr", "tidyr", "purrr"))
install.packages("e1071")
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


# Load the training dataset
download.file(url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
              destfile = "./pml-training.csv", method = "curl")


data_training <- read.csv("./pml-training.csv", na.strings=c("NA","#DIV/0!",""))


inTrain <- createDataPartition(y=data_training$classe, 
                               p=0.75, list=FALSE)


training <- data_training[inTrain, ]
testing <- data_training[-inTrain, ]
dim(training); dim(testing)

training %>%
  head

summary(data_training$classe)

##To check variables with missing values

apply(data_training,2,function(x) sum(is.na(x)))

### There are unnecessary variables in dataset
training_final <- training[, -c(1:7)]
testing_final <- testing[, -c(1:7)]

#Remove those columns with missing values (except classe column)

training_final_clean  <- training_final[ ,!names(training_final) %in% names(training_final)[apply(training_final, 2, anyNA)]]
testing_final_clean <- testing_final[ , !names(testing_final) %in% names(testing_final)[apply(testing_final, 2, anyNA)]]


names(training_final_clean)


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



## predicting with selected variables
library(rattle)
g <- classe ~ total_accel_dumbbell +magnet_belt_y + accel_arm_z + yaw_dumbbell  + gyros_dumbbell_y + magnet_dumbbell_z + total_accel_dumbbell + accel_arm_z 

model_with_tree1 <- train(g, data = training_final_clean, method = 'rpart')

predicted_outcome1 <- predict(model_with_tree1, testing_final_clean)

outcome_value_with_tree1 <- confusionMatrix(predicted_outcome1, testing_final_clean$classe)$overall[['Accuracy']]
print(outcome_value_with_tree1)


##Decision Tree Model
model_with_tree <- train(classe ~ . , data = training_final_clean, method = 'rpart')

fancyRpartPlot(model_with_tree$finalModel)

predicted_outcome11 <- predict(model_with_tree, training_final_clean)
outcome_value_with_tree11 <- confusionMatrix(predicted_outcome11, training_final_clean$classe)$overall[['Accuracy']]
print(outcome_value_with_tree11)

predicted_outcome22 <- predict(model_with_tree, testing_final_clean)
outcome_value_with_tree22 <- confusionMatrix(predicted_outcome22, testing_final_clean$classe)$overall[['Accuracy']]
print(outcome_value_with_tree22)


install.packages("randomForest")
library(randomForest)

set.seed(777)
model_fit_out <- randomForest(classe ~ ., data = training_final_clean, ntree = 777)

prediction1 <- predict(model_fit_out, training_final_clean, type = "class")
outcome_value_training_1 <- confusionMatrix(prediction1, training_final_clean$classe)$overall[['Accuracy']] 
print(outcome_value_training_1)

prediction2 <- predict(model_fit_out , testing_final_clean, type = "class")
outcome_value_testing_2 <- confusionMatrix(prediction2, testing_final_clean$classe)$overall[['Accuracy']] 
print(outcome_value_testing_2)







install.packages("nnet")
library(nnet)
fit<-nnet(classe ~ ., training_final_clean[,-1], size = 3, rang = 0.1, decay = 5e-4, maxit = 500) 

#import the function from Github
library(devtools)
require(reshape)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

#plot model
plot.nnet(fit)








