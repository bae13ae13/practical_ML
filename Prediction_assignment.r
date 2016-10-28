

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
apply(data_training,2,function(x) sum(is.na(x)))
## to check missing value in variables from dataset



nrow(as.data.frame(g))
b <- names(as.list(g))
k<- c()

for (i in 1:160) {
  if (b[i] == 0) {
    k[i] <- names(b[i])
  } 
  else {next}
}

a <- drop_na(as.data.frame(k))


data_training <- data_training %>%
  filter() %>%
  head

  
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


data(iris)
library(ggplot2); library(rpart)
names(iris)

inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain, ]; testing <- iris[-inTrain,]
dim(training)
dim(testing)

qplot(Petal.Width,Sepal.Width,colour=Species,data=training)



modFit <- train(Species ~ ., method="rpart", data=training)
print(modFit$finalModel)

plot(modFit$finalModel, uniform=TRUE,
     main="Classfication Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

install.packages("rpart.plot")

library(caret)
library(rattle)
library(rpart.plot)
fancyRpartPlot(modFit$finalModel)


predict(modFit, newdata=testing)


names(training)
dim(training)
dim(testing)

summary(training$classe)
training@classe

is.na(training$classe[1:14718])
apply(training,2,function(x) sum(is.na(x)))
class(training$classe)
frequency(training$classe[1:14718])
summary(training$classe)

modFit_cla <- train(classe ~ total_accel_dumbbell +magnet_belt_y + accel_arm_z + yaw_dumbbell + var_total_accel_belt
                    + gyros_dumbbell_y + magnet_dumbbell_z + total_accel_dumbbell + accel_arm_z + max_yaw_dumbbell, method="class", data=training)
print(modFit_cla$finalModel)
train()

plot(modFit$finalModel, uniform=TRUE,
     main="Classfication Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

install.packages("rpart.plot")

library(caret)
library(rattle)
library(rpart.plot)
fancyRpartPlot(modFit$finalModel)


###############
# TREE package
library(tree)

e <- classe ~ total_accel_dumbbell + magnet_dumbbell_z + gyros_dumbbell_y 

tr = tree(e, data=training)
summary(tr)
plot(tr); text(tr)


library(caret)
library(rattle)
library(rpart.plot)

s1 <- predict(tr, newdata=testing)
s2 <- predict(tr, newdata=data_testing)



data_testing <- read.csv("C:/Users/wkim/Downloads/pml-testing.csv")

library(neuralnet)
n <- names(a)
w <- a[3:92]
f <- as.formula(paste("classe ~", paste(w[!w %in% "classe"], collapse = " + ")))

m <- model.matrix( 
   ~ classe + total_accel_dumbbell +magnet_belt_y + accel_arm_z + yaw_dumbbell + var_total_accel_belt
    + gyros_dumbbell_y + magnet_dumbbell_z + total_accel_dumbbell + accel_arm_z + max_yaw_dumbbell, 
  data = training 
)

nn <- neuralnet(classe ~ total_accel_dumbbell +magnet_belt_y + accel_arm_z + yaw_dumbbell + var_total_accel_belt
                + gyros_dumbbell_y + magnet_dumbbell_z + total_accel_dumbbell + accel_arm_z + max_yaw_dumbbell,data=m,hidden=c(5,3),linear.output=T)

plot(nn)



library(ggplot2); library(rpart)
qplot(magnet_belt_y, accel_arm_z, colour=classe,data=training)
qplot(roll_dumbbell, yaw_dumbbell, colour=classe,data=training)
qplot(roll_dumbbell, total_accel_belt, colour=classe,data=training)

qplot(total_accel_dumbbell, total_accel_belt, colour=classe,data=training)
qplot(total_accel_arm, total_accel_forearm, colour=classe,data=training)

qplot(total_accel_arm, total_accel_belt, colour=classe,data=training)
qplot(total_accel_arm, total_accel_dumbbell, colour=classe,data=training)
qplot(gyros_dumbbell_y, var_total_accel_belt, colour=classe,data=training)

## !!!!
qplot(gyros_dumbbell_y, magnet_dumbbell_z, colour=classe,data=training)
## !!!!
qplot(magnet_dumbbell_z, total_accel_dumbbell, colour=classe,data=training)

qplot(gyros_dumbbell_y, total_accel_dumbbell, colour=classe,data=training)


qplot(max_yaw_dumbbell, accel_arm_z, colour=classe,data=training)


qplot(magnet_arm_z, max_yaw_belt, colour=classe,data=training)
qplot(max_yaw_belt, magnet_dumbbell_z, colour=classe,data=training)


hist(training$magnet_dumbbell_z, main="",breaks =30, xlab="ave. capital run length")

class(training$max_yaw_dumbbell)
summary(training$max_yaw_dumbbell)

training$classe


# plot densities  gyros_dumbbell_y total_accel_dumbbell magnet_arm_z

sm.density.compare(training$magnet_dumbbell_z, training$classe, xlab="magnet_dumbbell_z")
title(main="magnet_dumbbell_z by classe")

sm.density.compare(training$total_accel_dumbbell, training$classe, xlab="magnet_dumbbell_z")
title(main="magnet_dumbbell_z by classe")

sm.density.compare(training$gyros_dumbbell_y, training$classe, xlab="magnet_dumbbell_z")
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

histogram(~total_accel_dumbbell|classe,data=training,
          type="density",
          xlab="Speed of Motion",
          main="Magnet Dumbbell_z by classe",
          breaks=30)

histogram(~gyros_dumbbell_y|classe,data=training,
          type="density",
          xlab="Speed of Motion",
          main="Magnet Dumbbell_z by classe",
          breaks=20)



modFit <- train(classe ~ var_total_accel_belt + total_accel_arm + total_accel_dumbbell + total_accel_forearm, method="rpart", data=training)
print(modFit$finalModel)


plot(modFit$finalModel, uniform=TRUE,
     main="Classfication Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)


## var_total_accel_belt total_accel_belt total_accel_arm total_accel_dumbbell total_accel_forearm

## Predicting with tree

library(ggplot2); library(rpart)
names(iris)

inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain, ]; testing <- iris[-inTrain,]
dim(training)
dim(testing)

qplot(Petal.Width,Sepal.Width,colour=Species,data=training)


modFit <- train(Species ~ ., method="rpart", data=training)
print(modFit$finalModel)


plot(modFit$finalModel, uniform=TRUE,
     main="Classfication Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)


library(caret)
library(rattle)
library(rpart.plot)
fancyRpartPlot(modFit$finalModel)


predict(modFit, newdata=testing)


## material codes
table(training$jobclass)

dummies <- dummyVars(wage ~ jobclass, data=training)
head(predict(dummies,newdata=training))
names(training)

nsv <- nearZeroVar(training,saveMetrics=TRUE)
nsv