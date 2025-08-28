###########Packages######
library(ggpubr)
library(tidyverse)
library("readxl")
library(ggplot2)
library(dplyr)
###########EDA######
happy_df <- read.csv("C:/Users/Artrix/Downloads/World_Happiness_Report_2022.csv")

happy_df <- read_csv(file.choose())
boxplot(happy_df$Perceptions.of.corruption, main="Perceptions of corruption", horizontal = TRUE,xlab="Score")
boxplot(happy_df$Healthy.life.expectancy, main="Healthy life expectancy", horizontal = TRUE,xlab="Score")
boxplot(happy_df$Social.support, main="Social support", horizontal = TRUE,xlab="Score")
boxplot(happy_df$GDP.per.capita, main="GDP per capita", horizontal = TRUE,xlab="Score")
boxplot(happy_df$Generosity, main="Generosity", horizontal = TRUE,xlab="Score")
boxplot(happy_df$Freedom.to.make.life.choices, main="Freedom to Make Life Choices", horizontal = TRUE,xlab="Score")

hist()

hist(happy_df$Happiness.score,
     main="Happiness Score Observations", xlab="Happiness Score")

###########First######
happy_df["Happiness.score"][happy_df["Happiness.score"] < 5.5] <- "0"
happy_df["Happiness.score"][happy_df["Happiness.score"] >= 5.5] <- "1"
happy_df$Happiness.score<- as.factor(happy_df$Happiness.score)

subset <- sample(nrow(happy_df), nrow(happy_df) *0.80)
happy_train = happy_df[subset, ] #training data
happy_test = happy_df[-subset, ] #testing data

model_1 <- glm(`Happiness.score` ~ `GDP.per.capita`, family=binomial, data=happy_train)

ggplot(happy_train, aes(`GDP.per.capita`, `Happiness.score`)) + 
  geom_point() + 
  stat_smooth(method = lm, se = FALSE)

summary(model_1)
AIC(model_1)

model_2 <- glm(`Happiness.score` ~ `GDP.per.capita` + `Social.support` + `Healthy.life.expectancy` + `Freedom.to.make.life.choices` + `Generosity` + `Perceptions.of.corruption`, family=binomial, data=happy_train)

summary(model_2)
AIC(model_2)
###########Step######
library(leaps)

model.step <- step(model_2)
#GDP.per.capita has been removed
AIC(model.step)
summary(model.step)

pred_glm0_train <- predict(model.step, type="response")

library(ROCR)
pred <- prediction(pred_glm0_train, happy_train$Happiness.score)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)

#Get the AUC
unlist(slot(performance(pred, "auc"), "y.values"))

table(happy_train$Happiness.score, (pred_glm0_train > 0.9)*1, dnn=c("Truth","Predicted"))
table(happy_train$Happiness.score, (pred_glm0_train > 0.5)*1, dnn=c("Truth","Predicted"))
table(happy_train$Happiness.score, (pred_glm0_train > 0.1)*1, dnn=c("Truth","Predicted"))

#Symmetric cost
cost1 <- function(r, pi, pcut){
  mean(((r==0)&(pi>pcut)) | ((r==1)&(pi<pcut)))
}

pcut = 0.9
cost1(r = happy_train$Happiness.score, pi = pred_glm0_train, pcut)
pcut = 0.5
cost1(r = happy_train$Happiness.score, pi = pred_glm0_train, pcut)
pcut = 0.1
cost1(r = happy_train$Happiness.score, pi = pred_glm0_train, pcut)

#pcut = 0.5 is the best


#predicing test data
pred_glm0_test <- predict(model.step, newdata = happy_test, type="response")

pred <- prediction(pred_glm0_test, happy_test$Happiness.score)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
unlist(slot(performance(pred, "auc"), "y.values"))


pcut <- 0.9  
cost1(r = happy_test$Happiness.score, pi = pred_glm0_test, pcut)
pcut = 0.5
cost1(r = happy_test$Happiness.score, pi = pred_glm0_test, pcut)
pcut = 0.1
cost1(r = happy_test$Happiness.score, pi = pred_glm0_test, pcut)
###########Cross######
library(boot)

model_g <- glm(Happiness.score ~ `GDP.per.capita` + `Social.support` + `Freedom.to.make.life.choices` + `Perceptions.of.corruption`, family=binomial, data=happy_df)

pcut <- .5
costfunc <- function(obs, pred.p){
  weight1 <- 1 # define the weight for "true=1 but pred=0" (FN)
  weight0 <- 1 # define the weight for "true=0 but pred=1" (FP)
  pcut <- 1/(1+weight1/weight0)
  c1 <- (obs==1)&(pred.p < pcut) # count for "true=1 but pred=0" (FN)
  c0 <- (obs==0)&(pred.p >= pcut) # count for "true=0 but pred=1" (FP)
  cost <- mean(weight1*c1 + weight0*c0) # misclassification with weight
  return(cost) # you have to return to a value when you write R functions
} # end

cv.glm(data = happy_df, glmfit = model_g, cost=costfunc, K = 10)$delta[2]
###########Random Forest######
library(randomForest)

str(happy_train)
happy_train$Happiness.score = as.factor(happy_train$Happiness.score)



model_rf_1 <- randomForest(Happiness.score ~ `GDP.per.capita` + `Social.support` + `Healthy.life.expectancy` + `Freedom.to.make.life.choices` + `Generosity` + `Perceptions.of.corruption`, data=happy_train, ntree=500, proximity=TRUE)

model_rf_1
varImpPlot(model_rf_1)
importance(model_rf_1)
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model_rf_1$err.rate), times=3),
  Type=rep(c("OOB", "Happy", "Unhappy"), each=nrow(model_rf_1$err.rate)),
  Error=c(model_rf_1$err.rate[,"OOB"], 
          model_rf_1$err.rate[,"Happy"], 
          model_rf_1$err.rate[,"Unhappy"]))
plot()
ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(Happiness.score ~ `GDP.per.capita` + `Social.support` + `Healthy.life.expectancy` + `Freedom.to.make.life.choices`, data=happy_train, mtry=i, ntree=500)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values
## find the minimum error
min(oob.values)
## find the optimal value for mtry...
which(oob.values == min(oob.values))

model_rf_2 <- randomForest(Happiness.score ~ `GDP.per.capita` + `Social.support` + `Healthy.life.expectancy` + `Freedom.to.make.life.choices` + `Generosity` + `Perceptions.of.corruption`, 
                           data=happy_train,
                           ntree=500, 
                           proximity=TRUE,
                           )

model_rf_2

varImpPlot(model_rf_2)

model_rf_3 <- randomForest(Happiness.score ~ `GDP.per.capita` + `Social.support` + `Healthy.life.expectancy` + `Freedom.to.make.life.choices`, 
                           data=happy_train,
                           ntree=500, 
                           proximity=TRUE,
                           mtry=3)
model_rf_3

happy_test["Happiness.score"][happy_test["Happiness.score"] >= 5.5] <- "Happy"
happy_test["Happiness.score"][happy_test["Happiness.score"] < 5.5] <- "Unhappy"

happy_test$Happiness.score = as.factor(happy_test$Happiness.score)

Pred2 <- predict(model_rf_2, newdata=happy_test)
table(Pred2, happy_test$Happiness.score)

confusionMatrix(Pred, happy_test$Happiness.score)

library(caret)
