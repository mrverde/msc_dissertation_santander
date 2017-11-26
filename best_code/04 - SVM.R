rm(list=ls())

library(caret)
library(DMwR)

set.seed(1117)

'%ni%' <- Negate('%in%') 

setwd("/home/usuario/Documentos/R/TFM/")

train <- read.csv("train.csv", sep=",", header = TRUE, dec=".")
test <- read.csv("test.csv", sep=",", header = TRUE, dec=".")
train <- train
testID <- test$ID
TARGET <- train$TARGET

vars <- c("num_var4", "saldo_var40", "ind_var41_0", "ind_var37_cte", 
  "ind_var1_0", "imp_op_var40_ult1", "imp_sal_var16_ult1", 
  "ind_var17", "num_var24", "ind_var1", "ind_var40", "ind_var24")

train <- downSample(x=train[,colnames(train) %ni% "TARGET"], 
                    y=as.factor(train$TARGET), yname="TARGET")
TARGET <- train$TARGET
train <- train[,-which(names(train) %in% c("ID", "TARGET"))]
train <- data.frame(scale(train))
test <- data.frame(scale(test))


train <- train[,which(names(train) %in% vars)]
test <- test[,which(names(test) %in% vars)]

ctrl <- trainControl(method = "none", search="grid", allowParallel = TRUE, 
                     summaryFunction=twoClassSummary, classProbs=TRUE)
grid <- expand.grid(C=c(1), scale=c(0.1), degree=c(3))
train$TARGET <- make.names(TARGET)
mod <- train(TARGET ~ ., data=train, method="svmPoly", metric="ROC", 
             tuneGrid = grid, trControl=ctrl)

predictions <- predict(mod, test)
predictions <- as.numeric(predictions)
predictions[predictions==1] <- 0
predictions[predictions==2] <- 1
table(predictions)

write.csv(data.frame(ID=testID, TARGET=predictions), 
          file = "SVM_downsampling_12vars_C1_sc0.1_d3.csv",
          row.names=FALSE)
