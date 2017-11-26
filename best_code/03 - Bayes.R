rm(list=ls())

library(caret)
library(DMwR)
library(naivebayes)

set.seed(1117)

'%ni%' <- Negate('%in%') 

setwd("/home/usuario/Documentos/R/TFM/")

train <- read.csv("train.csv", sep=",", header = TRUE, dec=".")
test <- read.csv("test.csv", sep=",", header = TRUE, dec=".")
train <- train
testID <- test$ID
TARGET <- train$TARGET

vars <- c("ind_var30", "num_var37_med_ult2", "num_op_var40_efect_ult1", "var15", 
          "imp_op_var40_ult1", "ind_var5_0", "num_var45_hace3", "var38")

rows_train <- createDataPartition(train$TARGET, p=1, list=FALSE)
train <- train[rows_train, ]
train$TARGET <- as.factor(TARGET)
train <- SMOTE(TARGET ~ train, train, perc.over = 100, perc.under=200)
TARGET <- train$TARGET
train <- train[,-which(names(train) %in% c("ID", "TARGET"))]

train <- data.frame(scale(train))
test <- data.frame(scale(test))

train <- train[,which(names(train) %in% vars)]
test <- test[,which(names(test) %in% vars)]

nb_mod <- naive_bayes(x=train, y=TARGET)

pred_final <- predict(nb_mod, newdata=test)
pred_final <- as.numeric(pred_final)
pred_final[pred_final==1] <- 0
pred_final[pred_final==2] <- 1

write.csv(data.frame(ID=testID, TARGET=pred_final), 
          file = "Bayes_SMOTE_7vars.csv",
          row.names=FALSE)