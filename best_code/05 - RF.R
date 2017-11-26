rm(list=ls())

library(caret)

set.seed(1117)

'%ni%' <- Negate('%in%') 

#registerDoMC(cores=8)

setwd("/home/usuario/Documentos/R/TFM/")

train <- read.csv("train.csv", sep=",", header = TRUE, dec=".")
test <- read.csv("test.csv", sep=",", header = TRUE, dec=".")

TARGET <- train$TARGET
testID <- test$ID

train$val_cero <- rowSums(train == 0)
test$val_cero <- rowSums(test == 0)

vars<- c("var15", "saldo_var30", "num_var22_ult3", "num_meses_var5_ult3", "val_cero", 
         "ind_var30_0", "num_aport_var13_hace3", "var38", "num_var22_ult1", "saldo_var5",
         "num_var35", "ind_var41_0", "saldo_medio_var5_ult3", "ind_var43_recib_ult1", 
         "ind_var43_emit_ult1", "num_var5", "ind_var37_cte", "num_var41_0", "num_var14",
         "saldo_medio_var12_hace2", "ind_var37_0", "ind_var8_0", "ind_var30", 
         "num_op_var39_comer_ult1", "num_var4", "imp_aport_var17_ult1", "ind_var19",
         "num_meses_var8_ult3", "imp_ent_var16_ult1", "num_op_var41_comer_ult1", 
         "imp_op_var41_ult1", "num_meses_var44_ult3", "ind_var14",  "ind_var9_ult1", 
         "saldo_medio_var8_ult3", "saldo_medio_var8_hace2", "imp_op_var41_comer_ult3", 
         "delta_imp_aport_var17_1y3")  

train <- train[,which(names(train) %in% vars)]
test <- test[,which(names(test) %in% vars)]


ctrl <- trainControl(method="none", classProbs=TRUE,
                     summaryFunction=multiClassSummary)

grid <- expand.grid(mtry = c(12), splitrule="gini")

train$TARGET <- make.names(TARGET)

parRF_mod <- train(as.formula(paste(paste("TARGET ~", paste(vars, collapse=" + ")),sep = "")), data=train, method= "ranger",
                   metric="ROC", tuneGrid=grid, trControl=ctrl)

predictions <- predict(parRF_mod, test)

predictions <- as.numeric(predictions)
predictions[predictions==1] <- 0
predictions[predictions==2] <- 1

table(predictions)

write.csv(data.frame(ID=testID, TARGET=predictions), 
          file = "RF_descompensado_39vars_mtry12.csv",
          row.names=FALSE)

