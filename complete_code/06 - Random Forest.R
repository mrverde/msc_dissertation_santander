######################### 05 - Random Forest #########################
#Borramos la memoria
rm(list=ls())

require(foreach)
library(caret)
#library(doMC)

set.seed(1117)

'%ni%' <- Negate('%in%') 

#registerDoMC(cores=8)

setwd("/home/usuario/Documentos/R/TFM/")

train <- read.csv("train.csv", sep=",", header = TRUE, dec=".")
test <- read.csv("test.csv", sep=",", header = TRUE, dec=".")

train$val_cero <- rowSums(train == 0)
test$val_cero <- rowSums(test == 0)

vars_boruta <- c("ind_var13_corto_0", "ind_var14_0", "ind_var26_0", "ind_var25", "num_var1", "num_var37", 
                 "num_var39_0", "num_var41_0", "num_var42_0", "saldo_var25", "saldo_var31", "saldo_var42", 
                 "delta_imp_aport_var13_1y3", "delta_num_aport_var13_1y3", "delta_num_reemb_var13_1y3", 
                 "num_op_var39_efect_ult1", "num_op_var39_efect_ult3", "num_reemb_var17_ult1", "num_sal_var16_ult1", 
                 "saldo_medio_var8_ult3", "saldo_medio_var13_corto_hace2", "imp_op_var39_comer_ult1", 
                 "imp_op_var39_comer_ult3", "ind_var14_0", "ind_var26_0", "ind_var25_0", "ind_var25", 
                 "ind_var37_0", "num_var1", "num_var37", "saldo_var25", "saldo_var31", "saldo_var42", 
                 "delta_imp_aport_var13_1y3", "delta_num_aport_var13_1y3", "delta_num_reemb_var13_1y3", 
                 "num_meses_var5_ult3", "num_reemb_var17_ult1", "num_sal_var16_ult1", "num_var45_ult1")   

vars_logit <- c("var15", "saldo_var30", "num_var22_ult3", "num_meses_var5_ult3", "val_cero", 
                "ind_var30_0", "num_aport_var13_hace3", "var38", "num_var22_ult1", "saldo_var5",
                "num_var35", "ind_var41_0", "saldo_medio_var5_ult3", "ind_var43_recib_ult1", 
                "ind_var43_emit_ult1", "num_var5", "ind_var37_cte", "num_var41_0", "num_var14",
                "saldo_medio_var12_hace2", "ind_var37_0", "ind_var8_0", "ind_var30", 
                "num_op_var39_comer_ult1", "num_var4", "imp_aport_var17_ult1", "ind_var19",
                "num_meses_var8_ult3", "imp_ent_var16_ult1", "num_op_var41_comer_ult1", 
                "imp_op_var41_ult1", "num_meses_var44_ult3", "ind_var14",  "ind_var9_ult1", 
                "saldo_medio_var8_ult3", "saldo_medio_var8_hace2", "imp_op_var41_comer_ult3", 
                "delta_imp_aport_var17_1y3")  


train_RF <- train[,which(names(train) %in% vars_logit)]

train_RF2 <- train[,which(names(train) %in% vars_boruta)]




ctrl <- trainControl(method="repeatedcv", repeats=10, classProbs=TRUE,
                     summaryFunction=multiClassSummary)
#Vemos también el número de columnas
ncol(train_RF)

grid <- expand.grid(mtry = c(3, 6, 12), splitrule="gini") #

train_RF$TARGET <- make.names(train$TARGET)
train_RF2$TARGET <- make.names(train$TARGET)


parRF_mod <- train(as.formula(paste(paste("TARGET ~", paste(colnames(train_RF)[1:(length(train_RF)-1)], collapse=" + ")),sep = "")), data=train_RF, method= "ranger",
                   metric="AUC", tuneGrid=grid, trControl=ctrl)

parRF_mod2 <- train(as.formula(paste(paste("TARGET ~", paste(colnames(train_RF2)[1:(length(train_RF2)-1)], collapse=" + ")),sep = "")), data=train_RF2, method= "ranger",
                   metric="AUC", tuneGrid=grid, trControl=ctrl)

testID <- test$ID
test1 <- test[,which(names(test) %in% colnames(train_RF))]
test2 <- test[,which(names(test) %in% colnames(train_RF2))]


pred1 <- predict(parRF_mod, test1)

pred2 <- predict(parRF_mod2, test2)

table(pred1)
table(pred2)


predictions <- as.numeric(pred1)
predictions[predictions==1] <- 0
predictions[predictions==2] <- 1


write.csv(data.frame(ID=testID, TARGET=predictions), 
          file = "RF_descompensado_39vars.csv",
          row.names=FALSE)
