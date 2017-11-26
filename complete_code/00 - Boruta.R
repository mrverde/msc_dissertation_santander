######################### 00 - BORUTA #########################
library(Boruta)

bind_rose$TARGET <- as.factor(bind_rose$TARGET)
borutaMod <- Boruta(TARGET ~ ., data=bind_rose, doTrace=1)

bind_up$TARGET <- as.factor(bind_up$TARGET)
borutaMod_up <- Boruta(TARGET ~ ., data=bind_up, doTrace=1)

bind_smote$TARGET <- as.factor(bind_smote$TARGET)
borutaMod_smote <- Boruta(TARGET ~ ., data=bind_smote, doTrace=1)

bind_rose$TARGET <- as.factor(bind_rose$TARGET)
borutaMod_rose <- Boruta(TARGET ~ ., data=bind_rose, doTrace=1)


roughFixMod <- TentativeRoughFix(borutaMod)
boruta_signif <- getSelectedAttributes(roughFixMod, withTentative=TRUE)
boruta_signif
imps <- attStats(roughFixMod)
imps

pdf("outputboruta.pdf",width=8,height=5,paper='special') 
plot(roughFixMod, cex.axis=.7, las=2, xlab="", ylab="",
     main="", xaxt="n",outline=FALSE)
dev.off()



bind_down$TARGET <- TARGET_down
output_bind_down_logit <- recursive_logit(data.frame(bind_down), TARGET, bind_validation, TARGET_validation, 70)

bind_up$TARGET <- TARGET_up
output_bind_up_logit <- recursive_logit(data.frame(bind_up), TARGET, bind_validation, TARGET_validation, 70)

bind_smote$TARGET <- TARGET_smote
output_bind_smote_logit <- recursive_logit(data.frame(bind_smote), TARGET, bind_validation, TARGET_validation, 70)

bind_rose$TARGET <- TARGET_rose
output_bind_rose_logit <- recursive_logit(data.frame(bind_rose), TARGET, bind_validation, TARGET_validation, 70)



#RANDOM FOREST
library(caret)

vars_boruta <- c("ind_var13_corto_0", "ind_var14_0", "ind_var26_0", "ind_var25", "num_var1", "num_var37", 
                 "num_var39_0", "num_var41_0", "num_var42_0", "saldo_var25", "saldo_var31", "saldo_var42", 
                 "delta_imp_aport_var13_1y3", "delta_num_aport_var13_1y3", "delta_num_reemb_var13_1y3", 
                 "num_op_var39_efect_ult1", "num_op_var39_efect_ult3", "num_reemb_var17_ult1", "num_sal_var16_ult1", 
                 "saldo_medio_var8_ult3", "saldo_medio_var13_corto_hace2", "imp_op_var39_comer_ult1_s", 
                 "imp_op_var39_comer_ult3_s", "ind_var14_0_s", "ind_var26_0_s", "ind_var25_0_s", "ind_var25_s", 
                 "ind_var37_0_s", "num_var1_s", "num_var37_s", "saldo_var25_s", "saldo_var31_s", "saldo_var42_s", 
                 "delta_imp_aport_var13_1y3_s", "delta_num_aport_var13_1y3_s", "delta_num_reemb_var13_1y3_s", 
                 "num_meses_var5_ult3_s", "num_reemb_var17_ult1_s", "num_sal_var16_ult1_s", "num_var45_ult1_s")   

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
ncol(train)

grid <- expand.grid(mtry = c(3, 4, 13))

train_RF$TARGET <- TARGET
train_RF$TARGET <- make.names(train_RF$TARGET)

parRF_mod <- train(as.formula(paste(paste("TARGET ~", paste(vars_logit, collapse=" + ")),sep = "")), data=train_RF, method= "parRF",
                     metric="ROC", tuneGrid=grid, trControl=ctrl)


pred <- predict(parRF_mod, validation)
confusionMatrix(pred, validation$Class)




#### XG BOOST
library(xgboost)

ROC_val <- c()
ROC_var <- c()
for(i in 1:length(train_rose.s)){
  train_xg <- xgb.DMatrix(data.matrix(train_rose.s[,i]),
                          label = as.numeric(TARGET_rose))
  
  test_xg <- xgb.DMatrix(data.matrix(validation.s))
  watchlist <- list(train = train_xg, test = test_xg)
  param <- list("objective" = "binary:logistic", "eval_metric" = "auc")
  cv.nround <- 10
  cv.nfold <- 3
  cvMod <- xgb.cv(param=param, data=train_xg, nfold =cv.nfold,
                  nrounds = cv.nround)
  nrounds <- 50
  xgMod <- xgb.train(param=param, data=train_xg, nrounds=nrounds,
                     booster="gblinear",  maximize = FALSE, verbose = 1)
  
  pred <- predict(xgMod, test_xg)
  #hist(pred)
  y_pred_num <- ifelse(pred > 0.5, 1, 0)                       #Cambia la probabilidad de prediccion a 0 y 1
  y_pred <- factor(y_pred_num, levels=c(0,1)) 
  table(y_pred)
  print(InformationValue::AUROC(TARGET_validation,pred))
  ROC_val[i] <- InformationValue::AUROC(TARGET_validation,pred)
  ROC_var[i] <- colnames(bind_rose)[i]
  
}
superROC <- data.frame(var=ROC_var, ROC=ROC_val)

pred <- predict(xgMod, test_final.s)


param <- list( "objective" = "binary:logistic", "eval_metric" = "auc", "eta" = 0.0202,
               "max_depth" = 5, "subsample" = 0.6815, "colsample_bytree" = 0.701)
clf <- xgb.train( param = param, data = train_xg,  nrounds = 560, verbose = 1, watchlist = watchlist,
                  booster = "gbtree",  maximize = FALSE)
