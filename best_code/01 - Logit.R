rm(list=ls())

library(ROSE)

set.seed(1117)

'%ni%' <- Negate('%in%') 

setwd("/home/usuario/Documentos/R/TFM/")

train <- read.csv("train.csv", sep=",", header = TRUE, dec=".")
test <- read.csv("test.csv", sep=",", header = TRUE, dec=".")

vars <- c("var15_s", "saldo_var30", "num_med_var22_ult3_s", "imp_op_var39_efect_ult3_s", "num_var24",
          "saldo_medio_var5_hace2", "num_ent_var16_ult1_s", "ind_var30_0_s", "num_aport_var13_hace3",
          "saldo_var37", "num_var22_ult3", "saldo_medio_var17_hace2_s", "ind_var13_largo_s", "num_var40_s", 
          "saldo_medio_var44_hace2", "num_sal_var16_ult1", "num_op_var39_efect_ult3", "num_var22_ult1_s", 
          "imp_op_var41_efect_ult1_s", "imp_op_var40_efect_ult1_s", "imp_ent_var16_ult1", "ind_var24")

train$val_cero <- rowSums(train == 0)
test$val_cero <- rowSums(test == 0)

log_vars <- c("saldo_medio_var12_hace2","saldo_medio_var12_hace3","saldo_medio_var12_ult1","saldo_medio_var12_ult3",
              "saldo_medio_var13_corto_hace2","saldo_medio_var13_corto_hace3","saldo_medio_var13_corto_ult1",
              "saldo_medio_var13_corto_ult3","saldo_medio_var13_largo_hace2","saldo_medio_var13_largo_hace3",
              "saldo_medio_var13_largo_ult1","saldo_medio_var13_largo_ult3","saldo_medio_var13_medio_hace2",
              "saldo_medio_var13_medio_hace3","saldo_medio_var13_medio_ult1","saldo_medio_var13_medio_ult3",
              "saldo_medio_var17_hace2","saldo_medio_var17_hace3","saldo_medio_var17_ult1","saldo_medio_var17_ult3",
              "saldo_medio_var29_hace2","saldo_medio_var29_hace3","saldo_medio_var29_ult1","saldo_medio_var29_ult3",
              "saldo_medio_var33_hace2","saldo_medio_var33_hace3","saldo_medio_var33_ult1","saldo_medio_var33_ult3",
              "saldo_medio_var44_hace2","saldo_medio_var44_hace3","saldo_medio_var44_ult1","saldo_medio_var44_ult3",
              "saldo_medio_var5_hace2","saldo_medio_var5_hace3","saldo_medio_var5_ult1","saldo_medio_var5_ult3",
              "saldo_medio_var8_hace2","saldo_medio_var8_hace3","saldo_medio_var8_ult1","saldo_medio_var8_ult3",  
              "saldo_var1","saldo_var12","saldo_var13","saldo_var13_corto","saldo_var13_largo","saldo_var13_medio",
              "saldo_var14","saldo_var17","saldo_var18","saldo_var2_ult1","saldo_var20","saldo_var24","saldo_var25",
              "saldo_var26","saldo_var27","saldo_var28","saldo_var29","saldo_var30","saldo_var31","saldo_var32",  
              "saldo_var33","saldo_var34","saldo_var37","saldo_var40","saldo_var41","saldo_var42","saldo_var44",
              "saldo_var46","saldo_var5","saldo_var6","saldo_var8", "var38") 

for (i in log_vars){
  train[ , i] <- log(train[ , i])
}

for (i in log_vars){
  test[ , i] <- log(test[ , i])
}

for (var in names(train)){
  if (length(unique(train[[var]])) == 1) {
    train[[var]] <- NULL
    test[[var]] <- NULL
  }
}

var_par <- combn(names(train), 2, simplify = F)
to_delete <- c()
for(par in var_par) {
  v1 <- par[1]
  v2 <- par[2]
  
  if (!(v1 %in% to_delete) & !(v2 %in% to_delete)){
    if (all(train[[v1]] == train[[v2]])){
      to_delete <- c(to_delete, v2)
    }
  }
}

set_vars <- setdiff(names(train), to_delete)
train <- train[,-which(names(train) %in% to_delete)]
test <- test[,-which(names(test) %in% to_delete)]

train[train$var3 == -999999, "var3"] <- 37
train[train$var36 == 99, "var36"] <- 4
train[train$delta_imp_aport_var13_1y3 == 9999999999, "delta_imp_aport_var13_1y3"] <- 6
train[train$delta_imp_aport_var17_1y3 == 9999999999, "delta_imp_aport_var17_1y3"] <- 2
train[train$delta_imp_compra_var44_1y3 == 9999999999, "delta_imp_compra_var44_1y3"] <- 7
train[train$delta_imp_reemb_var13_1y3 == 9999999999, "delta_imp_reemb_var13_1y3"] <- 1
train[train$delta_imp_reemb_var17_1y3 == 9999999999, "delta_imp_reemb_var17_1y3"] <- 1
train[train$delta_num_aport_var13_1y3 == 9999999999, "delta_num_aport_var13_1y3"] <- 2
train[train$delta_num_aport_var17_1y3 == 9999999999, "delta_num_aport_var17_1y3"] <- 3
train[train$delta_num_compra_var44_1y3 == 9999999999, "delta_num_compra_var44_1y3"] <- 4
train[train$delta_num_compra_var44_1y3 == 4, "delta_num_compra_var44_1y3"] <- 3
train[train$delta_num_reemb_var13_1y3 == 9999999999, "delta_num_reemb_var13_1y3"] <- 1
train[train$delta_num_reemb_var17_1y3 == 9999999999, "delta_num_reemb_var17_1y3"] <- 1
train <- do.call(data.frame,lapply(train, function(x) replace(x, is.infinite(x), NA)))
train[is.na(train)] <- 0

train <- ROSE(TARGET ~ ., data=train)$data
TARGET <- train$TARGET
train.s <- data.frame(scale(train))
colnames(train.s) <- paste0(colnames(train.s),"_s")
train <- dplyr::bind_cols(train, train.s)

testID <- test$ID

test[test$var3 == -999999, "var3"] <- 37
test[test$var36 == 99, "var36"] <- 4
test[test$delta_imp_aport_var13_1y3 == 9999999999, "delta_imp_aport_var13_1y3"] <- 6
test[test$delta_imp_aport_var17_1y3 == 9999999999, "delta_imp_aport_var17_1y3"] <- 2
test[test$delta_imp_compra_var44_1y3 == 9999999999, "delta_imp_compra_var44_1y3"] <- 7
test[test$delta_imp_reemb_var13_1y3 == 9999999999, "delta_imp_reemb_var13_1y3"] <- 1
test[test$delta_imp_reemb_var17_1y3 == 9999999999, "delta_imp_reemb_var17_1y3"] <- 1
test[test$delta_num_aport_var13_1y3 == 9999999999, "delta_num_aport_var13_1y3"] <- 2
test[test$delta_num_aport_var17_1y3 == 9999999999, "delta_num_aport_var17_1y3"] <- 3
test[test$delta_num_compra_var44_1y3 == 9999999999, "delta_num_compra_var44_1y3"] <- 4
test[test$delta_num_reemb_var13_1y3 == 9999999999, "delta_num_reemb_var13_1y3"] <- 1
test[test$delta_num_reemb_var17_1y3 == 9999999999, "delta_num_reemb_var17_1y3"] <- 1
test <- do.call(data.frame,lapply(test, function(x) replace(x, is.infinite(x), NA)))
test[is.na(test)] <- 0
test.s <- data.frame(scale(test))

colnames(test.s) <- paste0(colnames(test.s),"_s")
test <- dplyr::bind_cols(test, test.s)

train <- train[,which(names(train) %in% vars)]
test <- test[,which(names(test) %in% vars)]

train$TARGET <- TARGET
logitmod <- glm(as.formula(paste("TARGET ~ ", paste(vars, collapse=" + "))), 
                family = binomial, data = train)
predictions <- predict(logitmod, newdata=test, type="response") 
y_pred_num <- ifelse(predictions > 0.5, 1, 0)
table(y_pred_num)

write.csv(data.frame(ID=testID, TARGET=predictions), 
          file = "Logit_ROSE_binded_22vars.csv",
          row.names=FALSE)

