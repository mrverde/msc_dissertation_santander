######  CODIGO QUE APARECERA TFM

######################### 01 - CODIGO BASE #########################

rm(list=ls())

library(doMC)
library(caret)
library(Boruta)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(gridExtra)
library(DMwR)
library(ROSE)

set.seed(1117)

'%ni%' <- Negate('%in%') 

registerDoMC(cores=8)

datos <- read.csv("train.csv", sep=",", header = TRUE, dec=".")
test_final <- read.csv("test.csv", sep=",", header = TRUE, dec=".")

datosbackup <- datos
test_finalbackup <- test_final

datos$val_cero <- rowSums(datos == 0)
test_final$val_cero <- rowSums(test_final == 0)

fin_db <- datos

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
  fin_db[ , i] <- log(fin_db[ , i])
}

for (var in names(fin_db)){
  if (length(unique(fin_db[[var]])) == 1) {
    fin_db[[var]] <- NULL
    test_final[[var]] <- NULL
  }
}

var_par <- combn(names(fin_db), 2, simplify = F)
to_delete <- c()
for(par in var_par) {
  v1 <- par[1]
  v2 <- par[2]
  
  if (!(v1 %in% to_delete) & !(v2 %in% to_delete)){
    if (all(fin_db[[v1]] == fin_db[[v2]])){
      to_delete <- c(to_delete, v2)
    }
  }
}

set_vars <- setdiff(names(fin_db), to_delete)
fin_db <- fin_db[,-which(names(fin_db) %in% to_delete)]
test_final <- test_final[,-which(names(test_final) %in% to_delete)]

fin_db[fin_db$var3 == -999999, "var3"] <- 37
fin_db[fin_db$var36 == 99, "var36"] <- 4
fin_db[fin_db$delta_imp_aport_var13_1y3 == 9999999999, "delta_imp_aport_var13_1y3"] <- 6
fin_db[fin_db$delta_imp_aport_var17_1y3 == 9999999999, "delta_imp_aport_var17_1y3"] <- 2
fin_db[fin_db$delta_imp_compra_var44_1y3 == 9999999999, "delta_imp_compra_var44_1y3"] <- 7
fin_db[fin_db$delta_imp_reemb_var13_1y3 == 9999999999, "delta_imp_reemb_var13_1y3"] <- 1
fin_db[fin_db$delta_imp_reemb_var17_1y3 == 9999999999, "delta_imp_reemb_var17_1y3"] <- 1
fin_db[fin_db$delta_num_aport_var13_1y3 == 9999999999, "delta_num_aport_var13_1y3"] <- 2
fin_db[fin_db$delta_num_aport_var17_1y3 == 9999999999, "delta_num_aport_var17_1y3"] <- 3
fin_db[fin_db$delta_num_compra_var44_1y3 == 9999999999, "delta_num_compra_var44_1y3"] <- 4
fin_db[fin_db$delta_num_compra_var44_1y3 == 4, "delta_num_compra_var44_1y3"] <- 3
fin_db[fin_db$delta_num_reemb_var13_1y3 == 9999999999, "delta_num_reemb_var13_1y3"] <- 1
fin_db[fin_db$delta_num_reemb_var17_1y3 == 9999999999, "delta_num_reemb_var17_1y3"] <- 1
fin_db <- do.call(data.frame,lapply(fin_db, function(x) replace(x, is.infinite(x), NA)))
fin_db[is.na(fin_db)] <- 0

#TRAIN-TEST
rows_train <- createDataPartition(fin_db$TARGET, p=0.8, list=FALSE)
train <- fin_db[rows_train, ]
validation <- fin_db[-rows_train, ]
TARGET <- train$TARGET
TARGET_validation <- validation$TARGET
validation <- validation[,-which(names(validation) %in% c("ID", "TARGET"))]
TARGET_scaled <- train$TARGET
train.s <- train[,-which(names(train) %in% c("ID", "TARGET"))]
train.s <- data.frame(scale(train.s))
validation.s <- data.frame(scale(validation))


#DOWNSAMPLING
train_down <- downSample(x=train[,colnames(train) %ni% "TARGET"], 
                         y=as.factor(train$TARGET), yname="TARGET")
TARGET_down <- train_down$TARGET
train_down <- train_down[,-which(names(train_down) %in% c("ID", "TARGET"))]
train_down.s <- data.frame(scale(train_down))

#UPSAMPLING
train_up <- upSample(x=train[,colnames(train) %ni% "TARGET"], 
                     y=as.factor(train$TARGET), yname="TARGET")
TARGET_up <- train_up$TARGET
train_up <- train_up[,-which(names(train_up) %in% c("ID", "TARGET"))]
train_up.s <- data.frame(scale(train_up))

#SMOTE
train$TARGET <- as.factor(train$TARGET)
train_smote <- SMOTE(TARGET ~ train, train, perc.over = 100, perc.under=200)
train$TARGET <- as.numeric(train$TARGET)
TARGET_smote <- train_smote$TARGET
train_smote <- train_smote[,-which(names(train_smote) %in% c("ID", "TARGET"))]
train_smote.s <- data.frame(scale(train_smote))

#ROSE
train_rose <- ROSE(TARGET ~ ., data=train)$data
train_rose[train_rose==1] <- 0
train_rose[train_rose==2] <- 1
TARGET_rose <- train_rose$TARGET
train_rose <- train_rose[,-which(names(train_rose) %in% c("ID", "TARGET"))]
train_rose.s <- data.frame(scale(train_rose))

colnames(train.s) <- paste0(colnames(train.s),"_s")
colnames(train_down.s) <- paste0(colnames(train_down.s),"_s")
colnames(train_up.s) <- paste0(colnames(train_up.s),"_s")
colnames(train_smote.s) <- paste0(colnames(train_smote.s),"_s")
colnames(train_rose.s) <- paste0(colnames(train_rose.s),"_s")
bind <- dplyr::bind_cols(train, train.s)
bind_down <- dplyr::bind_cols(train_down, train_down.s)
bind_up <- dplyr::bind_cols(train_up, train_up.s)
bind_smote <- dplyr::bind_cols(train_smote, train_smote.s)
bind_rose <- dplyr::bind_cols(train_rose, train_rose.s)
colnames(validation.s) <- paste0(colnames(validation.s),"_s")
bind_validation <- dplyr::bind_cols(validation, validation.s)
TARGET_fin_db <- fin_db$TARGET
bind_fin_db <- fin_db[,-which(names(fin_db) %in% c("ID", "TARGET"))]
bind_fin_db <- data.frame(scale(bind_fin_db))
colnames(bind_fin_db) <- paste0(colnames(bind_fin_db),"_s")
bind_fin_db <- dplyr::bind_cols(fin_db, bind_fin_db)


for (i in log_vars){
  test_final[ , i] <- log(test_final[ , i])
}

test_final <- test_final[ , !names(test_final) %in% to_delete]
test_final <- test_final[,-which(names(test_final) %in% c("ID"))]
test_final[test_final$var3 == -999999, "var3"] <- 37
test_final[test_final$var36 == 99, "var36"] <- 4
test_final[test_final$delta_imp_aport_var13_1y3 == 9999999999, "delta_imp_aport_var13_1y3"] <- 6
test_final[test_final$delta_imp_aport_var17_1y3 == 9999999999, "delta_imp_aport_var17_1y3"] <- 2
test_final[test_final$delta_imp_compra_var44_1y3 == 9999999999, "delta_imp_compra_var44_1y3"] <- 7
test_final[test_final$delta_imp_reemb_var13_1y3 == 9999999999, "delta_imp_reemb_var13_1y3"] <- 1
test_final[test_final$delta_imp_reemb_var17_1y3 == 9999999999, "delta_imp_reemb_var17_1y3"] <- 1
test_final[test_final$delta_num_aport_var13_1y3 == 9999999999, "delta_num_aport_var13_1y3"] <- 2
test_final[test_final$delta_num_aport_var17_1y3 == 9999999999, "delta_num_aport_var17_1y3"] <- 3
test_final[test_final$delta_num_compra_var44_1y3 == 9999999999, "delta_num_compra_var44_1y3"] <- 4
test_final[test_final$delta_num_reemb_var13_1y3 == 9999999999, "delta_num_reemb_var13_1y3"] <- 1
test_final[test_final$delta_num_reemb_var17_1y3 == 9999999999, "delta_num_reemb_var17_1y3"] <- 1
test_final <- do.call(data.frame,lapply(test_final, function(x) replace(x, is.infinite(x), NA)))
test_final[is.na(test_final)] <- 0
test_final.s <- data.frame(scale(test_final))
colnames(test_final.s) <- paste0(colnames(test_final.s),"_s")
bind_test_final <- dplyr::bind_cols(test_final, test_final.s)


######################### 02 - REGRESIÓN LOGÍSTICA #########################
recursive_logit <- function(input.df, target_var, eval.df, target_eval, iter=50, val_cut=0.0001, 
                            ROC_val_evol=c(), ROC_vars_evol=c(), contador=0){
  contador <- contador + 1
  print(paste("Iteracion ", as.character(contador)))
  ROC_val <- c()
  ROC_var <- c()
  input.iter.df <- input.df[ , !names(input.df) %in% ROC_vars_evol]
  for (i in 1:ncol(input.iter.df)){
    if (contador == 1){
      logitmod <- glm(as.formula(paste(paste(paste(as.character(substitute(target_var)), " ~ "), 
                      paste(ROC_vars_evol,  "+ ", collapse=" + ")), colnames(input.iter.df[i]),sep = "")), 
                      family = binomial, data = input.df)
    }else if(contador != 1){
      logitmod <- glm(as.formula(paste(paste("TARGET ~", paste(ROC_vars_evol, collapse=" + ")), 
                      paste(" + ", colnames(input.iter.df[i])),sep = "")), family = binomial, 
                      data = input.df)
    }
    pred <- predict(logitmod, newdata=eval.df, type="response") 
    ROC_val[i] <- InformationValue::AUROC(target_eval,pred) 
    ROC_var[i] <- colnames(input.iter.df[i])
  }
  
  ROC <- data.frame(var=ROC_var, ROC=ROC_val)
  ROC_vars_evol <- append(ROC_vars_evol, (as.character(ROC[which.max(ROC$ROC), 1])))
  ROC_val_evol <- append(ROC_val_evol, (as.numeric(ROC[which.max(ROC$ROC), 2])))
  print(as.character(ROC[which.max(ROC$ROC), 1]))
  print((as.numeric(ROC[which.max(ROC$ROC), 2])))
  
  if(contador > 1){
    if(ROC_val_evol[length(ROC_val_evol)] <= ROC_val_evol[length(ROC_val_evol)-1]){
      print("Fin - Añadir una variable baja el valor ROC")
      df <- data.frame(var=ROC_vars_evol, ROC=ROC_val_evol)
      df$pos_var=1:nrow(df)
      beepr::beep(3)
      return(df)   
    }else if(contador == iter){
      print("Fin - Se ha llegado al numero de iteraciones")
      df <- data.frame(var=ROC_vars_evol, ROC=ROC_val_evol)
      df$pos_var=1:nrow(df)
      beepr::beep(3)
      return(df)
    }else if(ROC_val_evol[length(ROC_val_evol)] - (ROC_val_evol[length(ROC_val_evol)-1]) <= val_cut){
      print("Fin - Se ha llegado al nivel de sensibilidad")
      df <- data.frame(var=ROC_vars_evol, ROC=ROC_val_evol)
      df$pos_var=1:nrow(df)
      beepr::beep(3)
      return(df)
    }
  }
  recursive_logit(input.df, target_var, eval.df, target_eval, iter, val_cut, ROC_val_evol, ROC_vars_evol, contador)
}

elim <- c("ID", "TARGET", "delta_imp_trasp_var33_out_1y3", "delta_imp_trasp_var33_out_1y3_s",
          "imp_trasp_var33_out_ult1_s", "num_trasp_var33_out_ult1_s", "delta_num_reemb_var13_1y3_s", 
          "delta_num_reemb_var17_1y3_s", "ind_var6_0_s", "ind_var6_s", "ind_var13_medio_0_s", "ind_var18_0_s",
          "ind_var34_0_s", "num_var6_0_s", "num_var6_s", "num_var13_medio_0_s", "num_var18_0_s", "num_var34_0_s",
          "num_op_var40_hace3_s", "saldo_var6_s", "saldo_var13_medio_s", "saldo_var18_s", "saldo_var34_s",
          "delta_imp_amort_var18_1y3_s", "delta_imp_amort_var34_1y3_s", "delta_imp_aport_var33_1y3_s", 
          "delta_imp_reemb_var33_1y3_s", "delta_imp_trasp_var17_in_1y3_s", "delta_imp_trasp_var17_out_1y3_s", 
          "delta_imp_trasp_var33_in_1y3_s", "delta_num_aport_var33_1y3_s", "imp_amort_var18_ult1_s", 
          "imp_amort_var34_ult1_s", "imp_aport_var17_hace3_s", "imp_aport_var33_hace3_s", "imp_aport_var33_ult1_s",
          "imp_var7_emit_ult1_s", "imp_reemb_var17_hace3_s", "imp_reemb_var33_ult1_s", "imp_trasp_var17_in_hace3_s", 
          "imp_trasp_var17_in_ult1_s", "imp_trasp_var17_out_ult1_s", "imp_trasp_var33_in_hace3_s", 
          "imp_trasp_var33_in_ult1_s", "imp_venta_var44_hace3_s", "ind_var7_emit_ult1_s", "num_aport_var33_hace3_s", 
          "num_aport_var33_ult1_s", "num_var7_emit_ult1_s", "num_meses_var13_medio_ult3_s", "num_meses_var29_ult3_s",
          "num_reemb_var17_hace3_s", "num_reemb_var33_ult1_s", "num_trasp_var17_in_hace3_s", "num_trasp_var17_in_ult1_s", 
          "num_trasp_var17_out_ult1_s", "num_trasp_var33_in_hace3_s", "num_trasp_var33_in_ult1_s", 
          "num_venta_var44_hace3_s", "saldo_medio_var13_medio_hace2_s", "saldo_medio_var13_medio_ult3_s",
          "saldo_medio_var17_hace3_s", "saldo_medio_var29_hace2_s", "saldo_medio_var29_hace3_s", 
          "saldo_medio_var29_ult1_s", "saldo_medio_var29_ult3_s", "saldo_medio_var33_hace3_s", "num_aport_var17_hace3_s")

bind <- bind[,-which(names(bind) %in% elim)]
bind_down <- bind_down[,-which(names(bind_down) %in% elim)]
bind_up <- bind_up[,-which(names(bind_up) %in% elim)]
bind_smote <- bind_smote[,-which(names(bind_smote) %in% elim)]
bind_rose <- bind_rose[,-which(names(bind_rose) %in% elim)]
bind_fin_db <- bind_fin_db[,-which(names(bind_fin_db) %in% elim)]
bind_validation <- bind_validation[,-which(names(bind_validation) %in% elim)]
bind_test_final <- bind_test_final[,-which(names(bind_test_final) %in% elim)]

train.s$TARGET <- TARGET
output.s_logit <- recursive_logit(data.frame(train.s), TARGET, validation.s, TARGET_validation, 50)
train_down.s$TARGET <- TARGET_down
output_down.s_logit <- recursive_logit(data.frame(train_down.s), TARGET, validation.s, TARGET_validation, 50)
train_up.s$TARGET <- TARGET_up
output_up.s_logit <- recursive_logit(data.frame(train_up.s), TARGET, validation.s, TARGET_validation, 50)
train_smote.s$TARGET <- TARGET_smote
output_smote.s_logit <- recursive_logit(data.frame(train_smote.s), TARGET, validation.s, TARGET_validation, 50)
train_rose.s$TARGET <- TARGET_rose
output_rose.s_logit <- recursive_logit(data.frame(train_rose.s), TARGET, validation.s, TARGET_validation, 50)

ext_ROC <- function(logitmod, validation){
  TARGET_validation <- validation$TARGET
  validation <- validation[,-which(names(validation) %in% c("ID", "TARGET"))]
  #Funcion para obtener la puntuacion ROC
  pred <- predict(logitmod, newdata=validation, type="response") 
  ROC <- InformationValue::AUROC(TARGET_validation,pred) 
  return(ROC)
}

select_model <-  function(n, mod, train, validation){
  if(mod == "descomp"){
    TARGET <- train$TARGET
    train <- train[,-which(names(train) %in% c("ID", "TARGET"))]
    train$TARGET <- TARGET
    logitmod <- glm(as.formula(paste("TARGET ~ ", 
                    paste(output_bind_logit$var[1:length(output_bind_logit$var-n)], 
                          collapse=" + "))), family = binomial, data = train)
    ROC_vals <- ext_ROC(logitmod, validation)
    
  }else if(mod == "down_bind"){
    train_down <- downSample(x=train[,colnames(train) %ni% "TARGET"], 
                             y=as.factor(train$TARGET), yname="TARGET")
    TARGET_down <- train_down$TARGET
    train_down <- train_down[,-which(names(train_down) %in% c("ID", "TARGET"))]
    train_down$TARGET <- TARGET_down
    logitmod <- glm(as.formula(paste("TARGET ~ ", 
                paste(output_bind_down_logit$var[1:length(output_bind_down_logit$var-n)], 
                      collapse=" + "))), family = binomial, data = train_down)
    ROC_vals <- ext_ROC(logitmod, validation)
    
  }else if(mod == "up_bind"){
    train_up <- upSample(x=train[,colnames(train) %ni% "TARGET"], 
                         y=as.factor(train$TARGET), yname="TARGET")
    TARGET_up <- train_up$TARGET
    train_up <- train_up[,-which(names(train_up) %in% c("ID", "TARGET"))]
    train_up$TARGET <- TARGET_up
    logitmod <- glm(as.formula(paste("TARGET ~ ", 
                paste(output_bind_up_logit$var[1:length(output_bind_up_logit$var-n)], 
                      collapse=" + "))), family = binomial, data = train_up)
    ROC_vals <- ext_ROC(logitmod, validation)
    
  }else if (mod == "smote_bind"){
    train$TARGET <- as.factor(train$TARGET)
    train_smote <- SMOTE(TARGET ~ train, train, perc.over = 100, perc.under=200)
    train$TARGET <- as.numeric(train$TARGET)
    TARGET_smote <- train_smote$TARGET
    train_smote <- train_smote[,-which(names(train_smote) %in% c("ID", "TARGET"))]
    train_smote$TARGET <- TARGET_smote
    logitmod <- glm(as.formula(paste("TARGET ~ ", 
                paste(output_bind_smote_logit$var[1:length(output_bind_smote_logit$var-n)], 
                     collapse=" + "))), family = binomial, data = train_smote)
    ROC_vals <- ext_ROC(logitmod, validation)
    
  }else if (mod == "rose_bind"){
    train_rose <- ROSE(TARGET ~ ., data=train)$data
    train$TARGET <- as.numeric(train$TARGET)
    TARGET_rose <- train_rose$TARGET
    train_rose <- train_rose[,-which(names(train_rose) %in% c("ID", "TARGET"))]
    train_rose$TARGET <- TARGET_rose
    logitmod <- glm(as.formula(paste("TARGET ~ ", 
                paste(output_bind_rose_logit$var[1:length(output_bind_rose_logit$var-n)], 
                      collapse=" + "))), family = binomial, data = train_rose)
    ROC_vals <- ext_ROC(logitmod, validation)
  }
  return(ROC_vals)
}

split_select_model <- function(n, iter, modelo){
  ROC_vals <- c()
  bind_fin_db$TARGET <- TARGET_fin_db
  for(i in 1:iter){
    if(i%%10 == 0){print(i)}
    set.seed(Sys.time())
    #Split test-train
    rows_train <- createDataPartition(bind_fin_db$TARGET, p=0.8, list=FALSE)
    train <- bind_fin_db[rows_train, ]
    validation <- bind_fin_db[-rows_train, ]
    TARGET_validation <- validation$TARGET
    validation <- validation[,-which(names(validation) %in% c("ID", "TARGET"))]
    validation$TARGET <- TARGET_validation
    if(modelo=="descomp"){ROC <- select_model(n, "descomp", train, validation)
    }else if(modelo=="down_bind"){ROC <- select_model(n, "down_bind", train, validation)
    }else if(modelo=="up_bind"){ROC <- select_model(n, "up_bind", train, validation)
    }else if(modelo=="rose_bind"){ROC <- select_model(n, "rose_bind", train, validation)
    }else if(modelo=="smote_bind"){ROC <- select_model(n, "smote_bind", train, validation)
    }
    ROC_vals[i] <- ROC
  }
  return(ROC_vals)
}

test_less_vars <- function(n, itera=100, model){
  contador <- 0
  testamiento <- list()
  for(nume in n:0){
    print(paste("Se quitan", as.character(nume), "variables"))
    contador <- contador + 1
    testamiento[[contador]] <- split_select_model(nume, iter=itera, model)
  }
  print(testamiento)
  salida <- as.data.frame(testamiento)
  rownames(salida)<- NULL
  inv_l <- contador:0
  for (n in 1:ncol(salida)){
    colnames(salida)[n]<-paste("vars_men_", inv_l[n]-1, sep="")
  }
  salida <- salida[,seq(dim(salida)[2],1)]
  return(salida)
}

test_less_bind <- test_less_vars(9, 40, "descomp")
test_less_down_bind <- test_less_vars(9, 40, "down_bind")
test_less_up_bind <- test_less_vars(9, 40, "up_bind")
test_less_smote_bind <- test_less_vars(9, 40, "smote_bind")
test_less_rose_bind <- test_less_vars(9, 40, "rose_bind")

means_sd_less_var <- data.frame(means=colMeans(test_less_bind), 
                     sd=matrixStats::colSds(as.matrix(test_less_bind)))
means_sd_down_less_var <- data.frame(means=colMeans(test_less_down_bind), 
                          sd=matrixStats::colSds(as.matrix(test_less_down_bind)))
means_sd_up_less_var <- data.frame(means=colMeans(test_less_up_bind), 
                        sd=matrixStats::colSds(as.matrix(test_less_up_bind)))
means_sd_smote_less_var <- data.frame(means=colMeans(test_less_smote_bind), 
                           sd=matrixStats::colSds(as.matrix(test_less_smote_bind)))
means_sd_rose_less_var <- data.frame(means=colMeans(test_less_rose_bind), 
                          sd=matrixStats::colSds(as.matrix(test_less_rose_bind)))