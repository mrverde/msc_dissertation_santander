######################### 02 - SVM #########################

#Cargamos las librerías
library(doMC)
library(caret)
library(Boruta)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(gridExtra)
library(DMwR)
library(caret)

#Establecemos los núcleos usados a 8
registerDoMC(cores=7)

#Establezco el directorio de trabajo de la seccion
setwd("/home/usuario/Documentos/R/TFM/02.1-Logit/")

########## FUNCIÓN RECURSIVA REGRESIÓN LOGÍSTICA ##########
recursive_SVM <- function(input.df, target_var, eval.df, target_eval, metod, iter=50, val_cut=0.00001, ROC_val_evol=c(), ROC_vars_evol=c(), contador=0){
  #Funcion recursiva que va probando todas las variables de un dataframe y va seleccionando las variables que mas aumentan la curva ROC
      #input.df es el DF con nuestros datos de entrenamiento
      #target_var es el nombre de la columna objetivo a clasificar en df.input
      #eval.df es el nombre del df con los datos de test
      #target_eval es la columna a clasificar de los datos de test
      #val_cut es la sensibilidad de la formula
      #iter es el numero de iteraciones
      #NO TOCAR EL RESTO DE VARIABLES
  contador <- contador + 1
  print(paste("Iteracion ", as.character(contador)))
  grid <- expand.grid(C=c(1), sigma= c(.001))
  ctrl <- trainControl(method = "none", search="grid", allowParallel = TRUE, summaryFunction=twoClassSummary, classProbs=TRUE)
  
  #Bloque de prueba de todas las variables de la bd
  ROC_val <- c()
  ROC_var <- c()
  input.iter.df <- input.df[ , !names(input.df) %in% c(ROC_vars_evol, "TARGET")]
  for (i in 1:ncol(input.iter.df)){
    if (i%%50 == 0){print(paste("Subiteracion ", as.character(i)))}
    if (contador == 1){
      mod <- train(as.formula(paste(paste(paste(as.character(substitute(target_var)), " ~ "), paste(ROC_vars_evol,  "+ ", collapse=" + ")), colnames(input.iter.df[i]),sep = "")), data=input.df, method=metod, metric="ROC", tuneGrid = grid, trControl=ctrl)
    }else if(contador != 1){
      mod <- train(as.formula(paste(paste("TARGET ~", paste(ROC_vars_evol, collapse=" + ")), paste(" + ", colnames(input.iter.df[i])),sep = "")), data=input.df, method=metod, metric="ROC", tuneGrid = grid, trControl=ctrl)
    }
    pred <- predict(mod, validation.s) 
    pred <- as.character(pred)
    pred[pred=="X0"] <- 0
    pred[pred=="X1"] <- 1
    pred <- as.numeric(pred)
    ROC_val[i] <- InformationValue::AUROC(target_eval,pred) 
    ROC_var[i] <- colnames(input.iter.df[i])
  }
  
  #Bloque de extracción de la mejor variable
  ROC <- data.frame(var=ROC_var, ROC=ROC_val)
  ROC_vars_evol <- append(ROC_vars_evol, (as.character(ROC[which.max(ROC$ROC), 1])))
  ROC_val_evol <- append(ROC_val_evol, (as.numeric(ROC[which.max(ROC$ROC), 2])))
  print(as.character(ROC[which.max(ROC$ROC), 1]))
  print((as.numeric(ROC[which.max(ROC$ROC), 2])))
  
  #Bloque de salida
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
  recursive_SVM(input.df, target_var, eval.df, target_eval, metod, iter, val_cut, ROC_val_evol, ROC_vars_evol, contador)
}

#Para ejecutarlo
train_down.s$TARGET <- make.names(TARGET_down)
output_down.s_SVM <- recursive_SVM(data.frame(train_down.s), TARGET, validation.s, TARGET_validation, "svmLinear", 50)

#train_up.s$TARGET <- make.names(TARGET_up)
#output_up.s_SVM <- recursive_SVM(data.frame(train_up.s), TARGET, validation.s, TARGET_validation, "svmLinear", 50)

train_smote.s$TARGET <- make.names(TARGET_smote)
output_smote.s_SVM <- recursive_SVM(data.frame(train_smote.s), TARGET, validation.s, TARGET_validation, "svmLinear", 50)

train_down.s$TARGET <- make.names(TARGET_down)
output_down.s_SVM_poly <- recursive_SVM(data.frame(train_down.s), TARGET, validation.s, TARGET_validation, "svmPoly", 50)

#train_up.s$TARGET <- make.names(TARGET_up)
#output_up.s_SVM <- recursive_SVM(data.frame(train_up.s), TARGET, validation.s, TARGET_validation, "svmLinear", 50)

train_smote.s$TARGET <- make.names(TARGET_smote)
output_smote.s_SVM_poly <- recursive_SVM(data.frame(train_smote.s), TARGET, validation.s, TARGET_validation, "svmPoly", 50)

train_down.s$TARGET <- make.names(TARGET_down)
output_down.s_SVM_rad <- recursive_SVM(data.frame(train_down.s), TARGET, validation.s, TARGET_validation, "svmRadial", 50)

#train_up.s$TARGET <- make.names(TARGET_up)
#output_up.s_SVM <- recursive_SVM(data.frame(train_up.s), TARGET, validation.s, TARGET_validation, "svmLinear", 50)

train_smote.s$TARGET <- make.names(TARGET_smote)
output_smote.s_SVM_rad <- recursive_SVM(data.frame(train_smote.s), TARGET, validation.s, TARGET_validation, "svmRadial", 50)







mi_vars <- c("num_var30", "imp_op_var39_efect_ult1", "num_var8", "num_op_var40_comer_ult3", "saldo_var8", "num_op_var40_efect_ult3", "ind_var25_cte", "var15")
ctrl <- trainControl(method = "none", search="grid", allowParallel = TRUE, summaryFunction=twoClassSummary, classProbs=TRUE)
bind_rose$TARGET <- make.names(TARGET_rose)
contador <- 0
for (i in c(0.25, 0.5, 0.75, 1, 1.25, 1.5)){
  for (j in c(.01, .015, 0.2, 0.3, 0.4)){
    contador <- contador + 1
    grid <- expand.grid(C=c(i), sigma= c(j))
    print(paste("Iteración", as.character(contador)))
    print(paste("C =", as.character(i), "sigma =", as.character(j)))
    mod <- train(as.formula(paste(paste("TARGET ~", paste(boruta_signif, collapse=" + ")),sep = "")), data=bind_rose, method="svmRadial", metric="ROC", tuneGrid = grid, trControl=ctrl)
    pred <- predict(mod, bind_validation) 
    pred <- as.character(pred)
    pred[pred=="X0"] <- 0
    pred[pred=="X1"] <- 1
    pred <- as.numeric(pred)
    ROC_val[contador] <- InformationValue::AUROC(TARGET_validation,pred) 
    print(paste("AUC -> ", as.character(ROC_val[contador])))
  }
}








ROC_var <- colnames(input.iter.df[i])



#GUARDAR DF 
df <- data.frame(var=ROC_vars_evol, roc_value=ROC_val_evol)
save(output, file="ROC_evol_logit_227vars_sucesivo_31vars_79ROC.Rda")


#EXPORTAR CSV A KAGGLE
logitmod <- glm(as.formula(paste("TARGET ~", paste(output_smote.s_logit$var[1:(length(output_smote.s_logit$var)-6)], collapse=" + "))), family = binomial, data = train_smote.s)
pred_final <- predict(logitmod,newdata=test_final.s, type="response")
y_pred_num <- ifelse(pred_final > 0.5, 1, 0)
table(y_pred_num)
df_out <- data.frame(ID=test_finalbackup$ID, TARGET=y_pred_num)
write.csv(df_out, file = "ROC_evol_logit_227vars_sucesivo_24vars_83ROC_smote_log.csv",row.names=FALSE)
