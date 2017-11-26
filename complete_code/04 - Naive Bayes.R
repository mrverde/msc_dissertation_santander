######################### 03 - Naïve Bayes #########################
#Cargamos las librerías
library(doMC)
library(caret)
library(Boruta)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(gridExtra)
library(DMwR)
library(e1071)

#install.packages("caret", "Boruta", "ggplot2", "ggthemes", "reshape2", "gridExtra", "DMwR")

#Establecemos los núcleos usados a 8
registerDoMC(cores=8)

#Establezco el directorio de trabajo de la seccion
setwd("/home/usuario/Documentos/R/TFM/02.3 - Naive Bayes/")

#Variables a eliminar del df porque dan problemas
elim <- c("ID", "TARGET", "delta_imp_reemb_var17_1y3", "delta_num_reemb_var17_1y3", "imp_reemb_var17_ult1", 
          "num_reemb_var17_ult1", "delta_imp_reemb_var13_1y3", "delta_num_reemb_var13_1y3", 
          "imp_reemb_var13_ult1", "num_reemb_var13_ult1")

#Creamos una variable con la clasificacion objetivo y borramos las variables inútiles
train.s <- train.s[,-which(names(train.s) %in% elim)]
train_down.s <- train_down.s[,-which(names(train_down.s) %in% elim)]
train_up.s <- train_up.s[,-which(names(train_up.s) %in% elim)]
train_smote.s <- train_smote.s[,-which(names(train_smote.s) %in% elim)]
train_rose.s <- train_rose.s[,-which(names(train_rose.s) %in% elim)]
validation.s <- validation.s[,-which(names(validation.s) %in% elim)]


########## FUNCIÓN RECURSIVA REGRESIÓN LOGÍSTICA ##########
recursive_bayes <- function(input.df, target_var, eval.df, target_eval, iter=50, val_cut=0.00001, ROC_val_evol=c(), ROC_vars_evol=c(), contador=0){
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
  
  #Bloque de prueba de todas las variables de la bd
  ROC_val <- c()
  ROC_var <- c()
  input.iter.df <- input.df[ , !names(input.df) %in% ROC_vars_evol]
  for (i in 1:ncol(input.iter.df)){

    if (contador == 1){
      nb_mod <- naiveBayes(as.formula(paste(paste(paste(as.character(substitute(target_var)), " ~ "), paste(ROC_vars_evol,  "+ ", collapse=" + ")), colnames(input.iter.df[i]),sep = "")), data = input.df)
    }else if(contador != 1){
      nb_mod <- naiveBayes(as.formula(paste(paste(paste(as.character(substitute(target_var)), " ~ "), paste(ROC_vars_evol,  "+ ", collapse=" + ")), colnames(input.iter.df[i]),sep = "")), data = input.df)
    }
    pred <- predict(nb_mod, eval.df) 
    ROC_val[i] <- InformationValue::AUROC(target_eval,as.numeric(pred))
    ROC_var[i] <- colnames(input.iter.df[i])
    print(colnames(input.iter.df[i]))
    print(InformationValue::AUROC(target_eval,as.numeric(pred)))
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
  recursive_bayes(input.df, target_var, eval.df, target_eval, iter, val_cut, ROC_val_evol, ROC_vars_evol, contador)
}

#Para ejecutar bayes
train.s$TARGET <- as.integer(TARGET)
output.s_bayes <- recursive_bayes(train.s, TARGET, validation.s, TARGET_validation, 50)

train_down.s$TARGET <- as.integer(TARGET_down)
output_down.s_bayes <- recursive_bayes(train_down.s, TARGET_down, validation.s, TARGET_validation, 50)

train_up.s$TARGET <- as.integer(TARGET_up)
output_up.s_bayes <- recursive_bayes(train_up.s, TARGET_up, validation.s, TARGET_validation, 50)

train_smote.s$TARGET <- as.integer(TARGET_smote)
output_smote.s_bayes <- recursive_bayes(train_smote.s, TARGET_smote, validation.s, TARGET_validation, 50)

train_rose.s$TARGET <- as.integer(TARGET_rose)
output_rose.s_bayes <- recursive_bayes(train_rose.s, TARGET_rose, validation.s, TARGET_validation, 50)

#GUARDAR DF 
df <- data.frame(val=ROC_vars_evol, roc_value=ROC_val_evol)
save(df,file="ROC_evol_bayes_227vars_sucesivo.Rda")

#EXPORTAR CSV A KAGGLE
train_smote.s$TARGET <- as.factor(TARGET_smote)
nb_mod <- naiveBayes(as.formula(paste("TARGET ~", paste(output_smote.s_bayes$var[1:(length(output_smote.s_bayes$var))], collapse=" + "))), data = train_smote.s)
test_final_bayes <- test_final.s[ , c(output_smote.s_bayes$var)]
pred_final <- predict(nb_mod, newdata=test_final_bayes)
pred_final <- as.numeric(pred_final)
pred_final[pred_final==1] <- 0
pred_final[pred_final==2] <- 1
df_out <- data.frame(ID=test_finalbackup$ID, TARGET=pred_final)
write.csv(df_out, file = "ROC_evol_bayes_67ROC_sucesivo_SMOTE_8vars.csv",row.names=FALSE)