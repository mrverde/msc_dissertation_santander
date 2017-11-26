######################### 03 - KNN #########################

#Cargamos las librerías
library(doMC)
library(caret)
library(Boruta)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(gridExtra)
library(DMwR)

#Para instalar las librerías
#install.packages(c("doMC","caret","Boruta","ggplot2","ggthemes","reshape2","gridExtra","DMwR","caret"))

#Establecemos los núcleos usados a 8
registerDoMC(cores=8)

#Establezco el directorio de trabajo de la seccion
setwd("/home/usuario/Documentos/R/TFM/02.2 - KNN/")


recursive_knn <- function(input.df, target_var, eval.df, target_eval, iter=50, val_cut=0.0000001, ROC_vars_evol=c(),ROC_val_evol=c(), contador=0, ks=5){
  #Funcion que va iterando por las variables y va seleccionando las que mas incrementan el valor de ROC
  #iter - Numero de iteraciones (variables maximas)
  #val_cut - Valor de la sensibilidad de corte del algoritomo
  #ROC_vars_evol - Vector que va registrando el nombre de las variables
  #ROC_val_evol - Vector que va registrando el valor de ROC de las variables
  #Contador - No tocar. Contador de iteraciones. Necesario para que funcionen algunos parámetros del algoritmo.
  contador <- contador +1
  print(paste("Iteracion ", as.character(contador)))
  

  ROC_val <- c()
  ROC_var <- c()
  input.iter.df <- input.df[ , !names(input.df) %in% ROC_vars_evol]
  
  for (i in 1:ncol(input.iter.df)){
  #for (i in c(colnames(input.df))){
    nom_cols <- c(colnames(input.iter.df[i]), ROC_vars_evol)
    print(nom_cols)
    subdf <- as.matrix(input.df[ , nom_cols])
    fit <- knn3(subdf, target_var, k=ks)  
    subdf_validation <- as.matrix(eval.df[ , nom_cols])


    predictions <- tryCatch({
      predict(fit, subdf_validation, type="class")
    },error=function(cond){
      return(NA)
    })
    
    ROC_val <- append(ROC_val, InformationValue::AUROC(target_eval, as.numeric(predictions)))
  }
  
  ROC <- data.frame(var=c(colnames(input.iter.df)), ROC=ROC_val)
  ROC_interval <- ROC[(ROC$ROC >= 0 & ROC$ROC <= 1), ]
  ROC_vars_evol <- append(ROC_vars_evol, (as.character(ROC[which.max(ROC_interval$ROC), 1])))
  ROC_val_evol <- append(ROC_val_evol, (as.numeric(ROC[which.max(ROC_interval$ROC), 2])))
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
  recursive_knn(input.df, target_var, eval.df, target_eval, iter, val_cut, ROC_vars_evol, ROC_val_evol, contador, ks)

}


#Para ejecutarlo
train.s <- train.s[ , !names(train.s) %in% "TARGET"]
KNN_ROC <- recursive_knn(train.s, TARGET, validation.s, TARGET_validation, 50)

train_down.s <- train_down.s[ , !names(train_down.s) %in% "TARGET"]
KNN_ROC_down <- recursive_knn(train_down.s, TARGET_down, validation.s, TARGET_validation, 50)

train_up.s <- train_up.s[ , !names(train_up.s) %in% "TARGET"]
KNN_ROC_up <- recursive_knn(train_up.s, TARGET_up, validation.s, TARGET_validation, 50)

train_smote.s <- train_smote.s[ , !names(train_smote.s) %in% "TARGET"]
KNN_ROC_smote <- recursive_knn(train_smote.s, TARGET_smote, validation.s, TARGET_validation, 50)



how_many_k <- function(input.df, target_var, eval.df, target_eval, lista_vars, vec){
  #Funcion que itera cambiando el numero de vecinos.
  #lista_vars es la lista de variables de la BD que queremos usar en el modelo
  #vec es un numero entero. Es el máximo número de vecinos del bucle de iteración
  
  #Convertimos lista_vars a un vector de strings
  lista_vars <- c(as.character(lista_vars))
  
  #Iteramos por diferente número de vecinos, de 1 a 150
  ROC_val_evol <- c()
  ROC_vars_evol <- c()
  subdf <- data.frame(as.matrix(input.df[ , lista_vars]))
  subdf_validation <- data.frame(as.matrix(eval.df[ , lista_vars]))
  
  for(i in 1:vec){
    #Hacemos el KNN
    fit <- knn3(subdf, target_var, k=i)
    print(fit)
    
    #Predecimos
    predictions <- tryCatch({
      predict(fit, subdf_validation, type="class")
    },error=function(cond){
      print(paste("Error en la variable ", as.character(i)))
      return(NA)
    })
    #caret::confusionMatrix(tab)
    
    #Guarda el valor de la ROC en un vector
    ROC_val_evol <- append(ROC_val_evol, InformationValue::AUROC(target_eval, as.numeric(predictions)))
    print(InformationValue::AUROC(target_eval, as.numeric(predictions)))
    beepr::beep(7)
  }
  beepr::beep(3)
  return(ROC_val_evol)
}

#Para utilizar la salida de la función 
how_many <- how_many_k(train.s, TARGET, validation.s, TARGET_validation, KNN_ROC[, "var"], 120)
how_many_down <- how_many_k(train_down.s, TARGET_down, validation.s, TARGET_validation, KNN_ROC_down[, "var"], 120)
how_many_up <- how_many_k(train_up.s, TARGET_up, validation.s, TARGET_validation, KNN_ROC_up[, "var"], 120)
how_many_smote <- how_many_k(train_smote.s, TARGET_smote, validation.s, TARGET_validation, KNN_ROC_smote[, "var"], 120)


prueba_inversa <- function(){
  #Esta función hace un KNN con todas las variables menos una que va iterando
  for (i in 1:227){
    proba_smote <- train_smote.s[ , -c(137, i)]
    fit <- knn3(as.matrix(proba_smote), TARGET_smote, k=5)  
    
    proba_validation <- validation.s[ , -c(137, i)]
    predictions <- predict(fit, proba_validation,  type="class")
    
    print(InformationValue::AUROC(TARGET_validation, as.numeric(predictions)))
    beepr::beep(3)
  }
  
}
# La AUC en todas las iteraciones se queda en 0.58. No merece la pena seguir con este algoritmo

prueba_inversa()

#Ejecutar 50 veces
#Iterar por los k 100
#Recursión variables

#GUARDAR DF 
#df <- data.frame(k=1:150, roc_value=ROC_val_evol)
#save(KNN_ROC,file="ROC_evol_knn_50vars_5K_OK.Rda")


logitmod <- glm(as.formula(paste("TARGET ~", paste(output_bind_rose_logit$var, collapse=" + "))), family = binomial, data = bind_rose)
pred_final <- predict(logitmod,newdata=bind_test_final, type="response")