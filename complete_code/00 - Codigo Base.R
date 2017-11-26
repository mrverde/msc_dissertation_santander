######################### 02 - REGRESIÓN LOGÍSTICA #########################
#Borramos la memoria
rm(list=ls())

#Cargamos las librerías
library(doMC)
library(caret)
library(Boruta)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(gridExtra)
library(DMwR)
library(ROSE)

#Establecemos una semilla
set.seed(1117)

#Definimos la función "not in"
'%ni%' <- Negate('%in%') 

#Establecemos los núcleos usados a 8
registerDoMC(cores=8)

#Establezco el directorio de la BD
setwd("/home/usuario/Documentos/R/TFM/")

#Para cargar la base de datos en memoria
datos <- read.csv("train.csv", sep=",", header = TRUE, dec=".")
test_final <- read.csv("test.csv", sep=",", header = TRUE, dec=".")

#Establezco el directorio de trabajo de la seccion
setwd("/home/usuario/Documentos/R/TFM/02.1-Logit/")

#Hacemos un respaldo de seguridad de los datos
datosbackup <- datos
test_finalbackup <- test_final

#Para crear una nueva variable con el número de 0 que tiene cada fila
datos$val_cero <- rowSums(datos == 0)
test_final$val_cero <- rowSums(test_final == 0)

############  TRANSFORMACIÓN Y ELIMINACIÓN DE VARIABLES, DATOS PERDIDOS, OUTLIERS, ETC.  ############
#Creamos el objeto fin_db copiando datos
fin_db <- datos

#Logaritmos
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

#Calculamos los logaritmos
for (i in log_vars){
  fin_db[ , i] <- log(fin_db[ , i])
}


###### TRASFORMACION FINAL
#Borramos las constantes
for (var in names(fin_db)){
  if (length(unique(fin_db[[var]])) == 1) {
    fin_db[[var]] <- NULL
    test_final[[var]] <- NULL
  }
}

#Eliminamos las variables únicas
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






#Transformaciones de datos perdidos
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

#Transformamos los infinitos en ceros
fin_db <- do.call(data.frame,lapply(fin_db, function(x) replace(x, is.infinite(x), NA)))

#Transformamos los NaN en ceros
fin_db[is.na(fin_db)] <- 0



########### TEST-TRAIN SPLIT ###########
rows_train <- createDataPartition(fin_db$TARGET, p=0.8, list=FALSE)
train <- fin_db[rows_train, ]
validation <- fin_db[-rows_train, ]
TARGET <- train$TARGET

######## VALIDATION SIN ESTANDARIZAR #########
TARGET_validation <- validation$TARGET
validation <- validation[,-which(names(validation) %in% c("ID", "TARGET"))]

######## NORMALIZACION Y CENTRADO ########
TARGET_scaled <- train$TARGET
train.s <- train[,-which(names(train) %in% c("ID", "TARGET"))]
train.s <- data.frame(scale(train.s))
validation.s <- data.frame(scale(validation))


###########  DOWNSAMPLING  ###########
#Hacemos el downsampling a los datos de entrenamiento
train_down <- downSample(x=train[,colnames(train) %ni% "TARGET"], y=as.factor(train$TARGET), yname="TARGET")

#Creamos una variable con la clasificacion objetivo y borramos las variables inútiles
TARGET_down <- train_down$TARGET
train_down <- train_down[,-which(names(train_down) %in% c("ID", "TARGET"))]

#Creamos la version estandarizada
train_down.s <- data.frame(scale(train_down))


###########  UPSAMPLING  ###########
#Hacemos el upsampling a los datos de entrenamiento
train_up <- upSample(x=train[,colnames(train) %ni% "TARGET"], y=as.factor(train$TARGET), yname="TARGET")

#Creamos una variable con la clasificacion objetivo y borramos las variables inútiles
TARGET_up <- train_up$TARGET
train_up <- train_up[,-which(names(train_up) %in% c("ID", "TARGET"))]

#Creamos la version estandarizada
train_up.s <- data.frame(scale(train_up))


###########  RESAMPLING  ###########
#Hacemos el SMOTE a los datos de entrenamiento
train$TARGET <- as.factor(train$TARGET)
train_smote <- SMOTE(TARGET ~ train, train, perc.over = 100, perc.under=200)
train$TARGET <- as.numeric(train$TARGET)

#Creamos una variable con la clasificacion objetivo y borramos las variables inútiles
TARGET_smote <- train_smote$TARGET
train_smote <- train_smote[,-which(names(train_smote) %in% c("ID", "TARGET"))]

#Creamos la version estandarizada
train_smote.s <- data.frame(scale(train_smote))


#Hacemos ROSE a los datos de entrenamiento
train_rose <- ROSE(TARGET ~ ., data=train)$data
train_rose[train_rose==1] <- 0
train_rose[train_rose==2] <- 1

#Creamos una variable con la clasificacion objetivo y borramos las variables inútiles
TARGET_rose <- train_rose$TARGET
train_rose <- train_rose[,-which(names(train_rose) %in% c("ID", "TARGET"))]

#Creamos la version estandarizada
train_rose.s <- data.frame(scale(train_rose))

########### EXPERIMENTO BINDED DF ###########
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


###########  CREAMOS BD PARA DATOS DE TEST FINALES  ###########
for (i in log_vars){
  test_final[ , i] <- log(test_final[ , i])
}


test_final <- test_final[ , !names(test_final) %in% to_delete]
test_final <- test_final[,-which(names(test_final) %in% c("ID"))]


#Transformaciones de datos perdidos
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

