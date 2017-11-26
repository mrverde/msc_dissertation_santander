######################### 06 - XGBoost #########################
#Borramos la memoria
rm(list=ls())

#Cargamos las librerías
library(xgboost)
library(Matrix)
library(caret)
library(pROC)

#Establezco el directorio de la BD
setwd("/home/usuario/Documentos/R/TFM/")

#Establecemos una semilla
set.seed(1000)

#Para cargar la base de datos en memoria
fin_db <- read.csv("train.csv", sep=",", header = TRUE, dec=".")
test_final <- read.csv("test.csv", sep=",", header = TRUE, dec=".")

#Establezco el directorio del XGBoost
setwd("/home/usuario/Documentos/R/TFM/2.6 - XGBoost/")

############  TRANSFORMACIÓN Y ELIMINACIÓN DE VARIABLES, DATOS PERDIDOS, OUTLIERS, ETC.  ############
#copiamos y despues borramos las variables ID y TARGET
TARGET_fin_db <- fin_db$TARGET
ID_test_final <- test_final$ID
fin_db <- fin_db[,-which(names(fin_db) %in% c("ID", "TARGET"))]
test_final <- test_final[,-which(names(test_final) %in% c("ID"))]


#Para crear una nueva variable con el número de 0 que tiene cada fila
fin_db$val_cero <- as.integer(rowSums(fin_db == 0))
test_final$val_cero <- as.integer(rowSums(test_final == 0))

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


#Asignamos la clasificación a la columna TARGET
fin_db$TARGET <- TARGET_fin_db

##### PREDICCION ####
#Creamos la matriz XGB fin_db
xgb_mat_fin_db <- sparse.model.matrix(TARGET ~ ., data = fin_db)
train_xg_fin_db <- xgb.DMatrix(data = xgb_mat_fin_db, label = TARGET_fin_db)



#watchlist <- list(train=train_xg_fin_db)

param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "auc",
                eta                 = 0.02,
                max_depth           = 5,
                subsample           = 0.7,
                colsample_bytree    = 0.7
)

#Creamos una función para entrenar los modelos
xgb_model <- function(xgb_df, nr, plot_xgb=TRUE){
  modelo <- xgb.train(params              = param, 
                      data                = xgb_df, 
                      nrounds             = nr,
                      verbose             = 1,
                      #                    watchlist           = watchlist,
                      maximize            = FALSE)
  if(plot_xgb==TRUE){
    importance_matrix <- xgb.importance(colnames(xgb_df), model = modelo)
    xgb.plot.importance(importance_matrix[1:30], rel_to_first = TRUE, xlab = "Importancia relativa de las variables")
  }
  return(modelo)
}


#mod <- xgb_model(train_xg_fin_db, 550)



##### BLOQUE DE VALIDACION #####
#Genera los valores de los parámetros de búsqueda del xgb.cv
gen_xgb.cv_param <- function(){
  nrounds_val <- c()
  eta_val <- c()
  subsample_val <- c()
  colsample_val <- c()
  min_child_val <- c()
  max_depth_val <- c()
  contador <- 1
  for (i_nrounds in seq(100,1000,150)){
    for (i_eta in seq(2, 10, 2.5)/i_nrounds){ 
      for (i_subsample in c(0.5, 0.75, 1)){
        for (i_colsample in c(0.4, 0.7, 1)){
          for (i_min_child in 3/c(1.25, 2.5, 5)){
            for (i_max_depth in seq(4,10,3)){
              nrounds_val[contador] <- i_nrounds
              eta_val[contador] <- i_eta
              subsample_val[contador] <- i_subsample
              colsample_val[contador] <- i_colsample
              min_child_val[contador] <- i_min_child
              max_depth_val[contador] <- i_max_depth
              contador <- contador + 1
            }
          }
        }
      }
    }
  }
  param_df <- data.frame(nrounds=nrounds_val, eta=eta_val, subsample=subsample_val, 
                         colsample_bytree=colsample_val, min_child_weight=min_child_val, 
                         max_depth=max_depth_val)
  param_df$modelo <- 1:length(param_df$nrounds)
  return(param_df)
}
bd_param <- gen_xgb.cv_param()


#Utiliza la cross-validation con el XGB. Utiliza la bd de parámetros de la función gen_xgb.cv_param. Hay que especificarle el rango de modelos que queremos probar.
#Devuelve un df con los resultados
xgb_cv_search <- function(min, max){
  contador <- 1
  res_cv_search <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(res_cv_search) <- c("modelo", "iter", "train_auc_mean", "train_auc_std", "test_auc_mean", "test_auc_std")
  for (i in min:max){
    cat("\n\n\n Modelo - ", as.character(i), "\n")
    parametros <- bd_param[i,]
    
    mod <- xgb.cv(params = parametros, data=train_xg_fin_db, nrounds=parametros$nrounds, nfold=5, showsd = TRUE, 
                  metrics = list("auc"), verbose = T, prediction = T)
    max <- lapply(mod$evaluation_log[parametros$nrounds], '[', 1)
    max$modelo <- parametros$modelo
    res_cv_search <- rbind(res_cv_search, max)
    
  }
  return(res_cv_search)
}

#xgb_cv1 <- xgb_cv_search(1,50)
#xgb_cv2 <- xgb_cv_search(50,100)
#xgb_cv3 <- xgb_cv_search(200,300)
#save.image("~/Documentos/R/TFM/2.6 - XGBoost/xgb-cv-final2.RData")
#xgb_cv6 <- xgb_cv_search(301,350)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final6.RData')
#xgb_cv7 <- xgb_cv_search(351,400)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final7.RData')
#xgb_cv8 <- xgb_cv_search(401,450)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final8.RData')
#xgb_cv9 <- xgb_cv_search(451,500)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final9.RData')
#xgb_cv10 <- xgb_cv_search(501,550)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final10.RData')
#xgb_cv11 <- xgb_cv_search(551,600)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final11.RData')
#xgb_cv12 <- xgb_cv_search(601,650)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final12.RData')
#xgb_cv13 <- xgb_cv_search(651,700)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final13.RData')
#xgb_cv14 <- xgb_cv_search(701,750)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final14.RData')
#xgb_cv15 <- xgb_cv_search(751,800)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final15.RData')
#xgb_cv16 <- xgb_cv_search(801,850)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final16.RData')
#xgb_cv17 <- xgb_cv_search(851,900)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final17.RData')
#xgb_cv18 <- xgb_cv_search(901,950)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final18.RData')
#xgb_cv19 <- xgb_cv_search(951,1000)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final19.RData')
#xgb_cv20 <- xgb_cv_search(1001,1050)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final20.RData')
#xgb_cv21 <- xgb_cv_search(1051,1100)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final21.RData')
#xgb_cv22 <- xgb_cv_search(1101,1150)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final22.RData')
#xgb_cv23 <- xgb_cv_search(1151,1200)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final23.RData')
#xgb_cv24 <- xgb_cv_search(1201,1250)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final24.RData')
#xgb_cv25 <- xgb_cv_search(1251,1300)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final25.RData')
#xgb_cv26 <- xgb_cv_search(1301,1350)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final26.RData')
#xgb_cv27 <- xgb_cv_search(1351,1400)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final27.RData')
#xgb_cv28 <- xgb_cv_search(1401,1450)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final28.RData')
#xgb_cv29 <- xgb_cv_search(1451,1500)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final29.RData')
#xgb_cv30 <- xgb_cv_search(1501,1550)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final30.RData')
#xgb_cv31 <- xgb_cv_search(1551,1600)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final31.RData')
#xgb_cv32 <- xgb_cv_search(1601,1650)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final32.RData')
#xgb_cv33 <- xgb_cv_search(1651,1700)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final33.RData')
#xgb_cv34 <- xgb_cv_search(1701,1750)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final34.RData')
#xgb_cv35 <- xgb_cv_search(1751,1800)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final35.RData')
#xgb_cv36 <- xgb_cv_search(1801,1850)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final36.RData')
#xgb_cv37 <- xgb_cv_search(1851,1900)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final37.RData')
#xgb_cv38 <- xgb_cv_search(1901,1950)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final38.RData')
#xgb_cv39 <- xgb_cv_search(1951,2000)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final39.RData')

#xgb_cv40 <- xgb_cv_search(2001,2050)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final40.RData')
#xgb_cv41 <- xgb_cv_search(2051,2100)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final41.RData')
#xgb_cv42 <- xgb_cv_search(2101,2150)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final42.RData')
#xgb_cv43 <- xgb_cv_search(2151,2200)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final43.RData')
#xgb_cv44 <- xgb_cv_search(2201,2250)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final44.RData')
#xgb_cv45 <- xgb_cv_search(2251,2268)
#save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final45.RData')

#Genera todas las lineas que deberemos ejecutar para comprobar todo el rango de modelos con la función xgb_cv_search
print_lines <- function(){
  contador <- 0
  for(i in 1:45){
    contador <- contador + 1
    cat("xgb_cv", as.character(contador), " <- xgb_cv_search(", as.character((contador*50)+1), ",", as.character((1+contador)*50), ")\n", sep="")
    cat("save.image('~/Documentos/R/TFM/2.6 - XGBoost/rescv/xgb-cv-final", as.character(contador), ".RData')\n", sep="")
  }
}
print_lines()

print_xgb_cv <- function(){
  contador <- 0
  for(i in 1:45){
    contador <- contador + 1
    cat("xgb_cv", as.character(contador), ", ", sep="")
  }
}
print_xgb_cv()

xgb_cv <- rbind(xgb_cv1, xgb_cv2, xgb_cv3, xgb_cv6, xgb_cv7, xgb_cv8, xgb_cv9, xgb_cv10, xgb_cv11, xgb_cv12, 
      xgb_cv13, xgb_cv14, xgb_cv15, xgb_cv16, xgb_cv17, xgb_cv18, xgb_cv19, xgb_cv20, xgb_cv21, xgb_cv22, xgb_cv23, 
      xgb_cv24, xgb_cv25, xgb_cv26, xgb_cv27, xgb_cv28, xgb_cv29, xgb_cv30, xgb_cv31, xgb_cv32, xgb_cv33, xgb_cv34, 
      xgb_cv35, xgb_cv36, xgb_cv37, xgb_cv38, xgb_cv39, xgb_cv40, xgb_cv41, xgb_cv42, xgb_cv43, xgb_cv44, xgb_cv45)

rm(xgb_cv1, xgb_cv2, xgb_cv3, xgb_cv6, xgb_cv7, xgb_cv8, xgb_cv9, xgb_cv10, xgb_cv11, xgb_cv12, 
   xgb_cv13, xgb_cv14, xgb_cv15, xgb_cv16, xgb_cv17, xgb_cv18, xgb_cv19, xgb_cv20, xgb_cv21, xgb_cv22, xgb_cv23, 
   xgb_cv24, xgb_cv25, xgb_cv26, xgb_cv27, xgb_cv28, xgb_cv29, xgb_cv30, xgb_cv31, xgb_cv32, xgb_cv33, xgb_cv34, 
   xgb_cv35, xgb_cv36, xgb_cv37, xgb_cv38, xgb_cv39, xgb_cv40, xgb_cv41, xgb_cv42, xgb_cv43, xgb_cv44, xgb_cv45)

write.csv(bd_param, "parametrosxgb.csv")
write.csv(xgb_cv, "results_xgbcv.csv")
#23 de septiembre - Modelos 2063, 1421, 1739, 1745, 1742
#24 de septiembre - Modelos 1094, 1415, 2069, 1418, 1826        #### mas alto 1826 con 0.840304 - Puesto 600
#25 de septiembre - Modelos 1487, 1091, 1424, 2150, 2222, 1718
#26 de septiembre - Modelos 1199, 1121, 1529, 1151, 1103
#27 de septiembre - Modelos 2225, 1175, 2066, 1391, 2231
#28 de septiembre - Modelos 1100, 2141, 2036, 1499, 443


##### BLOQUE DE TEST #####
#Creamos la matriz XGB de test
test_final$TARGET <- -1
test_final_xg <- sparse.model.matrix(TARGET ~ ., data = test_final)

bd_param <- read.csv("parametrosxgb.csv", sep=",", header = TRUE, dec=".")
xgb_cv <- read.csv("results_xgbcv.csv", sep=",", header = TRUE, dec=".")


param <- bd_param[bd_param$modelo==2063, ]
param$objective <- "binary:logistic" 
param$booster <- "gbtree"
param$eval_metric <-"auc"

mod <- xgb_model(train_xg_fin_db, param$nrounds)
pred <- predict(mod, test_final_xg)

#importance_matrix <- xgb.importance(colnames(train_xg_fin_db), model = mod)
#pdf("XGB_imp_vars.pdf",width=8,height=5,paper='special') 
#xgb.plot.importance(importance_matrix[1:20], rel_to_first = TRUE)
#dev.off()

count_pred <- pred
count_pred[count_pred<0.5] <- 0
count_pred[count_pred>=0.5] <- 1
table(count_pred)

submission <- data.frame(ID=ID_test_final, TARGET=pred)
write.csv(submission, file = "ROC_xgboost_mod_443.csv",row.names=FALSE)


xgb.plot.importance(importance_matrix = NULL, top_n = NULL,
                    measure = NULL, rel_to_first = FALSE, left_margin = 10, cex = NULL,
                    plot = TRUE, ...)

best_smote <- read.csv("best_smote.csv", sep=",", header = TRUE, dec=".")
best_xgb <- read.csv("best_xgb.csv", sep=",", header = TRUE, dec=".")
pred <- (best_smote$TARGET*0.20) + (best_xgb$TARGET*0.80)
submission <- data.frame(ID=ID_test_final, TARGET=pred)
write.csv(submission, file = "ROC_mixed_xgb_logsmote_80-20",row.names=FALSE)