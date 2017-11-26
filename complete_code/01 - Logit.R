######################### 02 - REGRESIÓN LOGÍSTICA #########################

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
library(ROSE)

#Establecemos los núcleos usados a 8
registerDoMC(cores=8)

#Establezco el directorio de trabajo de la seccion
setwd("/home/usuario/Documentos/R/TFM/02.1-Logit/")

#Establecemos semilla
set.seed(1117)

########## FUNCIÓN RECURSIVA REGRESIÓN LOGÍSTICA ##########
recursive_logit <- function(input.df, target_var, eval.df, target_eval, iter=50, val_cut=0.0001, ROC_val_evol=c(), ROC_vars_evol=c(), contador=0){
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
      logitmod <- glm(as.formula(paste(paste(paste(as.character(substitute(target_var)), " ~ "), paste(ROC_vars_evol,  "+ ", collapse=" + ")), colnames(input.iter.df[i]),sep = "")), family = binomial, data = input.df)
    }else if(contador != 1){
      logitmod <- glm(as.formula(paste(paste("TARGET ~", paste(ROC_vars_evol, collapse=" + ")), paste(" + ", colnames(input.iter.df[i])),sep = "")), family = binomial, data = input.df)
    }
    
    pred <- predict(logitmod, newdata=eval.df, type="response") 
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
  recursive_logit(input.df, target_var, eval.df, target_eval, iter, val_cut, ROC_val_evol, ROC_vars_evol, contador)
}

#Variables a eliminar del df porque dan problemas
elim <- c("ID", "TARGET", "delta_imp_trasp_var33_out_1y3", "delta_imp_trasp_var33_out_1y3_s",
          "imp_trasp_var33_out_ult1_s", "num_trasp_var33_out_ult1_s", "delta_num_reemb_var13_1y3_s", 
          "delta_num_reemb_var17_1y3_s", "ind_var6_0_s", "ind_var6_s", "ind_var13_medio_0_s", "ind_var18_0_s",
          "ind_var34_0_s", "num_var6_0_s", "num_var6_s", "num_var13_medio_0_s", "num_var18_0_s", "num_var34_0_s",
          "num_op_var40_hace3_s", "saldo_var6_s", "saldo_var13_medio_s", "saldo_var18_s", "saldo_var34_s",
          "delta_imp_amort_var18_1y3_s", "delta_imp_amort_var34_1y3_s", "delta_imp_aport_var33_1y3_s", "delta_imp_reemb_var33_1y3_s",
          "delta_imp_trasp_var17_in_1y3_s", "delta_imp_trasp_var17_out_1y3_s", "delta_imp_trasp_var33_in_1y3_s", "delta_num_aport_var33_1y3_s",
          "imp_amort_var18_ult1_s", "imp_amort_var34_ult1_s", "imp_aport_var17_hace3_s", "imp_aport_var33_hace3_s", "imp_aport_var33_ult1_s",
          "imp_var7_emit_ult1_s", "imp_reemb_var17_hace3_s", "imp_reemb_var33_ult1_s", "imp_trasp_var17_in_hace3_s", "imp_trasp_var17_in_ult1_s", 
          "imp_trasp_var17_out_ult1_s", "imp_trasp_var33_in_hace3_s", "imp_trasp_var33_in_ult1_s", "imp_venta_var44_hace3_s", "ind_var7_emit_ult1_s",
          "num_aport_var33_hace3_s", "num_aport_var33_ult1_s", "num_var7_emit_ult1_s", "num_meses_var13_medio_ult3_s", "num_meses_var29_ult3_s",
          "num_reemb_var17_hace3_s", "num_reemb_var33_ult1_s", "num_trasp_var17_in_hace3_s", "num_trasp_var17_in_ult1_s", "num_trasp_var17_out_ult1_s",
          "num_trasp_var33_in_hace3_s", "num_trasp_var33_in_ult1_s", "num_venta_var44_hace3_s", "saldo_medio_var13_medio_hace2_s", "saldo_medio_var13_medio_ult3_s",
          "saldo_medio_var17_hace3_s", "saldo_medio_var29_hace2_s", "saldo_medio_var29_hace3_s", "saldo_medio_var29_ult1_s", "saldo_medio_var29_ult3_s",
          "saldo_medio_var33_hace3_s", "num_aport_var17_hace3_s")

#Creamos una variable con la clasificacion objetivo y borramos las variables inútiles
bind <- bind[,-which(names(bind) %in% elim)]
bind_down <- bind_down[,-which(names(bind_down) %in% elim)]
bind_up <- bind_up[,-which(names(bind_up) %in% elim)]
bind_smote <- bind_smote[,-which(names(bind_smote) %in% elim)]
bind_rose <- bind_rose[,-which(names(bind_rose) %in% elim)]
bind_fin_db <- bind_fin_db[,-which(names(bind_fin_db) %in% elim)]
bind_validation <- bind_validation[,-which(names(bind_validation) %in% elim)]
bind_test_final <- bind_test_final[,-which(names(bind_test_final) %in% elim)]

#Para ejecutarlo
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



################ EXPERIMENTO BINDED #################
bind$TARGET <- TARGET
output_bind_logit <- recursive_logit(data.frame(bind), TARGET, bind_validation, TARGET_validation, 70)
save.image("~/Documentos/R/TFM/02.1-Logit/output_bind_final.RData")

bind_down$TARGET <- TARGET_down
output_bind_down_logit <- recursive_logit(data.frame(bind_down), TARGET, bind_validation, TARGET_validation, 70)
save.image("~/Documentos/R/TFM/02.1-Logit/output_bind_final.RData")

bind_up$TARGET <- TARGET_up
output_bind_up_logit <- recursive_logit(data.frame(bind_up), TARGET, bind_validation, TARGET_validation, 70)
save.image("~/Documentos/R/TFM/02.1-Logit/output_bind_final.RData")

bind_smote$TARGET <- TARGET_smote
output_bind_smote_logit <- recursive_logit(data.frame(bind_smote), TARGET, bind_validation, TARGET_validation, 70)
save.image("~/Documentos/R/TFM/02.1-Logit/output_bind_final.RData")

bind_rose$TARGET <- TARGET_rose
output_bind_rose_logit <- recursive_logit(data.frame(bind_rose), TARGET, bind_validation, TARGET_validation, 70)
save.image("~/Documentos/R/TFM/02.1-Logit/output_bind_final.RData")


########## PLOT EVOLUCION ROC - 1 MODELO ##########
plot_1var <- function(df.in, title, title.pdf=FALSE){
  #Dibuja la evolución de los valores de ROC de una variable.
  #df.in - DF de entrada. Tiene que tener 2 variables llamadas var (nombre variables) y ROC (valores ROC)
  #title - str con el compañamiento al titulo del grafico
  #title.pdf - Introducir un str con el titulo si queremos exportar PDF
  plot <- ggplot(data=df.in, aes(y = ROC, x = factor(var, levels=unique(var)), group=1, order=FALSE)) +  geom_line(color="firebrick3", size=1.25) + 
    theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="bottom") + 
     labs(title=paste("Evolución puntuación AUC - ", title), x=paste("Total de variables seleccionadas = ", as.character(length(df.in$var))), y="Valor de AUC") + 
    scale_y_continuous(minor_breaks = seq(0, 1, 0.00625))  + theme(panel.grid.major.y = element_line(colour = "grey60", size=0.3, linetype = "dashed"), 
    axis.text.x = element_text(angle = 90, vjust = 1, hjust=1, size=6), panel.grid.minor.y = element_line(colour = "grey80", size=0.1, linetype = "dashed"), 
    panel.grid.major.x = element_line(colour = "grey60", size=0.3, linetype = "F1"), panel.grid.minor.x = element_line(colour = "white", size=0.3, linetype = "F1")) 
  print(plot)
  if (title.pdf != FALSE){ggsave(paste(title.pdf,".pdf"), width = 9, height = 4.5)}
}

plot_1var(output_bind_smote_logit, "SMOTE", "Smote")
plot_1var(output_bind_up_logit, "Upsampling", "Upsampling")
plot_1var(output_bind_down_logit, "Downsampling", "Downsampling")
plot_1var(output_bind_logit, "Descompensado", "Descompensado")
plot_1var(output_bind_rose_logit, "ROSE", "ROSE")


########## PLOT EVOLUCION ROC - VARIOS MODELOS ##########
plot_comparison <- function(list_df, title, leg.val, title.pdf=FALSE){
  #Dibuja en un mismo gráfico la evolución de la puntuación ROC de varios modelos
  #list_df - Lista de DF, tienen que tener: pos_var - Entero con la posicion de cada variable en el eje x; ROC - Valor de ROC
  #title - STR - Complemento al titulo del grafico
  #leg.val - Vector de STR. Descripción en la leyenda
  #title.pdf - STR - Opcional - Exporta el gráfico a PDF
  
  c_df = dplyr::bind_rows(list_df, .id = "source")
  colors <- RColorBrewer::brewer.pal(9, "Set1")
  ar <- data.frame(max=tapply(c_df$ROC, c_df$source, max))
  
  g <- ggplot(c_df, aes(x=pos_var, y=ROC))   
    
  lines_vec <- c()
  for(i in (1:length(list_df))){
    i = as.character(i)
    lines_vec <-paste(lines_vec, print(paste(c("+ geom_hline(data=ar, aes(yintercept=max[", i, "]), linetype='4C88C488', color=colors[", i, "])"), collapse="")))
  }
  lines_vec <- paste("g <- g", lines_vec)  
  eval(parse(text=lines_vec))
  
  g <- g + geom_line(aes(colour=source), size=1) + theme_bw() + scale_y_continuous(minor_breaks = seq(0, 1, 0.00625)) + 
        scale_x_continuous(breaks = seq(1, 230, 1)) + labs(title=paste("Evolución puntuación AUC - ", title), x="Nº de variables", y="Valor de AUC") + 
        theme(plot.title = element_text(hjust = 0.5), panel.grid.major.y = element_line(colour = "grey60", size=0.3, linetype = "dashed"), 
        axis.text.x = element_text(angle = 0, vjust = 1, size=6), panel.grid.minor.y = element_line(colour = "grey80", size=0.1, linetype = "dashed"), 
        panel.grid.major.x = element_line(colour = "grey60", size=0.3, linetype = "F1"),  legend.text=element_text(size=7), legend.title=element_text(size=8), 
        legend.position=c(.85,.2), legend.background = element_rect(colour = 'black', size = 0.3), panel.grid.minor.x = element_line(colour = "white", size=0.3, linetype = "F1")) + 
    scale_colour_manual(labels = leg.val, values = colors) + labs(colour="Método") 
  print(g)
  
  if (title.pdf != FALSE){ggsave(paste(title.pdf,".pdf"), width = 9, height = 4.5)}
}

#Ejemplo de invocacion
arguments <- list(output_bind_smote_logit, output_bind_up_logit, output_bind_down_logit, output_bind_logit, output_bind_rose_logit)
legend_names <- c("SMOTE", "Upsampling", "Downsampling","Descompensada", "ROSE")
plot_comparison(arguments, "LOGIT", legend_names, "Evolucion modelos Logit")


########## TESTANDO MODELOS  ##########
#Este bloque necesitalos outputs de recursive_logit para funcionar
ext_ROC <- function(logitmod, validation){
  TARGET_validation <- validation$TARGET
  validation <- validation[,-which(names(validation) %in% c("ID", "TARGET"))]
  #Funcion para obtener la puntuacion ROC
  pred <- predict(logitmod, newdata=validation, type="response") 
  ROC <- InformationValue::AUROC(TARGET_validation,pred) 
  return(ROC)
}

select_model <-  function(n, mod, train, validation){
  #Esta funcion tiene una serie de bloques condicionales. Cada uno utiliza diferentes sistemas de balanceo. Despues calcula un modelo logit de cada uno
  #Necesita la funcion ext_ROC
  #n - Numero de variables que queremos quitar
  #mod - forma de compensar que queremos utilizar puede tomar los valores "descomp", "down", "up", "smote"
  if(mod == "descomp"){
    TARGET <- train$TARGET
    train <- train[,-which(names(train) %in% c("ID", "TARGET"))]
    train$TARGET <- TARGET
    logitmod <- glm(as.formula(paste("TARGET ~ ", paste(output_bind_logit$var[1:length(output_bind_logit$var-n)], collapse=" + "))), family = binomial, data = train)
    ROC_vals <- ext_ROC(logitmod, validation)
    
  }else if(mod == "down_bind"){
    train_down <- downSample(x=train[,colnames(train) %ni% "TARGET"], y=as.factor(train$TARGET), yname="TARGET")
    TARGET_down <- train_down$TARGET
    train_down <- train_down[,-which(names(train_down) %in% c("ID", "TARGET"))]
    train_down$TARGET <- TARGET_down
    logitmod <- glm(as.formula(paste("TARGET ~ ", paste(output_bind_down_logit$var[1:length(output_bind_down_logit$var-n)], collapse=" + "))), family = binomial, data = train_down)
    ROC_vals <- ext_ROC(logitmod, validation)
    
  }else if(mod == "up_bind"){
    train_up <- upSample(x=train[,colnames(train) %ni% "TARGET"], y=as.factor(train$TARGET), yname="TARGET")
    TARGET_up <- train_up$TARGET
    train_up <- train_up[,-which(names(train_up) %in% c("ID", "TARGET"))]
    train_up$TARGET <- TARGET_up
    logitmod <- glm(as.formula(paste("TARGET ~ ", paste(output_bind_up_logit$var[1:length(output_bind_up_logit$var-n)], collapse=" + "))), family = binomial, data = train_up)
    ROC_vals <- ext_ROC(logitmod, validation)
    
  }else if (mod == "smote_bind"){
    train$TARGET <- as.factor(train$TARGET)
    train_smote <- SMOTE(TARGET ~ train, train, perc.over = 100, perc.under=200)
    train$TARGET <- as.numeric(train$TARGET)
    TARGET_smote <- train_smote$TARGET
    train_smote <- train_smote[,-which(names(train_smote) %in% c("ID", "TARGET"))]
    train_smote$TARGET <- TARGET_smote
    logitmod <- glm(as.formula(paste("TARGET ~ ", paste(output_bind_smote_logit$var[1:length(output_bind_smote_logit$var-n)], collapse=" + "))), family = binomial, data = train_smote)
    ROC_vals <- ext_ROC(logitmod, validation)
    
  }else if (mod == "rose_bind"){
    train_rose <- ROSE(TARGET ~ ., data=train)$data
    train$TARGET <- as.numeric(train$TARGET)
    TARGET_rose <- train_rose$TARGET
    train_rose <- train_rose[,-which(names(train_rose) %in% c("ID", "TARGET"))]
    train_rose$TARGET <- TARGET_rose
    logitmod <- glm(as.formula(paste("TARGET ~ ", paste(output_bind_rose_logit$var[1:length(output_bind_rose_logit$var-n)], collapse=" + "))), family = binomial, data = train_rose)
    ROC_vals <- ext_ROC(logitmod, validation)
    
  }
  return(ROC_vals)
}





split_select_model <- function(n, iter, modelo){
  #Esta funcion divide los datos un numero de iteraciones iter y selecciona el modelo a probar llamando a la funcion select_model
  #Utiliza las funciones select_model y ext_ROC
  #n numero entero. Por si queremos probar el modelo con un numero menor de variables. Valor 0 para probar con todas.
  #iter numero entero. Numero total de iteraciones que queremos hacer de cada modelo
  #modelo - selecciona entre diferentes metodos para compensar clases desbalanceadas, puede tomar: "descomp", "down", "up", "smote"
  
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
  #Para testar diferentes modelos un numero de veces (itera) quitando un numero (n) de variables
  #Hace uso de las funciones split_select_model, select_model y ext_ROC
  #n es el numero de variables que queremos quitar puede ser 0
  #itera es el numero de veces que queremos que se ejecuten los modelos
  #model la clase de algoritmo compensador que queremos ejecutar, puede tomar los valores: "descomp", "down", "up", "smote"
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
  #reversed df
  salida <- salida[,seq(dim(salida)[2],1)]
  return(salida)
}

#
#Para ejecutar
test_less_var_down <- test_less_vars(15, 100, "down")
test_less_var_up_less_var <- test_less_vars(15, 100, "up")
test_less_var_smote_less_var <- test_less_vars(15, 100, "smote")


########## Testamos los modelos ###############
test_less_bind <- test_less_vars(9, 40, "descomp")
save.image("~/Documentos/R/TFM/02.1-Logit/output_bind_final.RData")

test_less_down_bind <- test_less_vars(9, 40, "down_bind")
save.image("~/Documentos/R/TFM/02.1-Logit/output_bind_final.RData")

test_less_up_bind <- test_less_vars(9, 40, "up_bind")
save.image("~/Documentos/R/TFM/02.1-Logit/output_bind_final.RData")

test_less_smote_bind <- test_less_vars(9, 40, "smote_bind")
save.image("~/Documentos/R/TFM/02.1-Logit/output_bind_final.RData")

test_less_rose_bind <- test_less_vars(9, 40, "rose_bind")
save.image("~/Documentos/R/TFM/02.1-Logit/output_bind_final.RData")

#Creamos df con medias y desviaciones estándar
means_sd_less_var <- data.frame(means=colMeans(test_less_bind), sd=matrixStats::colSds(as.matrix(test_less_bind)))
means_sd_down_less_var <- data.frame(means=colMeans(test_less_down_bind), sd=matrixStats::colSds(as.matrix(test_less_down_bind)))
means_sd_up_less_var <- data.frame(means=colMeans(test_less_up_bind), sd=matrixStats::colSds(as.matrix(test_less_up_bind)))
means_sd_smote_less_var <- data.frame(means=colMeans(test_less_smote_bind), sd=matrixStats::colSds(as.matrix(test_less_smote_bind)))
means_sd_rose_less_var <- data.frame(means=colMeans(test_less_rose_bind), sd=matrixStats::colSds(as.matrix(test_less_rose_bind)))

#Creamos los modelos finales 
#Descompensado
logitmod <- glm(as.formula(paste("TARGET ~", paste(output_bind_logit$var[1:(length(output_bind_logit$var)-8)], collapse=" + "))), family = binomial, data = bind)
pred_final <- predict(logitmod,newdata=bind_test_final, type="response")
df_out <- data.frame(ID=test_final$ID, TARGET=pred_final)
write.csv(df_out, file = "ROC_evol_bind_descompensado_sucesivo_19vars_80ROC.csv",row.names=FALSE)

#Downsampling
logitmod <- glm(as.formula(paste("TARGET ~", paste(output_bind_down_logit$var[1:(length(output_bind_logit$var))], collapse=" + "))), family = binomial, data = bind_down)
pred_final <- predict(logitmod,newdata=bind_test_final, type="response")
df_out <- data.frame(ID=test_final$ID, TARGET=pred_final)
write.csv(df_out, file = "ROC_evol_bind_down_sucesivo_35vars_82ROC.csv",row.names=FALSE)

#Upsampling
logitmod <- glm(as.formula(paste("TARGET ~", paste(output_bind_logit$var[1:(length(output_bind_logit$var)-5)], collapse=" + "))), family = binomial, data = bind_up)
pred_final <- predict(logitmod,newdata=bind_test_final, type="response")
df_out <- data.frame(ID=test_final$ID, TARGET=pred_final)
write.csv(df_out, file = "ROC_evol_bind_upsampling_sucesivo_19vars_82ROC.csv",row.names=FALSE)

#Smote
logitmod <- glm(as.formula(paste("TARGET ~", paste(output_bind_logit$var[1:(length(output_bind_logit$var)-3)], collapse=" + "))), family = binomial, data = bind_smote)
pred_final <- predict(logitmod,newdata=bind_test_final, type="response")
df_out <- data.frame(ID=test_final$ID, TARGET=pred_final)
write.csv(df_out, file = "ROC_evol_bind_smote_sucesivo_27vars_81ROC.csv",row.names=FALSE)

#ROSE
logitmod <- glm(as.formula(paste("TARGET ~", paste(output_bind_logit$var[1:(length(output_bind_logit$var)-1)], collapse=" + "))), family = binomial, data = bind_rose)
pred_final <- predict(logitmod,newdata=bind_test_final, type="response")
df_out <- data.frame(ID=test_final$ID, TARGET=pred_final)
write.csv(df_out, file = "ROC_evol_bind_rose_sucesivo_22vars_80ROC.csv",row.names=FALSE)


#Salvar el Workspace
#save.image("~/Documentos/R/TFM/02.1-Logit/Workspace FINAL.RData")




#GUARDAR DF 
df <- data.frame(var=ROC_vars_evol, roc_value=ROC_val_evol)
save(output, file="ROC_evol_logit_227vars_sucesivo_31vars_79ROC.Rda")


#EXPORTAR CSV A KAGGLE
#[1:(length(output_smote.s_logit$var)-6)]
logitmod <- glm(as.formula(paste("TARGET ~", paste(output_bind_rose_logit$var, collapse=" + "))), family = binomial, data = bind_rose)
pred_final <- predict(logitmod,newdata=bind_test_final, type="response")
y_pred_num <- ifelse(pred_final > 0.5, 1, 0)
table(y_pred_num)
df_out <- data.frame(ID=test_finalbackup$ID, TARGET=pred_final)
write.csv(df_out, file = "ROC_evol_bind_logit_sucesivo_27vars_80ROC_smote_log_dontpanic.csv",row.names=FALSE)

colnames(test_final.s) <- paste0(colnames(test_final.s),"_s")

