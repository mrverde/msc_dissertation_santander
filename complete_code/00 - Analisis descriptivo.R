######################### 01 - ANÁLISIS EXPLORATORIO DE LOS DATOS #########################
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
library(caret)
library(scales)

#Establecemos los núcleos usados a 8
registerDoMC(cores=8)

#Establezco el directorio de la BD
setwd("/home/usuario/Documentos/R/TFM/")

#Para cargar la base de datos en memoria
datos <- read.csv("train.csv", sep=",", header = TRUE, dec=".")
test_final <- read.csv("test.csv", sep=",", header = TRUE, dec=".")

#Establezco el directorio de trabajo de la seccion
setwd("/home/usuario/Documentos/R/TFM/01-Analisis descriptivo/")

#Hacemos un respaldo de seguridad de los datos
datosbackup <- datos

#Para reestablecer los datos originales
#datos <- datosbackup

#Para obtener el nombre de las variables
bd_nomvar <- data.frame("NOM_VAR"=colnames(datos))
#write.csv(bdvar, "nombres_variables", row.names=FALSE) #Para guardar el nombre de las variables en un csv

#Casos que pertenecen a cada clase
table(datos$TARGET)

#Graficamos la distribución de la variable TARGET
ggplot(datos, aes(TARGET))  + geom_histogram(bins=2, col="white") + scale_x_continuous(breaks=c(0,1)) +
  labs(title="Distribución de casos", x="Clases", y="Nº de casos") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.5, bins=2) 
ggsave("01.Dist.casos.pdf", width = 4, height = 4.5)

#Creamos tablas de contingencia para comprobar los valores de cada una de las variables.
for (i in 1:ncol(datos)){
  print(colnames(datos[i]))
  print(table(datos[,i]))
  readline(prompt="Presiona [enter] para continuar")
}

#Para crear un histograma de la variable var15 (edad)
#ggplot(datos, aes(var15)) + geom_histogram(bins = length(unique(datos$var15))) +
#  labs(title="Histograma de var15", x="Posible edad", y="Nº de casos") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
#  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#ggsave("02.Histograma_var15.pdf", width = 4, height = 4.5)


#Para crear una nueva variable con el número de 0 que tiene cada fila
datos$val_cero <- rowSums(datos == 0)
summary(datos$val_cero)
table(datos$val_cero)
#Para añadirle un histograma
ggplot(datos, aes(x=val_cero, fill=make.names(datos$TARGET))) + geom_histogram(bins = length(unique(datos$val_cero))) +
  labs(title="Histograma de total de ceros en cada fila", x="Nº de ceros", y="Nº de casos") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  guides(fill=guide_legend(title=NULL)) + coord_cartesian(xlim=c(270,360)) + scale_fill_manual(values=c("#AAAAAA", "#333333"))
ggsave("02.Historial_N.ceros.pdf", width = 8, height = 3)



#### CREACIÓN DE LISTAS DE VARIABLES E ITERACIÓN POR ELLAS ####

#Funcion que itera y crea una tabla de contingencia con las variables y la variable clasificación objetivo
f_iter_tabla <- function(vars){
  for(i in vars){
    print(i)
    print(table(datos[ , i], datos$TARGET))
    readline(prompt="Presiona [enter] para continuar")
  }  
}


######## VECTORES CON GRUPOS DE VARIABLES - GRUPOS DE VARIABLES GRANDES - DELTA, IMP, IND, NUM Y SALDO ########

#VARIABLES DELTA
delta_vars <- c("delta_imp_amort_var18_1y3", "delta_imp_amort_var34_1y3", "delta_imp_aport_var13_1y3", "delta_imp_aport_var17_1y3",
                "delta_imp_aport_var33_1y3", "delta_imp_compra_var44_1y3", "delta_imp_reemb_var13_1y3", "delta_imp_reemb_var17_1y3",
                "delta_imp_reemb_var33_1y3", "delta_imp_trasp_var17_in_1y3", "delta_imp_trasp_var17_out_1y3", "delta_imp_trasp_var33_in_1y3",
                "delta_imp_trasp_var33_out_1y3", "delta_imp_venta_var44_1y3", "delta_num_aport_var13_1y3", "delta_num_aport_var17_1y3",
                "delta_num_aport_var33_1y3", "delta_num_compra_var44_1y3", "delta_num_reemb_var13_1y3", "delta_num_reemb_var17_1y3",
                "delta_num_reemb_var33_1y3", "delta_num_trasp_var17_in_1y3", "delta_num_trasp_var17_out_1y3", "delta_num_trasp_var33_in_1y3",
                "delta_num_trasp_var33_out_1y3", "delta_num_venta_var44_1y3")


#VARIABLES IMP
imp_vars <- c("imp_amort_var18_hace3","imp_amort_var18_ult1", "imp_amort_var34_hace3","imp_amort_var34_ult1","imp_aport_var13_hace3",
              "imp_aport_var13_ult1","imp_aport_var17_hace3","imp_aport_var17_ult1","imp_aport_var33_hace3","imp_aport_var33_ult1",
              "imp_compra_var44_hace3","imp_compra_var44_ult1","imp_ent_var16_ult1","imp_op_var39_comer_ult1","imp_op_var39_comer_ult3",
              "imp_op_var39_efect_ult1","imp_op_var39_efect_ult3","imp_op_var39_ult1","imp_op_var40_comer_ult1","imp_op_var40_comer_ult3",
              "imp_op_var40_efect_ult1","imp_op_var40_efect_ult3","imp_op_var40_ult1","imp_op_var41_comer_ult1","imp_op_var41_comer_ult3",
              "imp_op_var41_efect_ult1","imp_op_var41_efect_ult3","imp_op_var41_ult1","imp_reemb_var13_hace3","imp_reemb_var13_ult1",
              "imp_reemb_var17_hace3","imp_reemb_var17_ult1","imp_reemb_var33_hace3","imp_reemb_var33_ult1","imp_sal_var16_ult1",
              "imp_trans_var37_ult1","imp_trasp_var17_in_hace3","imp_trasp_var17_in_ult1","imp_trasp_var17_out_hace3",
              "imp_trasp_var17_out_ult1","imp_trasp_var33_in_hace3","imp_trasp_var33_in_ult1","imp_trasp_var33_out_hace3",
              "imp_trasp_var33_out_ult1","imp_var43_emit_ult1","imp_var7_emit_ult1","imp_var7_recib_ult1","imp_venta_var44_hace3",
              "imp_venta_var44_ult1")

#VARIABLES IND
ind_vars <- c("ind_var1", "ind_var1_0","ind_var10_ult1","ind_var10cte_ult1","ind_var12","ind_var12_0","ind_var13","ind_var13_0",
              "ind_var13_corto","ind_var13_corto_0","ind_var13_largo","ind_var13_largo_0","ind_var13_medio","ind_var13_medio_0",
              "ind_var14","ind_var14_0","ind_var17","ind_var17_0","ind_var18","ind_var18_0","ind_var19","ind_var2","ind_var2_0",
              "ind_var20","ind_var20_0","ind_var24","ind_var24_0","ind_var25","ind_var25_0","ind_var25_cte","ind_var26",
              "ind_var26_0","ind_var26_cte","ind_var27","ind_var27_0","ind_var28","ind_var28_0","ind_var29","ind_var29_0",
              "ind_var30","ind_var30_0","ind_var31","ind_var31_0","ind_var32","ind_var32_0","ind_var32_cte","ind_var33",
              "ind_var33_0","ind_var34","ind_var34_0","ind_var37","ind_var37_0","ind_var37_cte","ind_var39","ind_var39_0",
              "ind_var40","ind_var40_0","ind_var41","ind_var41_0","ind_var43_emit_ult1","ind_var43_recib_ult1","ind_var44",
              "ind_var44_0","ind_var46","ind_var46_0","ind_var5","ind_var5_0","ind_var6","ind_var6_0","ind_var7_emit_ult1",
              "ind_var7_recib_ult1","ind_var8","ind_var8_0","ind_var9_cte_ult1","ind_var9_ult1")

#VARIABLES NUM
num_vars <- c("num_aport_var13_hace3","num_aport_var13_ult1","num_aport_var17_hace3","num_aport_var17_ult1",
              "num_aport_var33_hace3","num_aport_var33_ult1","num_compra_var44_hace3","num_compra_var44_ult1",
              "num_ent_var16_ult1","num_med_var22_ult3","num_med_var45_ult3","num_meses_var12_ult3","num_meses_var13_corto_ult3",
              "num_meses_var13_largo_ult3","num_meses_var13_medio_ult3","num_meses_var17_ult3","num_meses_var29_ult3",
              "num_meses_var33_ult3","num_meses_var39_vig_ult3","num_meses_var44_ult3","num_meses_var5_ult3",
              "num_meses_var8_ult3","num_op_var39_comer_ult1","num_op_var39_comer_ult3","num_op_var39_efect_ult1",
              "num_op_var39_efect_ult3","num_op_var39_hace2","num_op_var39_hace3","num_op_var39_ult1","num_op_var39_ult3",
              "num_op_var40_comer_ult1","num_op_var40_comer_ult3","num_op_var40_efect_ult1","num_op_var40_efect_ult3",
              "num_op_var40_hace2","num_op_var40_hace3","num_op_var40_ult1","num_op_var40_ult3","num_op_var41_comer_ult1",
              "num_op_var41_comer_ult3","num_op_var41_efect_ult1","num_op_var41_efect_ult3","num_op_var41_hace2",
              "num_op_var41_hace3","num_op_var41_ult1","num_op_var41_ult3","num_reemb_var13_hace3","num_reemb_var13_ult1",
              "num_reemb_var17_hace3","num_reemb_var17_ult1","num_reemb_var33_hace3","num_reemb_var33_ult1","num_sal_var16_ult1",
              "num_trasp_var11_ult1","num_trasp_var17_in_hace3","num_trasp_var17_in_ult1","num_trasp_var17_out_hace3",
              "num_trasp_var17_out_ult1","num_trasp_var33_in_hace3","num_trasp_var33_in_ult1","num_trasp_var33_out_hace3",
              "num_trasp_var33_out_ult1","num_var1","num_var1_0","num_var12","num_var12_0","num_var13","num_var13_0",
              "num_var13_corto","num_var13_corto_0","num_var13_largo","num_var13_largo_0","num_var13_medio",
              "num_var13_medio_0","num_var14","num_var14_0","num_var17","num_var17_0","num_var18","num_var18_0",
              "num_var2_0_ult1","num_var2_ult1","num_var20","num_var20_0","num_var22_hace2","num_var22_hace3","num_var22_ult1",
              "num_var22_ult3","num_var24","num_var24_0","num_var25","num_var25_0","num_var26","num_var26_0","num_var27",
              "num_var27_0","num_var28","num_var28_0","num_var29","num_var29_0","num_var30","num_var30_0","num_var31",
              "num_var31_0","num_var32","num_var32_0","num_var33","num_var33_0","num_var34","num_var34_0","num_var35",
              "num_var37","num_var37_0","num_var37_med_ult2","num_var39","num_var39_0","num_var4","num_var40","num_var40_0",
              "num_var41","num_var41_0","num_var42","num_var42_0","num_var43_emit_ult1","num_var43_recib_ult1","num_var44",
              "num_var44_0","num_var45_hace2","num_var45_hace3","num_var45_ult1","num_var45_ult3","num_var46","num_var46_0",
              "num_var5","num_var5_0","num_var6","num_var6_0","num_var7_emit_ult1","num_var7_recib_ult1","num_var8",
              "num_var8_0","num_venta_var44_hace3","num_venta_var44_ult1")

#VARIABLES SALDO
saldo_vars <- c("saldo_medio_var12_hace2","saldo_medio_var12_hace3","saldo_medio_var12_ult1","saldo_medio_var12_ult3",
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
                "saldo_var46","saldo_var5","saldo_var6","saldo_var8")  

#VARIABLES VAR
var_vars <- c("var15", "var21", "var3", "var36", "var38")

#Comprobamos si hay observaciones de la clase minoritaria en los datos perdidos
f_iter_tabla(delta_vars)  #DELTA
f_iter_tabla(imp_vars)    #IMP
f_iter_tabla(ind_vars)    #IND
f_iter_tabla(num_vars)    #NUM
f_iter_tabla(saldo_vars)  #SALDO
f_iter_tabla(var_vars)    #VARS


######## VECTOR CON TODAS LAS VARIABLES ########
#Función que itera y hace tablas de contingencia 
f_iter_var <- function(dat){
  for(i in names(dat)){
    print(i)
    print(table(dat[ , i]))
    readline(prompt="Presiona [enter] para continuar")
  }  
}

#Ejecutamos la función
f_iter_var(datos)

############### OUTLIERS ##################
f_iter_outliers <- function(vars){
  for(i in vars){
    print(i)
    print(sort(table(fin_db[ , i]), decreasing = TRUE))
    print(i)
    print(length(boxplot.stats(fin_db[ , i])$out) )
    print(76020-length(boxplot.stats(fin_db[ , i])$out))
    print(max(fin_db[ , i])-min(fin_db[ , i]))
    readline(prompt="Presiona [enter] para continuar")
  }  
}

f_iter_outliers(num_vars)    #NUM
f_iter_outliers(saldo_vars)  #SALDO
f_iter_outliers(var_vars)    #VARS



outlier_values <-  length(boxplot.stats(fin_db$var38)$out)  # outlier values.
boxplot(fin_db$var38, main="Pressure Height", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)






############  ANÁLISIS CARACTERÍSTICAS VARIABLES  ############

#Cargamos la BD
stats_df <- read.csv("c3.df_var_variables.csv", sep=",", header = TRUE)

#Lista con el nombre de las variables
nom_variables <- c("var1", "var2", "var3", "var4", "var5", "var6", "var7", "var8", "var9", "var10", 
                   "var11", "var12", "var13", "var14", "var15", "var16", "var17", "var18", "var19", 
                   "var20", "var21", "var22", "var24", "var25", "var26", "var27", "var28", 
                   "var29", "var30", "var31", "var32", "var33", "var34", "var35", "var36", "var37", 
                   "var38", "var39", "var40", "var41", "var42", "var43", "var44", "var45", "var46")

#Funcion para extraer la leyenda de un gráfico
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

#Creamos una nueva variable para poder representar el total de ceros
stats_df$subst_zero <- stats_df$tot_vars - (stats_df$zero_vars + stats_df$almost_zero_vars)

#Creamos un df con el análisis del número de ceros de cada supervariable
stats_df_zeros <- melt(stats_df[ ,c("nom_var", "zero_vars", "almost_zero_vars", "subst_zero")])

#Dibujamos el gráfico 1 
plot1 <- ggplot(stats_df_zeros, aes(y = value, x = nom_var, fill = variable)) +  geom_bar(position = "fill", stat = "identity") + 
  coord_flip() + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="bottom") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(title="% de variables con ceros", x="Variables", y="Porcentaje") + scale_x_discrete(limits = rev(nom_variables)) +
  scale_fill_manual("", values=c("#AAABBB", "#666777", "#222333"), labels=c("Todos igual a 0", "Hasta 200 val. diferentes a 0", "+200 diferentes a 0")) +
  scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%"))
#Dibujamos el gráfico 2
plot2 <- ggplot(stats_df_zeros, aes(y = value, x = nom_var, fill = variable)) +  geom_bar(stat = "identity") + 
  coord_flip() + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="bottom") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(title="Total de variables con ceros", x="", y="Nº de variables") + scale_x_discrete(limits = rev(nom_variables)) +
  scale_fill_manual("", values=c("#AAABBB", "#666777", "#222333"), labels=c("Todos igual a 0  ", "Hasta 200 val. diferentes a 0  ", "+200 diferentes a 0"))
#Extraemos la leyenda común de los gráficos
mylegend <- g_legend(plot1)
#Dibujamos el gráfico final combinando los dos gráficos y la leyenda
plot3 <- grid.arrange(arrangeGrob(plot1 + theme(legend.position="none"), plot2 + theme(legend.position="none"),nrow=1),
             mylegend, nrow=2,heights=c(10, 1))
ggsave("05.Grafico_ceros_variables.pdf", plot3, width = 8, height = 6)

#Creamos un df con las variables en función de su prefijo
stats_df_pre <- melt(stats_df[ ,c("nom_var", "delta_vars", "imp_vars", "ind_vars", "num_vars", "saldo_vars", "var_vars")])

#Dibujamos el gráfico 1 
plot1 <- ggplot(stats_df_pre, aes(y = value, x = nom_var, fill = variable)) +  geom_bar(position = "fill", stat = "identity") + 
  coord_flip() + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="bottom") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(title="% de variables por etiqueta", x="Variables", y="Porcentaje") + scale_x_discrete(limits = rev(nom_variables)) +
  scale_fill_manual("", values=c('#8dd3dd','#80b1d3', '#bebada','#fb8072', "#F7BE81", "#A9F5A9"), labels=c("delta_vars", "imp_vars", "ind_vars", "num_vars", "saldo_vars", "var_vars")) +
  scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%"))
#Dibujamos el gráfico 2
plot2 <- ggplot(stats_df_pre, aes(y = value, x = nom_var, fill = variable)) +  geom_bar(stat = "identity") + 
  coord_flip() + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="bottom") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(title="Total de variables por etiqueta", x="", y="Nº de variables") + scale_x_discrete(limits = rev(nom_variables)) +
  scale_fill_manual("", values=c('#8dd3dd','#80b1d3', '#bebada','#fb8072', "#F7BE81", "#A9F5A9"), labels=c("delta_vars", "imp_vars", "ind_vars", "num_vars", "saldo_vars", "var_vars"))
#Extraemos la leyenda común de los gráficos
mylegend <- g_legend(plot1)
#Dibujamos el gráfico final combinando los dos gráficos y la leyenda
plot3 <- grid.arrange(arrangeGrob(plot1 + theme(legend.position="none"), plot2 + theme(legend.position="none"),nrow=1),
                      mylegend, nrow=2,heights=c(10, 1))
ggsave("04.Grafico_etiquetas_variables.pdf", plot3, width = 8, height = 6)


#Creamos un df con las variables en función de su tipo
stats_df_type <- melt(stats_df[ ,c("nom_var", "binary_val", "categ_val", "int_val", "float_val")])

#Arreglando errores
stats_df_type$variable <- as.character(stats_df_type$variable)
stats_df_type$variable[stats_df_type$variable =="binary_val"] <- "cont_val"
stats_df_type$variable[stats_df_type$variable =="int_val"] <- "cont_val"
stats_df_type <- aggregate(stats_df_type$value, by=list(stats_df_type$nom_var, stats_df_type$variable), FUN=sum)
colnames(stats_df_type) <- c("nom_var", "variable", "value")



#Dibujamos el gráfico 1 
plot1 <- ggplot(stats_df_type, aes(y = value, x = nom_var, fill = variable)) +  geom_bar(position = "fill", stat = "identity") + 
  coord_flip() + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="bottom") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(title="% de variables por tipo", x="Variables", y="Porcentaje") + scale_x_discrete(limits = rev(nom_variables)) +
  scale_fill_manual("", values=c('#8dd3dd','#fb8072','#bebada'), labels=c("Categóricas", "Discretas", "Continuas")) +
  scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%"))
#Dibujamos el gráfico 2
plot2 <- ggplot(stats_df_type, aes(y = value, x = nom_var, fill = variable)) +  geom_bar(stat = "identity") + 
  coord_flip() + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="bottom") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(title="Total de variables por tipo", x="", y="Nº de variables") + scale_x_discrete(limits = rev(nom_variables)) +
  scale_fill_manual("", values=c('#8dd3dd','#fb8072','#bebada'), labels=c("Categóricas", "Discretas", "Continuas"))
#Extraemos la leyenda común de los gráficos
mylegend <- g_legend(plot1)
#Dibujamos el gráfico final combinando los dos gráficos y la leyenda
plot3 <- grid.arrange(arrangeGrob(plot1 + theme(legend.position="none"), plot2 + theme(legend.position="none"),nrow=1),
                      mylegend, nrow=2,heights=c(10, 1))
ggsave("07.Grafico_tipo_variables.pdf", plot3, width = 8, height = 6)


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

#Creo lista con todas las variables que son 0 y aquellas con pocos valores distintos de 0 
to_delete <- c("num_reemb_var17_hace3","num_meses_var13_medio_ult3","num_var7_emit_ult1","num_var6","num_var6_0",
               "num_trasp_var17_in_ult1","delta_imp_reemb_var33_1y3","delta_imp_trasp_var33_out_1y3","delta_num_reemb_var33_1y3",
               "delta_num_trasp_var33_out_1y3","delta_imp_amort_var18_1y3","delta_imp_amort_var34_1y3","delta_imp_trasp_var17_out_1y3",
               "delta_num_trasp_var17_out_1y3","imp_reemb_var17_hace3","delta_num_trasp_var17_in_1y3","delta_imp_trasp_var33_in_1y3",
               "delta_num_trasp_var33_in_1y3","delta_imp_trasp_var17_in_1y3","num_meses_var29_ult3","num_aport_var33_ult1",
               "num_trasp_var17_in_hace3","num_venta_var44_hace3","imp_trasp_var17_in_hace3","saldo_medio_var13_medio_hace2",
               "imp_amort_var18_ult1","imp_amort_var34_ult1","num_meses_var13_largo_ult3","num_meses_var33_ult3","num_aport_var33_hace3",
               "num_compra_var44_hace3","delta_num_aport_var33_1y3","imp_venta_var44_hace3","num_op_var40_hace3","delta_imp_venta_var44_1y3",
               "delta_num_venta_var44_1y3","num_aport_var17_hace3","imp_aport_var33_ult1","imp_trasp_var17_in_ult1",  "saldo_var33",
               "imp_trasp_var17_out_ult1","imp_venta_var44_ult1","imp_trasp_var33_in_ult1","imp_trasp_var33_in_hace3",
               "saldo_medio_var13_largo_ult1","saldo_medio_var13_largo_ult3","saldo_medio_var13_largo_hace2","saldo_medio_var13_largo_hace3",
               "saldo_var20","saldo_medio_var44_hace2","saldo_medio_var17_hace2","saldo_medio_var33_ult1","saldo_medio_var33_ult3",
               "saldo_medio_var33_hace2","imp_compra_var44_hace3","saldo_medio_var44_hace3","saldo_medio_var33_hace3",
               "delta_imp_aport_var33_1y3","imp_aport_var33_hace3","saldo_medio_var17_hace3","imp_var7_emit_ult1","saldo_var34",
               "saldo_var29","imp_reemb_var33_ult1","num_venta_var44_ult1","ind_var13_medio","ind_var13_medio_0","ind_var18",
               "ind_var18_0","ind_var29","ind_var34","ind_var34_0","num_var20","num_var20_0","num_var13_medio","num_var13_medio_0",
               "num_var18","num_var18_0","num_var29","num_var34","num_var34_0","num_var29_0","imp_trasp_var33_out_ult1",
               "imp_aport_var17_hace3","ind_var20","ind_var20_0","num_var33","saldo_var18","saldo_var13_medio","saldo_var6",
               "num_var33_0","num_var24_0","ind_var33","ind_var33_0","ind_var29_0","ind_var6","ind_var7_emit_ult1","ind_var6_0",
               "ind_var2","ind_var2_0","ind_var27","ind_var27_0","ind_var28","ind_var28_0","ind_var41","ind_var46","ind_var46_0",
               "imp_amort_var18_hace3","imp_amort_var34_hace3","imp_reemb_var13_hace3","saldo_medio_var13_medio_hace3","saldo_var2_ult1",
               "saldo_var27","saldo_var28","saldo_var41","saldo_var46","imp_reemb_var33_hace3","imp_trasp_var17_out_hace3",
               "imp_trasp_var33_out_hace3","num_reemb_var13_hace3","num_reemb_var33_hace3","num_trasp_var17_out_hace3",
               "num_trasp_var33_out_hace3","num_var2_0_ult1","num_var2_ult1","num_var27","num_var27_0","num_var28","num_var28_0",
               "num_var41","num_var46","num_var46_0","saldo_medio_var29_hace3","num_reemb_var33_ult1","saldo_medio_var13_medio_ult1",
               "saldo_medio_var29_ult1","saldo_medio_var29_ult3","saldo_medio_var29_hace2","num_trasp_var17_out_ult1","num_trasp_var33_in_ult1",
               "num_trasp_var33_in_hace3","num_trasp_var33_out_ult1","saldo_medio_var13_medio_ult3")

#Eliminamos las columnas 
fin_db <- datos[ , !names(datos) %in% to_delete]

#Comprobamos las tablas de contingencia de las variables restantes en fin_db 
f_iter_var(fin_db)

#Variables con valores perdidos "var3",  "var36", "delta_imp_aport_var13_1y3", "delta_imp_aport_var17_1y3", "delta_imp_compra_var44_1y3", 
#"delta_imp_reemb_var13_1y3", "delta_imp_reemb_var17_1y3", "delta_num_aport_var13_1y3","delta_num_aport_var17_1y3", "delta_num_compra_var44_1y3",
# "delta_num_reemb_var13_1y3", "delta_num_reemb_var17_1y3", (comprobar saldo_var_30)

cozas <- c("var3",  "var36", "delta_imp_aport_var13_1y3", "delta_imp_aport_var17_1y3", "delta_imp_compra_var44_1y3", 
  "delta_imp_reemb_var13_1y3", "delta_imp_reemb_var17_1y3", "delta_num_aport_var13_1y3","delta_num_aport_var17_1y3", 
  "delta_num_compra_var44_1y3", "delta_num_reemb_var13_1y3", "delta_num_reemb_var17_1y3")

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

#Creamos una nueva variable con las variable no borradas
stats_df$non_del <- stats_df$tot_vars - stats_df$del_vars 

#Creamos un df con el análisis del número de ceros de cada supervariable
stats_df_del <- melt(stats_df[ ,c("nom_var", "del_vars", "non_del")])


#Dibujamos un gráfico que muestre de dónde hemos eliminado las variables
#Dibujamos el gráfico 1 
plot1 <- ggplot(stats_df_del, aes(y = value, x = nom_var, fill = rev(variable))) +  geom_bar(position = "fill", stat = "identity") + 
  coord_flip() + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="bottom") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(title="% de variables eliminadas", x="Variables", y="Porcentaje") + scale_x_discrete(limits = rev(nom_variables)) +
  scale_fill_manual("", values=c("#666777", "#EE2333"), labels=c("Mantenidas", "Eliminadas")) +
  scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%"))
#Dibujamos el gráfico 2
plot2 <- ggplot(stats_df_del, aes(y = value, x = nom_var, fill = rev(variable))) +  geom_bar(stat = "identity") + 
  coord_flip() + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="bottom") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(title="Total de variables eliminadas", x="", y="Nº de variables") + scale_x_discrete(limits = rev(nom_variables)) +
  scale_fill_manual("", values=c("#666777", "#EE2333"), labels=c("Mantenidas", "Eliminadas"))
#Extraemos la leyenda común de los gráficos
mylegend <- g_legend(plot1)
#Dibujamos el gráfico final combinando los dos gráficos y la leyenda
plot3 <- grid.arrange(arrangeGrob(plot1 + theme(legend.position="none"), plot2 + theme(legend.position="none"),nrow=1),
                      mylegend, nrow=2,heights=c(10, 1))
ggsave("08.Grafico_variables_eliminadas.pdf", plot3, width = 8, height = 6)


#Para hacer boxplots de los datos finales
make_boxplots <- function(database){
  for (i in 1:ncol(database)){
    print(ggplot(database, aes(x=make.names(TARGET), y=database[,i], fill=make.names(TARGET))) + labs(title = colnames(database)[i]) +
            geom_boxplot())
    dev.copy(png, paste(i, " - BOXPLOT - ",colnames(database)[i],'.png'))
    dev.off()
  }
}
make_boxplots(fin_db)


##### Pie chart - Validacion, test, entrenamiento #####
df <- data.frame(
  group = c("Entrenamiento", "Validacion", "Test"),
  value = c(60816, 15204, 75818)
)

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

# Barplot
bp<- ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity") + coord_polar("y") + scale_fill_grey() + theme_minimal() + blank_theme +
  theme(axis.text.x=element_blank()) + guides(fill=guide_legend(title="Datos de:"))
bp
ggsave("09.Datos_test_entrenamiento_validacion.pdf", bp, width = 8, height = 6)


df2 <- data.frame(
  group = c("Entrenamiento", "Test"),
  value = c(76020, 75818)
)

# Barplot
bp<- ggplot(df2, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity") + coord_polar("y") + scale_fill_grey() + theme_minimal() + blank_theme +
  theme(axis.text.x=element_blank()) + guides(fill=guide_legend(title="Datos de:"))
bp
ggsave("10.Datos_test_entrenamiento.pdf", bp, width = 8, height = 6)

### GRÁFICO TREE
stats_df$peso <- sample( letters[1:3], 46, replace=TRUE)
treeplot <- ggplot(stats_df, aes(area = tot_vars, fill = peso, label=nom_var)) +
  treemapify::geom_treemap(colour = "black") + treemapify::geom_treemap_text(colour = "black") + theme(legend.position="none") +
  scale_fill_brewer(palette = "Greys") 
plot(treeplot)
ggsave("11.Treeplot.pdf", treeplot, width = 8, height = 4.75)
