#TRABAJO DE TITULO: "DISTRIBICION POTENCIAL DE ESPECIES ENDEMICAS CON PROBLEMAS DE CONSERVACION EN EL DESIERTO COSTERO DE ANTOFAGASTA"
#NOMBRE: JUAN DOMINGO HUENUQUEO MARILLAN
#FECHA: 16/10/2020
#TEMA:  MODELACION DE DISTRIBUCION DE ESPECIES CON MAXENT

#Librerias
library(ENMeval) #Funciones para modelacion
library(ggplot2) #graficas
library(raster)
library(dismo) #funciones parar modelacion
library(rJava) #funcion para modelar, posee la tecnica MAXENT
maxent() #version de maxent 3.4.1

# 19 MODELACION MAXENT

#Seleccion de matrices de presencias y variables, background a partir de tablas generadas en SWD
oc <- (pres[,3:15]) #seleccion solo de presencias sobre variables
bg <- (bkg[,3:15]) #seleccion solo de background
oc.bg<-data.frame(rbind(oc,bg)) #generar matriz dataframe que une presencias sobre variables y background

#Etiquetar presencias sobre variables (1 = presencia) y background (0 = ausencia)
y <- c(rep(1,nrow(oc)), rep(0,nrow(bg))) #etiquetar con numeros binarios
me <- maxent(oc.bg, y, args=c("addsamplestobackground=true"), #aplicacion de modelo MAXENT
             path="C:/Users/juand/Desktop/mxent/adesmia_melanocaulos/outputR2")

#Contribucion por variable en el modelo Maxent 
var.importance(me)#revision visual de las variables que mas contribuyen al modelo generado
me_df <- var.importance(me) #tranformar datos de contribucion en una matriz
gr2 <- ggplot(me_df, aes(x=reorder(variable, percent.contribution), y = percent.contribution)) + 
  geom_bar(stat="identity", width=0.5, fill = "grey")  #graficar contribuciones en ggplot
gr2 + xlab("variables") + ylab("% porcentaje de contribución") + 
  ggtitle("contribución por variable modelo estandar maxent\n percentil (%)") + theme_bw()+ coord_flip()

#Crear mapa de prediccion a partir del modelo generado
map <- predict(me, layers, progress="text")
plot(map) #visualizacion

#Histograma de valores de entropia
map_df <- as.data.frame(map, xy=T) # transformar mapa en un data.frame
map_df <- na.omit(map_df) #eliminar valores nulos

ggplot(map_df, aes(x=layer)) +
  geom_histogram(color="darkgreen", fill="#00B050") +
  labs(title="Pixeles de Entropía modelo MAXENT",x="Valores de Entropía", y = "Cantidad de pixeles")+
  theme_classic() + scale_y_continuous(breaks=seq(0, 300000, 50000)) #visualizacion de valores de pixel segun la  entropia en el modelo

#Guardar los resultados
writeRaster(map,"C:/Users/juand/Desktop/mxent/adesmia_melanocaulos/adesmia_melanocaulos.tif",overwrite=T)
save(me,file="C:/Users/juand/Desktop/mxent/adesmia_melanocaulos/outputR2/mx_obj.RData")

#Proyectar modelo y presencias
gplot(map) +
  geom_tile(aes(fill = value)) +
  coord_equal() +
  geom_jitter(data = pres1, aes(x = x, y = y), color = "green",
              alpha = 0.4, size = 1) +
  scale_fill_gradientn(colours = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
                       na.value = "grey5",
                       name = "porcentaje (%)") +
  labs(title = "Alstroemeria graminea",
       x = "longitud",
       y = "latitud") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

#Respuesta por variables en el modelo
load("C:/Users/juand/Desktop/mxent/adesmia_melanocaulos/outputR2/mx_obj.RData") #se llama al modelo guardado anteriormente

#Respuestas por variable en el modelo (en mi caso son 13 variables que se utlizaron en el modelo)
resp <- data.frame(response(me, var = 1)) #respuesta de Elevacion
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Respuesta Elevación")

resp <- data.frame(response(me, var = 2)) #respuesta de produccion primaria acumulada
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Respuesta produccion primaria acumulada")

resp <- data.frame(response(me, var = 3)) #respuesta de evapotranspiracion potencial
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta ")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Respuesta Evapotranspiración")

resp <- data.frame(response(me, var = 4)) #respuesta de humedad relativa
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Respuesta humedad relativa")

resp <- data.frame(response(me, var = 5)) #respuesta de indice humedad topografica
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Respuesta índice humedad topográfica")

resp <- data.frame(response(me, var = 6))
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Respuesta índice posición topográfica")

resp <- data.frame(response(me, var = 7)) #respuesta de cobertura de nubosidad
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Respuesta cobertura nubosidad")

resp <- data.frame(response(me, var = 8)) #respuesta de orientacion de relieve
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Respuesta orientación")

resp <- data.frame(response(me, var = 9)) #respuesta de pendiente de terreno
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Respuesta pendiente")

resp <- data.frame(response(me, var = 10)) #respuesta de percipitacion acumulada
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Respuesta precipitación acumulada")

resp <- data.frame(response(me, var = 11)) #respuesta de produccion primaria media
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Producción primaria media")

resp <- data.frame(response(me, var = 12)) #respuesta de curvatura tangencial
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Respuesta curvatura tangencial")

resp <- data.frame(response(me, var = 13)) #respuesta de temperatura media
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Respuesta temperatura media")


# 20 EVALUACION Y CALIBRACION DEL MODELO MAXENT

#Evaluación del modelo usando kfold partitioning (validacion cruzada que divide las presencias en grupos y luego las evalua)
fold <- kfold(oc, k=4) #Genera un indice aleatorio de los folds (grupos de entrenamientos, k= N° de grupos)
occtest <- oc[fold == 1, ] #de k grupos, llamo al % de datos para probar el modelo
occtrain <- oc[fold != 1, ] #grupos restantes, menos el 1° grupo para el entranamiento del modelo, % datos para entrenar al modelo
y_env<-c(rep(1,nrow(occtrain)), rep(0,nrow(bg))) #1° grupos con presencias y ausencias
env.values<-data.frame(rbind(occtrain, bg)) #se genera un dataframe
me_env <- maxent(env.values, y_env, args=c("addsamplestobackground=true"), path="C:/Users/juand/Desktop/mxent/adesmia_melanocaulos/outputR3") #evaluacion del modelo con uno de los grupos k-fold
ev <- evaluate(me_env, p=data.frame(occtest), a=data.frame(bg)) #evaluar prediccion sobre el conjunto de datos creado
str(ev) #estadisticas de la evaluacion generada por grupos

#Calculo de AUC (Area bajo la Curva)
tss <- ev@TPR+ev@TNR-1 #Computing True Skill Statistic = TPR(Sensitivity)+TNR(Specificity)-1
plot(ev@t,tss,type="l") #visualizacion de area bajo la curva
ev@t[which.max(tss)] #evaluacion
plot((1-ev@TNR),ev@TPR,type="l",xlab="Fractional Predicted Area (1 - Specificity",
     ylab="Sensitiviy") #AUC Plot: X=1-Specificity, Y=Sensitivity, visualizacion final
ev@auc #evaluacion de AUC
plot(ev, "ROC") #grafico de curva

#Evaluacion del modelo con todos los grupos de entramiento K-fold para AUC y UMBRALES
auc<-rep(NA,4) #crea vector vacías para guardar valores de AUC para cada grupo de particion de k-fold
max.tss<-rep(NA,4) #crea vector vacías para guardar valores de UMBRALES para cada grupo de particion de k-fold
for (i in 1:4){ #cambiar el numero de ciclo de acuerdo a los n° de particiones de k-fold
  occtest <- oc[fold == i, ] #particion i (grupo de entreamiento del modelo seleccionado)
  occtrain <- oc[fold != i, ] #particion restante (grupo de entrenamiento del modelo modelo restante)
  env.values<-data.frame(rbind(occtrain, bg)) #evaluacion y union de particiones por cada grupo con el background
  y_env<-c(rep(1,nrow(occtrain)), rep(0,nrow(bg))) #particion i = 1, background = 0 por cada grupo de entrenamiento 
  me_env2 <- maxent(env.values, y_env, args=c("addsamplestobackground=true"),
                    path="C:/Users/juand/Desktop/mxent/adesmia_melanocaulos/outputR4") #generacion de modelos por cada particion seleccionada
  ev2<-evaluate(me_env2, p=data.frame(occtest), a=data.frame(bg)) #evaluacion del modelo por cada particion seleciconada
  auc[i]<-ev2@auc #curva AUC por cada particion seleccionada
  lines((1-ev2@TNR),ev2@TPR)#curva AUC
  tss<-ev2@TPR+ev2@TNR-1 #umbral por cada particion seleccionada
  max.tss[i]<-ev2@t[which.max(tss)] #umbral de corte
}

#Promedio de la evaluacion generada por los grupos de particion k-fold
mean(auc) #promedio de curva AUC de las particiones generadas
sd(auc) #variación por cada particion generada
mean(max.tss) #umbral promedio por cada particion generada

#Comparar visualmente modelos aplicando un threshold (umbral) y su prediccion inicial
par(mfrow=c(1,2)) #ploteo para dos columnas (2 mapas)
umbral.tss <- mean(max.tss) #generacion de umbral promedio dada el grupo de entrenamiento (particiones k-fold)
plot(map >= umbral.tss, main = "threshold ") #Visualizacion de umbral promedio de corte del modelo
plot(map, main = "Adesmia melanocaulos") #visualizacion de la prediccion del modelo
points(occs, pch='+', cex=0.5, col='black') #agregar presencias a la visualizacion
umbral <- (map >= umbral.tss) #visualizar la idoneidad del modelo a partir del umbral medio
writeRaster(umbral,"C:/Users/juand/Desktop/mxent/adesmia_melanocaulos/adesmia_melanocaulos_umbral.tif",overwrite=T) #guardar idoneidad del modelo (umbral de corte)