library(raster) #TRABAJO CON DATOS RASTER
library(HH) #VARIANCE INFLATION FACTOR
library(rgeos) #OPERACIONES GEOM√âTRICAS CON INFO GEOGR√ÅFICA
library(dismo) #LIBRERIA PARA MODELOS DE DISTRIBUCION
library(rgdal) # LIBRERÕA PARA VECTORES Y SHAPES

#ANALISIS DE CORRELACI”N ESPACIAL DE LAS VARIABLES PREDICTIVAS Y PRESENCIAS

#LISTADO DE VARIABLES 
variables <- list.files(path="~/ensayos/variables2",pattern='*.asc', full.names=TRUE)
variables
variables <- stack(variables)

#PRESENCIAS COMPLETAS
pres.comp<-read.table("~/especies/dalea_azurea/dalea_azurea.csv",header=T, sep=',', fill=TRUE, check.names=TRUE, stringsAsFactors=FALSE)
presencia<-pres.comp
presencias.y.variables<-extract(x=variables, y=presencia[ , c("longdec","latdec")]) #union de presencias y variables
presencias.y.variables<-data.frame(presencias.y.variables) #convertir a matriz data.frame
presencia<-data.frame(presencia, presencias.y.variables) #unir las coordenadas de presencias y variables a una sola tabla
#eliminar valores nulos

presencia <- na.omit(presencia) #elimina todas filas con valores NA
presencia
#guardar presencias y variables
write.csv(presencia, file = "~/especies/dalea_azurea/presencia&variables.csv")

#REDUCCION DE AUTOCORRELACI”N ESPACIAL (FUNCI”N "ReduceSpatialClustering")
res.grados<-xres(variables) #resoluciÛn de variables en grados
celdas.vacias<-1 #se cambia los valores nulos a 1 para la funciÛn 
distancia.minima<-res.grados*celdas.vacias #se calcula la distancia mÌnima basado en la resoluciÛn de variables
distancia.minima*111.19 #Distancia mÌnima calculada en kilÛmetros
#las columnas de coordenadas deben llamarse "latitude" y "longitude"
colnames(presencia)[3]<-"latitude"
colnames(presencia)[2]<-"longitude"

source("C:/Users/juand/Desktop/scripts pendientes ensayos y pruebas/funcionesSDM_taller1.R") #funciÛn
ReduceSpatialClustering 
presencia=ReduceSpatialClustering(data=presencia, minimum.distance=distancia.minima)
#volvemos a reescribir los nombres originales de las coordenadas despuÈs d aplicar la funciÛn
colnames(presencia)[3]<-"y"
colnames(presencia)[2]<-"x"
#guardar presencias finales
write.table(presencia, file="~/especies/dalea_azurea/dalea_azurea_2.csv", sep=",", row.names=FALSE, quote=FALSE)
presencia


#PREPARACI”N MODELACI”N: SAMPLE WITH DATA (SWD)
rm(list = ls())
#presencia <- presencia[,2:ncol(presencia)] #seleccion de coordenadas y variables desde la tabla
library(ggplot2)    # To plot locations
library(maps)       # To access useful maps
library(rasterVis)  # To plot raster objects
library(raster)

#BASE DE DATOS DE PRESENCIAS Y VARIABLES
occs <- read.csv("~/especies/dalea_azurea/dalea_azurea_2.csv") #Datos de presencia
layers <- stack(list.files("C:/Users/juand/Desktop/variables2","asc",full.names=TRUE))
names(layers)

#PRESENCIAS
pres<-extract(layers, occs[,2:3]) #variables asociadas a las presencias por medio de funciÛn "extract"
pres.coord <- (occs[,2:3]) #coordenadas de las presencias "x" e "y"
pres<- cbind(pres.coord, pres) #agregar coordenadas
pres<-na.omit(pres) #eliminar NA
pres<-unique(pres) #eliminar datos duplicados en caso de que existan

#BACKGROUND
bkg.coord <- randomPoints(mask=layers, n=nrow(pres)*417) # background con 10000 puntos aleatorios (*417 es un n˙mero multiplicado x la cantidad de presencias)
bkg.coord <- data.frame(bkg.coord) #crear matriz
bkg <- data.frame(extract(layers, bkg.coord)) # variables asociadas a los puntos aleotorios de background
bkg <- cbind(bkg.coord, bkg) #agregar coordenadas a los puntos aleatorios de background

#UNION DE PRESENCIAS Y BACKGROUND
#agregar nueva columna "presencia"  (presencias y background)
pres$presencia <- 1 #presencia = 1
bkg$presencia <- 0 #presencia (background) = 0. revisar tabla
pres.bkg<-rbind(pres,bkg) #union de coordenadas de presencias y ausencias, con variables asociadas

#REPRESENTACI”N ESPACIAL DE PRESENCIAS Y BACKGROUND
#seleccion entre 0 y 1 (presencia y background) a partir de la tabla anterior
pres1 <-pres.bkg[pres.bkg$presencia == "1",] #selecciÛn de presencias
bkg1 <- pres.bkg[pres.bkg$presencia == "0",] #selecciÛn de background
#seleccion solo coordenadas entre presencias y background
pres1 <- (pres1[,1:2]) #coordenadas presencias
bkg1 <- (bkg1[,1:2]) #coordenadas background

#MAPA CON PRESENCIAS Y BACKGROUND

ggplot() +
  geom_point(data = pres1, aes(x = x, y = y), color = "red",
              alpha = 0.4, size = 1) +
  geom_point(data = bkg1, aes(x = x, y = y), color = "blue",
              alpha = 0.2, size = 0.2) +
  labs(title = "Presencias y Background", x = "longitud", y = "latitud") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.2),
        legend.title = element_text(colour="blue", size=10, 
                                    face="bold")) +
  coord_fixed() +
  scale_x_continuous(limits = c(-72, -67)) +
  scale_y_continuous(limits = c(-30, -20))


#MODELO MAXENT 

library(ENMeval)
library(ggplot2)
library(raster)
library(dismo)
library(rJava)

maxent()

oc <- (pres[,3:15]) #seleccion sÛlo variables de presencias
bg <- (bkg[,3:15]) #selecicon sÛlo variables de background

oc.bg<-data.frame(rbind(oc,bg))

#Etiquetar presencias (1) y background (0)
y <- c(rep(1,nrow(oc)), rep(0,nrow(bg)))
me <- maxent(oc.bg, y, args=c("addsamplestobackground=true"), 
             path="C:/Users/juand/Desktop/mxent/dalea_azurea/outputR2")

#contribucion por variable en el modelo Maxent 
var.importance(me)
me_df <- var.importance(me)
write.table(me_df, file="~/GEO/PUCV y we· 2020/plots finales/contribuciÛn variables/dalea_azurea.csv", sep=",", row.names=FALSE, quote=FALSE)

gr2 <- ggplot(me_df, aes(x=reorder(variable, percent.contribution), y = percent.contribution)) + 
  geom_bar(stat="identity", width=0.5, fill = "grey") 
gr2 + xlab("variables") + ylab("% porcentaje de contribuciÛn") + 
  ggtitle("contribuciÛn por variable modelo estandar maxent\n percentil (%)") + theme_bw()+ coord_flip()

#crear mapa
map <- predict(me, layers, progress="text")
plot(map)
class(map)

#histograma
map_df <- as.data.frame(map, xy=T) # transformar mapa en un data.frame
map_df <- na.omit(map_df) #eliminar valores nulos

ggplot(map_df, aes(x=layer)) +
  geom_histogram(color="darkgreen", fill="#00B050") +
  labs(title="Pixeles de EntropÌa modelo MAXENT",x="Valores de EntropÌa", y = "Cantidad de pixeles")+
  theme_classic() + scale_y_continuous(breaks=seq(0, 300000, 50000))

#Guardar los resultados
writeRaster(map,"C:/Users/juand/Desktop/mxent/dalea_azurea/dalea_azurea.tif",overwrite=T)
save(me,file="C:/Users/juand/Desktop/mxent/dalea_azurea/outputR2/mx_obj.RData")

#proyectar modelo y presencias
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

#respuesta por variables
load("C:/Users/juand/Desktop/mxent/dalea_azurea/outputR2/mx_obj.RData")
#respuestas por variables (13 variables)

resp <- data.frame(response(me, var = 1))
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Respuesta ElevaciÛn")

resp <- data.frame(response(me, var = 2))
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Respuesta estacionalidad")

resp <- data.frame(response(me, var = 3))
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta ")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Respuesta EvapotranspiraciÛn")

resp <- data.frame(response(me, var = 4))
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Respuesta humedad relativa")

resp <- data.frame(response(me, var = 5))
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Respuesta Ìndice humedad topogr·fica")

resp <- data.frame(response(me, var = 6))
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Respuesta Ìndice posiciÛn topogr·fica")

resp <- data.frame(response(me, var = 7))
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Respuesta cobertura nubosidad")

resp <- data.frame(response(me, var = 8))
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Respuesta orientaciÛn")

resp <- data.frame(response(me, var = 9))
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Respuesta pendiente")

resp <- data.frame(response(me, var = 10))
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Respuesta precipitaciÛn acumulada")

resp <- data.frame(response(me, var = 11))
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("ProducciÛn primaria neta")

resp <- data.frame(response(me, var = 12))
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Respuesta curvatura tangencial")

resp <- data.frame(response(me, var = 13))
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Respuesta temperatura media")


#EvaluaciÛn usando kfold partitioning

#Ejemplo para 1 fold, validaciÛn cruzada. dividir las presencias en grupos
fold <- kfold(oc, k=2) #Genera un indice aleatorio de los folds
occtest <- oc[fold == 1, ] #de k grupos, llamo al 1∞, % de datos para probar el modelo
occtrain <- oc[fold != 1, ] #grupos restantes, menos el 1∞ para el entranamiento del modelo, % datos para entrenar al modelo
y_env<-c(rep(1,nrow(occtrain)), rep(0,nrow(bg))) #1∞ grupos con presencias y ausencias
env.values<-data.frame(rbind(occtrain, bg))

me_env <- maxent(env.values, y_env, args=c("addsamplestobackground=true"), path="C:/Users/juand/Desktop/mxent/dalea_azurea/outputR3")
ev <- evaluate(me_env, p=data.frame(occtest), a=data.frame(bg)) #evaluar prediccion sobre el conjunto de datos creado
str(ev)


#b˙squeda de umbrales

#Computing True Skill Statistic = TPR(Sensitivity)+TNR(Specificity)-1
tss <- ev@TPR+ev@TNR-1
plot(ev@t,tss,type="l")
ev@t[which.max(tss)]
#AUC Plot: X=1-Specificity, Y=Sensitivity
plot((1-ev@TNR),ev@TPR,type="l",xlab="Fractional Predicted Area (1 - Specificity",
     ylab="Sensitiviy")
ev@auc

plot(ev, "ROC")

#Now, for all folds
auc<-rep(NA,2) #crea vector vacÌas para guardar valores de AUC y umbrales
max.tss<-rep(NA,2)
for (i in 1:2){ #cambiar el numero de ciclo de acuerdo a los n∞ de k
  occtest <- oc[fold == i, ]
  occtrain <- oc[fold != i, ]
  env.values<-data.frame(rbind(occtrain, bg))
  y_env<-c(rep(1,nrow(occtrain)), rep(0,nrow(bg)))
  me_env2 <- maxent(env.values, y_env, args=c("addsamplestobackground=true"),
               path="C:/Users/juand/Desktop/mxent/dalea_azurea/outputR4")
  ev2<-evaluate(me_env2, p=data.frame(occtest), a=data.frame(bg))
  auc[i]<-ev2@auc
  lines((1-ev2@TNR),ev2@TPR)
  tss<-ev2@TPR+ev2@TNR-1
  max.tss[i]<-ev2@t[which.max(tss)]
}

mean(auc)
sd(auc) #variaciÛn 
mean(max.tss) #umbral promedio

#Comparar visualmente modelos aplicando un threshold
par(mfrow=c(1,2))
umbral.tss <- mean(max.tss)
plot(map >= umbral.tss, main = "threshold") #crear objeto raster con umbral
plot(map, main = "Dalea azurea")
points(occs, pch='+', cex=0.5, col='black')
umbral <- (map >= umbral.tss)
class(umbral)
writeRaster(umbral,"C:/Users/juand/Desktop/mxent/dalea_azurea/dalea_azurea_umbral.tif",overwrite=T)


#correlacion entre variables y presencias
library(corrgram)
library(RColorBrewer)
require(pacman)
pacman::p_load(raster, rgdal, rgeos,  velox, usdm, gtools, tidyverse, corrplot, Hmisc)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999,
        stringsAsFactors = FALSE)
#correlaciones 
corr_var <- read.csv("~/especies/dalea_azurea/presencia&variables.csv")
corr_var
corr_var <- corr_var[,4:ncol(corr_var)]
#correlaciones con cluster
col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
                           "cyan", "#007FFF", "blue", "#00007F"))
corrplot(corrgram(corr_var), type = "upper", order = "hclust", addrect = 4,
         col = col1(100))








           