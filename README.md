# TESIS-MDE-DESIERTO-COSTERO-ANTOFAGASTA
Repositorio que almacena todos los scripts utilizados en la metodología del desarrollo de la tesis sobre modelos de distribución potencial de especies (SDM) en el desierto costero de Antofagasta. La metodología desarrollada esta a modo de ejemplo utlizando las presencias de Adesmia melanocaulo

#  OASIS DE NIEBLA
![](https://github.com/juandomingoHM/juandomingoHM/blob/main/banner%20oaisis%20de%20neblina.png)

# METODOLOGÍA DESARROLLADA 

# indice

1. 

# Estandarizacion de variables

```r
Librerias requeridas
#Librerias
library(raster) 
library(rgeos) 
library(rgdal) 

```
```r
#Variables para la modelacion con diferente extensiom, resolucion y formato
variable1<-raster("~/ensayos/SRTM/mosaico.tif") #variable de ejemplo
variable2<-raster("~/ensayos/cr2/prec_acumulada/preac_sum.tif") #variable de ejemplo
variable3<-raster("~/ensayos/cr2/variables/preac_mean10.tif") #variable de ejemplo
variable4<-raster("~/ensayos/cr2/variables/temp_mean10.tif") #variable de ejemplo
variable5<-raster("~/ensayos/nubo_resultados/nubo_mean.tif") #variable de ejemplo

#Variable que servira para estandarizar a las dempas
variable.modelo<- raster("~/ensayos/modelo/modelo.asc") #rsolucion 1 km, extension: -72,-69,-30-20 y formato: *.asc

#Consulta de proyeccion y extension de las variables
projection(variable1)
extent(variable1)

#Estandarizar por extension y resolucion por cada variable
variable1<-resample(x=variable1, y=modelo, method="bilinear") #realizar este procedimiento por cada variable

# Revision de la estandarizacion de resolucion y extension
extent(variable1)#revision de extension de una variable cualquiera
xres(variable1)#reviision de resolucion de una variable cualquiera

#Guardar variables estandarizadas por resolucion y extension
writeRaster(variable1, filename="~/ensayos/variables_asc/variable1.asc", format="ascii", overwrite=TRUE)
```
#  ESTANDARIZACION DE VARIABLES: ELIMINACION DE CELDAS NULAS y MASCARA DE AREA DE ESTUDIO

```r
#Mascara de celdas nulas para eliminar aquellas celdas sin datos en el set de variables
valores.nulos=variable1*variable2*variable3*variable4*variable5   #mapa con celdas nulas (multiplicacion entre variables)
par(mfrow=c(1,1)) 
plot(valores.nulos) #visualizacion
```

# ELIMINACION DE CELDAS NULAS A LAS VARIABLES ESTANDARIZADAS

```r
#Mascara de celdas nulas para eliminar aquellas celdas sin datos en el set de variables
valores.nulos=variable1*variable2*variable3*variable4*variable5   #mapa con celdas nulas (multiplicacion entre variables)
par(mfrow=c(1,1)) 
plot(valores.nulos) #visualizacion
```
```r
#Brick para eliminar celdas nulas
variables.brick<-brick(variable1,variable2,variable3,variable4,variable5)

#Mantener los nombres originales de las variables estandarizadas
names(variables.brick)<-c("variable1","variable2","variable3","variable4","variable5")
plot(variables.brick)
```
```r
#aplicación de máscara con celdas con valores nulos (NA) a las variables estandarizadas
variables.brick<-mask(variables.brick, valores.nulos)
plot(variables.brick)
```
```r
#Aplicacion de mascara de chile continental(area de estudio) a las varaibles estandarizadas
Chile <- readOGR(dsn=path.expand("~/DATOS/archivos/shapes/extent_norte"),
                 layer="extent_norte")
plot(Chile)
projection(Chile)
variables.brick<-mask(variables.brick, Chile)
plot(variables.brick)
plot(variables.brick[[1]])
plot(Chile, add=T)
```
```r
#cortar cada capa de variables estadarizadas que estan contenidas en el brick
variables.brick<-crop(x=variables.brick, y=extent(dem30))
plot(variables.brick)

#guardamos las variables preparadas al disco duro por cada variable
writeRaster(variables.brick[["variable1"]], filename="~/ensayos/variables_crop/variable1.asc", format="ascii", overwrite=TRUE)
```
# ESTANDARIZACION DE VARIABLES: MASCARAS DE URBES-AGRICULTURA

```r
#lista de variables estandarizadas
lista.variables <- list.files(path="~/ensayos/variables_crop",pattern='*.asc', full.names=TRUE)
lista.variables

#crear stack para la lista de variables estandarizadas
variables <- stack(lista.variables)
names(variables)
variables
plot(variables[[1]])
```
```r
#capa vectorial de zonas urbanas
urbes <- readOGR(dsn=path.expand("~/DATOS/archivos/shapes/TESIS_MDE/cuad_mask"),
                 layer="cuad_mask")
urbes <- spTransform(urbes,"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") #reproyectar la capa vectorial con la proyeccion de las variables
plot(urbes)
```
```r
projection(urbes) #consulta de proyeccion

#aplicacion de mascaras de urbes a las variables estandarizadas
variables<-mask(variables, urbes)
plot(variables[[1]])
```
```r
#capa vectorial de zonas agricolas
agro <- readOGR(dsn=path.expand("~/DATOS/archivos/shapes/TESIS_MDE/agro_mask_cuad"),
                layer="agro_mask")
agro <- spTransform(agro,"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") #reproyectar la capa vectorial con la proyeccion de las variables
plot(agro)
```
```r
projection(agro) #consulta de proyeccion 

#aplicacion de mascaras de zonas agricolas a las variables estandarizadas
variables<-mask(variables, agro)
plot(variables[[3]])
```
```r
proj4string(variables) <- projection(raster())

# guardar variables estandarizadas finales por cada una 
writeRaster(variables[["variable1"]], filename="~/ensayos/variables/variable1.asc", format="ascii", overwrite=TRUE) #1
```
# MODELACION DE DISTRIBUCION POTENCIAL DE ESPECIES

```r
#Librerias principales
library(HH) #Variance Inflator Vector
library(dismo) #Modelacion de especies
```
# ANALISIS DE CORRELACIÓN ESPACIAL DE LAS VARIABLES PREDICTIVAS Y PRESENCIAS

```r
#Listado de variables
variables <- list.files(path="~/ensayos/variables2",pattern='*.asc', full.names=TRUE)
variables
variables <- stack(variables)

#Presencias completas
pres.comp<-read.table("~/especies/adesmia_melanocaulos/adesmia_melanocaulos.csv",header=T, sep=',', fill=TRUE, check.names=TRUE, stringsAsFactors=FALSE)
presencia<-pres.comp
presencias.y.variables<-extract(x=variables, y=presencia[ , c("longdec","latdec")]) #union de presencias y variables
presencias.y.variables<-data.frame(presencias.y.variables) #convertir a matriz data.frame
presencia<-data.frame(presencia, presencias.y.variables) #unir las coordenadas de presencias y variables a una sola tabla

#Eliminar valores nulos (NA)
presencia <- na.omit(presencia) #elimina todas filas con valores NA
presencia
#guardar presencias y variables
write.csv(presencia, file = "~/especies/adesmia_melanocaulos/presencia&variables.csv")
```
# REDUCCION DE AUTOCORRELACIÓN ESPACIAL (FUNCIÓN "ReduceSpatialClustering")

```r
#Convertir matriz de datos de las presencias para la apliciacion de la funcion
res.grados<-xres(variables) #resolución de variables en grados
celdas.vacias<-1 #se cambia los valores nulos a 1 para la función 
distancia.minima<-res.grados*celdas.vacias #se calcula la distancia mínima basado en la resolución de variables
distancia.minima*111.19 #Distancia mínima calculada en kilómetros
#las columnas de coordenadas deben llamarse "latitude" y "longitude"
colnames(presencia)[3]<-"latitude"
colnames(presencia)[2]<-"longitude"

#llamado de la funcion
source("C:/Users/juand/Desktop/scripts pendientes ensayos y pruebas/funcionesSDM_taller1.R") #función
ReduceSpatialClustering 
presencia=ReduceSpatialClustering(data=presencia, minimum.distance=distancia.minima)
#volvemos a reescribir los nombres originales de las coordenadas después d aplicar la función
colnames(presencia)[3]<-"y"
colnames(presencia)[2]<-"x"
#guardar presencias finales
write.table(presencia, file="~/especies/adesmia_melanocaulos/adesmia_melanocaulos_2.csv", sep=",", row.names=FALSE, quote=FALSE)
presencia
```
# PREPARACIÓN MODELACIÓN: SAMPLE WITH DATA (SWD)

```r
rm(list = ls()) #reset y eliminacion de objetos que ya no utilizaremos
#Librerias necesarias
library(ggplot2)    # visualizacion de objetos
library(maps)       # Mapas de base
library(rasterVis)  # Manipulacion de objetos raster

#Base de datos de presencias y variables
occs <- read.csv("~/especies/adesmia_melanocaulos/adesmia_melanocaulos_2.csv") #Datos de presencia
layers <- stack(list.files("C:/Users/juand/Desktop/variables2","asc",full.names=TRUE))
names(layers)

#Presencias sobre variables
pres<-extract(layers, occs[,2:3]) #variables asociadas a las presencias por medio de función "extract"
pres.coord <- (occs[,2:3]) #coordenadas de las presencias "x" e "y"
pres<- cbind(pres.coord, pres) #agregar coordenadas
pres<-na.omit(pres) #eliminar NA
pres<-unique(pres) #eliminar datos duplicados en caso de que existan

#Generacion de Background
bkg.coord <- randomPoints(mask=layers, n=nrow(pres)*417) # background con 10000 puntos aleatorios (*417 es un número multiplicado x la cantidad de presencias)
bkg.coord <- data.frame(bkg.coord) #crear matriz
bkg <- data.frame(extract(layers, bkg.coord)) # variables asociadas a los puntos aleotorios de background
bkg <- cbind(bkg.coord, bkg) #agregar coordenadas a los puntos aleatorios de background

#Union de presencias sobre variables con background
pres$presencia <- 1 #presencia = 1 agregar nueva columna "presencia"  (presencias y background)
bkg$presencia <- 0 #presencia (background) = 0. revisar tabla
pres.bkg<-rbind(pres,bkg) #union de coordenadas de presencias (presencias sobre variables) y ausencias (background)
```

# MODELO MAXENT

```r
#librerios necesarias
library(ENMeval)
library(ggplot2)
library(rJava)
maxent() #version de MAXENT
```
```r
#Datos de presencias sobre variables (presencias) y background generado (ausencias)
oc <- (pres[,3:15]) #seleccion sólo variables de presencias
bg <- (bkg[,3:15]) #selecicon sólo variables de background
oc.bg<-data.frame(rbind(oc,bg)) #Union de presencias y ausencias

#Etiquetar presencias (1) y background (0)
y <- c(rep(1,nrow(oc)), rep(0,nrow(bg)))

#generacion del modelo
me <- maxent(oc.bg, y, args=c("addsamplestobackground=true"), 
             path="C:/Users/juand/Desktop/mxent/adesmia_melanocaulos/outputR2") 

#contribucion por variable en el modelo Maxent 
var.importance(me)
me_df <- var.importance(me)
gr2 <- ggplot(me_df, aes(x=reorder(variable, percent.contribution), y = percent.contribution)) + 
  geom_bar(stat="identity", width=0.5, fill = "grey") 
gr2 + xlab("variables") + ylab("% porcentaje de contribución") + 
  ggtitle("contribución por variable modelo estandar maxent\n percentil (%)") + theme_bw()+ coord_flip()

#crear mapa de prediccion del modelo
map <- predict(me, layers, progress="text")
plot(map)
```
```r
class(map)
#histograma de pixeles de entropia a partir del modelo
map_df <- as.data.frame(map, xy=T) # transformar mapa en un data.frame
map_df <- na.omit(map_df) #eliminar valores nulos
ggplot(map_df, aes(x=layer)) +
  geom_histogram(color="darkgreen", fill="#00B050") +
  labs(title="Pixeles de Entropía modelo MAXENT",x="Valores de Entropía", y = "Cantidad de pixeles")+
  theme_classic() + scale_y_continuous(breaks=seq(0, 300000, 50000))
```
```r
#Guardar los resultados
writeRaster(map,"C:/Users/juand/Desktop/mxent/adesmia_melanocaulos/adesmia_melanocaulos.tif",overwrite=T)
save(me,file="C:/Users/juand/Desktop/mxent/adesmia_melanocaulos/outputR2/mx_obj.RData")

#respuesta por variables
load("C:/Users/juand/Desktop/mxent/adesmia_melanocaulos/outputR2/mx_obj.RData") #llamar a script del modelo generado

#Consutla de respuestas por variable del modelo 
resp <- data.frame(response(me, var = 1))
ggplot(data = resp) +
  geom_smooth(mapping = aes(x = V1, y = p, linetype = "respuesta")) + 
  scale_y_continuous(limit = c(-1,1)) +  ggtitle("Respuesta Elevación")
```

# 5 EVALUACION DEL MODELO usando kfold partitioning (particiones/grupos de entrenamiento)

#Ejemplo para 1 fold, validación cruzada. dividir las presencias en grupos
fold <- kfold(oc, k=4) #Genera un indice aleatorio de los K-folds
occtest <- oc[fold == 1, ] #de k grupos, llamo al 1°, % de datos para probar el modelo
occtrain <- oc[fold != 1, ] #grupos restantes, menos el 1° para el entranamiento del modelo, % datos para entrenar al modelo
y_env<-c(rep(1,nrow(occtrain)), rep(0,nrow(bg))) #1° grupos con presencias y ausencias
env.values<-data.frame(rbind(occtrain, bg))
me_env <- maxent(env.values, y_env, args=c("addsamplestobackground=true"), path="C:/Users/juand/Desktop/mxent/adesmia_melanocaulos/outputR3")
ev <- evaluate(me_env, p=data.frame(occtest), a=data.frame(bg)) #evaluar prediccion sobre el conjunto de datos creado
str(ev)

#busqueda de umbrales
```r
tss <- ev@TPR+ev@TNR-1 #Computing True Skill Statistic = TPR(Sensitivity)+TNR(Specificity)-1
plot(ev@t,tss,type="l") #Estadisticas del modelo generado a partir del 1° grupo de entrenamiento
ev@t[which.max(tss)] #evaluacion umbral
plot((1-ev@TNR),ev@TPR,type="l",xlab="Fractional Predicted Area (1 - Specificity",
     ylab="Sensitiviy")
ev@auc 
plot(ev, "ROC") #AUC Plot: X=1-Specificity, Y=Sensitivity. Curva Bajo el Umbral
```
```r
#Evaluacion con todos los grupos generados por las particiones K-fold
auc<-rep(NA,4) #crea vector vacio para guardar valores de AUC 
max.tss<-rep(NA,4) #crea vector vacio para guardar valores d umbrales de corte (Threshold)
for (i in 1:4){ #cambiar el numero de ciclo de acuerdo a los n° de particiones k-fold
  occtest <- oc[fold == i, ]
  occtrain <- oc[fold != i, ]
  env.values<-data.frame(rbind(occtrain, bg))
  y_env<-c(rep(1,nrow(occtrain)), rep(0,nrow(bg)))
  me_env2 <- maxent(env.values, y_env, args=c("addsamplestobackground=true"),
                    path="C:/Users/juand/Desktop/mxent/adesmia_melanocaulos/outputR4")
  ev2<-evaluate(me_env2, p=data.frame(occtest), a=data.frame(bg))
  auc[i]<-ev2@auc
  lines((1-ev2@TNR),ev2@TPR)
  tss<-ev2@TPR+ev2@TNR-1
  max.tss[i]<-ev2@t[which.max(tss)]
}

#Promedio de AUC y umbrales a partir de las particiones K-fold
mean(auc) #AUC promedio
sd(auc) #variación de AUC 
mean(max.tss) #umbral promedio

#Comparar visualmente modelos aplicando un threshold
par(mfrow=c(1,2))
umbral.tss <- mean(max.tss)
plot(map >= umbral.tss, main = "threshold ") #crear objeto raster con umbral
plot(map, main = "Adesmia melanocaulos")
points(occs, pch='+', cex=0.5, col='black')
umbral <- (map >= umbral.tss)
```
```r
class(umbral)
writeRaster(umbral,"C:/Users/juand/Desktop/mxent/adesmia_melanocaulos/adesmia_melanocaulos_umbral.tif",overwrite=T)
```

reporte SDM desarrollado: https://rpubs.com/juandomingoHM/678642
repositorios de referencia: https://github.com/BlasBenito/TallerModelosGBIF2019.git 


