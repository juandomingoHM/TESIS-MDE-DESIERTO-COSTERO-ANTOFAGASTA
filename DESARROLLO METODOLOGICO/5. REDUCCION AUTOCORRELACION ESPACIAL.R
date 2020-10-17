#TRABAJO DE TITULO: "DISTRIBICION POTENCIAL DE ESPECIES ENDEMICAS CON PROBLEMAS DE CONSERVACION EN EL DESIERTO COSTERO DE ANTOFAGASTA"
#NOMBRE: JUAN DOMINGO HUENUQUEO MARILLAN
#FECHA: 15/10/2020
#TEMA:  REDUCCION DE AUTOCORRELACION ESPACIAL DE PRESENCIAS


#ANALISIS DE CORRELACION ESPACIAL DE LAS VARIABLES PREDICTIVAS Y PRESENCIAS

#Librerias utilizadas
library(raster) 
library(HH) #Variance Inflation Factor
library(rgeos) 
library(dismo) #Operaciones y funciones para modelacion de distribucion potencial
library(rgdal) 


# 15  REDUCCION DE AUTOCORRELACION ESPACIAL DE LAS PRESENCIAS DE LA ESPECIE A MODELAR

#Listado de variables seleccionadas
variables <- list.files(path="~/ensayos/variables2",pattern='*.asc', full.names=TRUE)
variables
variables <- stack(variables)

#Presencias de la especie que se pretende modelar 
pres.comp<-read.table("~/especies/adesmia_melanocaulos/adesmia_melanocaulos.csv",header=T, sep=',', fill=TRUE, check.names=TRUE, stringsAsFactors=FALSE)
presencia<-pres.comp
presencias.y.variables<-extract(x=variables, y=presencia[ , c("longdec","latdec")]) #union de presencias y variables dada la longitud y latitud
presencias.y.variables<-data.frame(presencias.y.variables) #convertir a matriz data.frame
presencia<-data.frame(presencia, presencias.y.variables) #unir las coordenadas de presencias y variables a una sola tabla

#eliminar valores nulos
presencia <- na.omit(presencia) #elimina todas los valores nulos (NA)
presencia

#guardar matriz de presencias sobre las variables de la especie
write.csv(presencia, file = "~/especies/adesmia_melanocaulos/presencia&variables.csv")


# 16 REDUCCION DE AUTOCORRELACIÓN ESPACIAL (FUNCIÓN "ReduceSpatialClustering")

res.grados<-xres(variables) #resoluciin de variables en grados
celdas.vacias<-1 #se cambia los valores nulos a 1 para la funcion 
distancia.minima<-res.grados*celdas.vacias #se calcula la distancia minima basado en la resolucion de variables
distancia.minima*111.19 #Distancia minima calculada en kilometros

#las columnas de coordenadas deben llamarse "latitude" y "longitude" para aplicar la funcion correctamente
colnames(presencia)[3]<-"latitude"
colnames(presencia)[2]<-"longitude"

#activar la funcion "ReduceSpatialClustering"
source("C:/Users/juand/Desktop/scripts pendientes ensayos y pruebas/funcionesSDM_taller1.R") #Llamar a la funcion (buscar Script "funcionesSDM_taller1.R")
ReduceSpatialClustering #aplicar funcion 
presencia=ReduceSpatialClustering(data=presencia, minimum.distance=distancia.minima)

#volvemos a reescribir los nombres originales de las columnas de coordenadas después de aplicar la funcion
colnames(presencia)[3]<-"y"
colnames(presencia)[2]<-"x"

#guardar presencias finales con reduccion de autocorrelacion espacial
write.table(presencia, file="~/especies/adesmia_melanocaulos/adesmia_melanocaulos_2.csv", sep=",", row.names=FALSE, quote=FALSE)
presencia