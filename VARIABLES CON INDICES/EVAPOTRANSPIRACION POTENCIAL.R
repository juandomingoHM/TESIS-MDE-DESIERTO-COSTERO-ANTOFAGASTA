#TRABAJO DE TITULO: "DISTRIBICION POTENCIAL DE ESPECIES ENDEMICAS CON PROBLEMAS DE CONSERVACION EN EL DESIERTO COSTERO DE ANTOFAGASTA"
#NOMBRE: JUAN DOMINGO HUENUQUEO MARILLAN
#FECHA: 16/10/2020
#TEMA:  SCRIPT DE CALCULO DE EVAPOTRANSPIRACION POTENCIAL (MEDIANA 2009-2018)

#Librerias principales
library(sp)
library(raster)
library(rgdal)
library(GISTools)
library(wrspathrow)
library(rgeos)

# 1 CALCULO DE LA EVAPOTRANSPIRACION POTENCIAL (ETP) (HARGREAVES, 1985)

#temperatura minima mediana (2009-2018) a partir del CR2
archivos <-list.files("~/ensayos/cr2/temp.minima.median", full.names = T, pattern = glob2rx("*.tif")) #lista EVI
archivos[[1]]

#temperatura máxima mediana (2009-2018) a partir del CR2
archivos1 <-list.files("~/ensayos/cr2/evapo", full.names = T, pattern = glob2rx("*.tif")) #lista EVI
archivos1[[1]]

#variables de temperatura minima y maxima 
tmin <- archivos[1]
tmax <- archivos1[1]

#radiacion solar incidente
Rs <- (12.20*0.19*((tmax-tmin)^0.5)) #Rs radiocion solar incidente
#para la zona desertica costera del hemisferio sur es 12.20*0.19 segun
#Tabla de Radiación solar extraterrestre en mm/día (Allen et al., 1998)
plot(Rs)
summary(Rs)

#evapotranspiración potencial (ETP)
ET <- (0.0135*(tmedia + 17.78)*Rs)
plot(ET)
summary(ET)
writeRaster(ET, filename ="~/ensayos/cr2/evapo/ETP.tif", format= "GTiff", overwrite = T)