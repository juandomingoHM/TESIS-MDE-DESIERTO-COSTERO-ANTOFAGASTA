#TRABAJO DE TITULO: "DISTRIBICION POTENCIAL DE ESPECIES ENDEMICAS CON PROBLEMAS DE CONSERVACION EN EL DESIERTO COSTERO DE ANTOFAGASTA"
#NOMBRE: JUAN DOMINGO HUENUQUEO MARILLAN
#FECHA: 16/10/2020
#TEMA:  ANALISIS DE CORRELACION CON CORREGRAMAS


#Librerias
library(corrgram) #grafico de corregramas
library(RColorBrewer) #paleta de colores
require(pacman) #conjunto de paquetes de apoyo para corrgram
pacman::p_load(raster, rgdal, rgeos,  velox, usdm, gtools, tidyverse, corrplot, Hmisc)


# 21 CORREGRAMAS DE VARIABLES Y PRESENCIAS

#Limpieza y reinicio del programa R
g <- gc(reset = TRUE) #es necesario resetear las librerias y objetos previos
rm(list = ls()) #limpieza de objetos
options(scipen = 999,
        stringsAsFactors = FALSE)

#Corregramas de presencias sobre variables
corr_var <- read.csv("~/especies/adesmia_melanocaulos/presencia&variables.csv") #matriz que contiene correlacion
corr_var
corr_var <- corr_var[,4:ncol(corr_var)] #seleccionar aquellas columnas que contengan la correlacion, en mi caso parten desde la columna N°4

#Ploteode Corregramas
col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
                           "cyan", "#007FFF", "blue", "#00007F")) #seleccion de una paleta de colores para el corregrama
corrplot(corrgram(corr_var), type = "upper", order = "hclust", addrect = 4,
         col = col1(100)) #visualizacion del corregrama

