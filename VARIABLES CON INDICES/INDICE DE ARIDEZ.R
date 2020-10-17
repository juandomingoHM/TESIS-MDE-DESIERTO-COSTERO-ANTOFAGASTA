#TRABAJO DE TITULO: "DISTRIBICION POTENCIAL DE ESPECIES ENDEMICAS CON PROBLEMAS DE CONSERVACION EN EL DESIERTO COSTERO DE ANTOFAGASTA"
#NOMBRE: JUAN DOMINGO HUENUQUEO MARILLAN
#FECHA: 16/10/2020
#TEMA:  SCRIPT DE CALCULO DE INDICE DE ARIDEZ (MEDIANA 2009-2018)

#Librerias principales
library(raster) #TRABAJO CON DATOS RASTER
library(rgeos) #OPERACIONES GEOMÉTRICAS CON INFO GEOGRÁFICA

# 1 INDICE DE ARIDEZ MARTONNE 1926

#Calculo del indice 
temp<-raster("~/ensayos/cr2/variables/temp_mean10.tif") #temperatura media (mediana 2009-2018)
prec.acum<- raster("~/ensayos/cr2/prec_acumulada/preac_sum.tif") #precipitacion acumulada (mediana 2009-2018)
iL <- (prec.acum/(temp+10)) #indice de Martonne
summary(iL)
plot(iL)
setwd("~/ensayos/cr2/prec_acumulada")
writeRaster(iL, filename = "ia_martonne.tif", format= "GTiff", overwrite = T) #guardar indice en formato raster