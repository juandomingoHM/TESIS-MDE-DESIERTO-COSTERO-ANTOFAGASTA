#TRABAJO DE TITULO: "DISTRIBICION POTENCIAL DE ESPECIES ENDEMICAS CON PROBLEMAS DE CONSERVACION EN EL DESIERTO COSTERO DE ANTOFAGASTA"
#NOMBRE: JUAN DOMINGO HUENUQUEO MARILLAN
#FECHA: 16/10/2020
#TEMA:  SCRIPT DE COBERTURA DE NUBOSIDAD

#Librerias principales
library(sp)
library(raster)
library(rgdal)
library(GISTools)
library(wrspathrow)
library(rgeos)

# 1 MASCARA DE NUBES (MOD035) MEDIANA 2009-2018

#Lista de capas de nubes generadas cada 8 dias 2009-2018
archivos <-list.files("~/ensayos/nubo/2018/Surf_Ref_8Days_500m_v6/state_MOD35_cld", full.names = T, pattern = glob2rx("*.tif")) #lista EVI
archivos[1]
ras <-raster(archivos [1], band=1) #banda MOD035
plot(ras)

#Proyeccion y extension de las cobertura de nubosidad
projection(ras)
e <- extent(-72, -66, -30, -20) #extension utlizada como area de estudio 20°-30°S
cras <-crop (ras, e) #recorte entre EVI + Ext (Taltal-Paposo)
plot(cras)
extent(cras)

#outpath para salidas de la banda MOD035 con nueva extension
salida <- "~/ensayos/nubo/2018/nubo"
id <- substr(archivos,83,114) 
id
out.names <- paste(salida, "/", id,".tif",sep="")
out.names[1]

#ciclo de salidas de la Banda MOD035 con nueva extension para todas las capas por año (2009-2018)
for (i in 1:100) {
  ra<-raster(archivos [i],band= 1)  #Banda EVI
  ra_crop <-crop (ra, e) #corte en base a un ext
  plot(ra_crop)
  writeRaster(ra_crop, out.names[i], format="GTiff", overwrite = T) #guardar raster
  cat("Ext raster Norte grande de banda EVI", i,"hecho. \n") #mensaje de proceso realizado
}

# 2 PPOMEDIO DE COBERTURA NUBOISDAD POR AÑO

#Generar stack y brick de 23 imagenes por año
setwd("~/ensayos/nubo/2018/nubo") #cambiar direccion por cada año 
files <- list.files(pattern='.tif') #extraer listado de archivos
files
imageraster <- raster(files[2])#cargar imagen
extent(imageraster)
projection(imageraster)
plot(imageraster)
rbrick <- imageraster  #asignar a la variable rbrick

#ciclo para compactar las 23 imagenes por año
for (i in 2:length(files)) #
{ 
  imageraster <- raster(files[i])#cargar imagen
  names(imageraster) <- substr(files[i],1, 10)#extraer nombre del archivo
  # acumular las 12 imágenes en una misma variable raster
  rbrick <- addLayer(rbrick,imageraster)
}
class(rbrick)
names(rbrick)
rbrick
summary(rbrick)

#crear stack para la cobertura de nubosidad
stack_brick <- stack(rbrick)
nubosidad_2019 <- calc(stack_brick, mean) #media cover cloud 2019
plot(nubosidad_2019, main= "cobertura promedio anual de nubosidad 2018") #visualizacion de cobertura de nubosidad proimedio 2019
writeRaster(nubosidad_2019, filename = "~/ensayos/nubo_resultados/nubo_mean2018.tif", format= "GTiff", overwrite = T) #guardar resultado por año

# 3 MEDIANA DE COBERTURA DE NUBOSIDAD DE 10 AÑOS (2009-2018)

#Capas de cobertura nubosidad por año
setwd("~/ensayos/nubo_resultados")
files <- list.files(pattern='.tif') #extraer listado de archivos
files
imageraster <- raster(files[1])#cargar imagen
extent(imageraster)
plot(imageraster)
rbrick <- imageraster  #asignar a la variable rbrick

#ciclo para compactar las 10 imagenes promedio de cobertura nubosidad
for (i in 2:length(files)) #
{ 
  imageraster <- raster(files[i])#cargar imagen
  names(imageraster) <- substr(files[i],1, 10)#extraer nombre del archivo
  # acumular las 12 imágenes en una misma variable raster
  rbrick <- addLayer(rbrick,imageraster)
}
class(rbrick)
names(rbrick)

#Calculo de mediana de 10 imagenes promediadas de la cobertura nubosidad (2009-2018)
setwd("~/ensayos/nubo_resultados")
files <- list.files(pattern='.tif') #extraer listado de archivos
files
stack_brick <- stack(files)
cloud_median <- calc(stack_brick, median) #calculo de mediana de 10 imagenes
plot(cloud_median, main= "cobertura mediana nubosidad 2009-2018")
writeRaster(cloud_median, filename = "nubo_median.tif", format= "GTiff", overwrite = T)