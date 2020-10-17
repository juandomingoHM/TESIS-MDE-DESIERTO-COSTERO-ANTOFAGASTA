#TRABAJO DE TITULO: "DISTRIBICION POTENCIAL DE ESPECIES ENDEMICAS CON PROBLEMAS DE CONSERVACION EN EL DESIERTO COSTERO DE ANTOFAGASTA"
#NOMBRE: JUAN DOMINGO HUENUQUEO MARILLAN
#FECHA: 16/10/2020
#TEMA:  SCRIPT DE LAS VARIABLES RELACIONADAS A LA PRODUCCION PRIMARIA

#Librerias principales
library(sp)
library(raster)
library(rgdal)
library(GISTools)
library(wrspathrow)
library(rgeos)

# 1 PRODUCCION PRIMARIA MEDIA

#lista de Banda EVI (sola), reproyeccion y extension aplicado al area de estudio
archivos <-list.files("~/ensayos/EVI/2018/VI_16Days_250m_v6/EVI", full.names = T, pattern = glob2rx("*.tif")) #lista EVI
ras <-raster(archivos [1], band=1) #banda EVI (banda 2)
e <- extent(-72, -66, -30, -20) #extension area de estudio
cras <-crop (ras, e) #recorte banda evi con la extension de area de estudio
plot(cras)
extent(cras) #extension nueva
cras <- cras >= 0 #filtrar solo valores positivos incluyendo el 0
plot(cras)

#outpath para salidas de la banda EVI con extension del area de estudio
salida <- "~/ensayos/EVI/2018/EVI"
id <- substr(archivos,65,84)
id
out.names <- paste(salida, "/", id,".tif",sep="")
out.names[1]

#ciclo de salidas de la Banda EVI con ext del area de estudio para todas las imagenes por año (23 por año)
for (i in 1:100) {
  ra<-raster(archivos [i],band= 1)  #Banda EVI
  qa<-raster(archivos2 [i],band= 1) #aplicar bandas de calidad 4
  qa2<- raster(archivos3 [i],band= 1) #aplicar bandas de calidad 12
  ra_crop <-crop (ra, e) #corte en base a un extension de area de estudio
  ra_crop <- ra_crop >= 0 #toma solo valores postivos incluyendo el 0
  plot(ra_crop)
  writeRaster(ra_crop, out.names[i], format="GTiff", overwrite = T) #guardar raster
  cat("Ext raster Norte grande de banda EVI", i,"hecho. \n") #mensaje de proceso realizado
}


# 2 CALCULO DE LA PRODUCCION PRIMARIA NETA ANUAL POR AÑO Y MEDIANA POR DECADA

#Generar stack y brick de 23 imagenes por año
setwd("~/ensayos/EVI/2018/EVI")
files <- list.files(pattern='.tif') #extraer listado de archivos por año
files
imageraster <- raster(files[2])#cargar imagen
extent(imageraster)
projection(imageraster)
plot(imageraster)
rbrick <- imageraster  #asignar a la variable rbrick

#ciclo para compactar las 12 imagenes en un brick
for (i in 2:length(files)) 
{ 
  imageraster <- raster(files[i])#cargar imagen
  rbrick <- addLayer(rbrick,imageraster) # acumular las 12 imágenes en una misma variable raster
}
#12 imagenes promedio de produccion primaria media compactadas para calcular la mediana
class(rbrick)
names(rbrick)
rbrick

#produccion primaria media (ppn) por año (2008-2018)
stack_brick <- stack(rbrick)
plot(stack_brick)
ppn_2018 <- calc(stack_brick, mean) #calculo por año
summary(ppn_2018)
plot(ppn_2018, main= "produccion primaria media 2018") #visualizacion
writeRaster(ppn_2018, filename = "~/ensayos/EVI_resultados/EVI_mean/ppn_2018.tif", format= "GTiff", overwrite = T)

#produccion primaria media final (mediana de los 10 años)
setwd("~/ensayos/EVI_resultados/EVI_mean")
files <- list.files(pattern='.tif') #extraer listado de archivos
files
imageraster <- raster(files[2])#cargar imagen
extent(imageraster)
plot(imageraster)
rbrick <- imageraster  #asignar a la variable rbrick

#ciclo para compactar las 10 imagenes de la produccion primaria media
for (i in 2:length(files)) #
{ 
  imageraster <- raster(files[i])#cargar imagen
  # acumular las 10 imágenes en una misma variable raster
  rbrick <- addLayer(rbrick,imageraster)
}
class(rbrick)
names(rbrick)
rbrick
stack_brick <- stack(rbrick)
setwd("~/ensayos/EVI_resultados")

#calculo de la produccion primaria media (a partir de la media de 10 imagenes)
ppn_2018 <- calc(stack_brick, median)
plot(sum_2018, main= "produccion total anual promedio biomasa 2009-2018")
writeRaster(sum_2018, filename = "ppn_median.tif", format= "GTiff", overwrite = T)


# 3 CALCULO DE LA PRODUCCION PRIMARIA ACUMULADA Y MEDIANA POR DECADA

#Generar stack y brick de 23 imagenes por año
setwd("~/ensayos/EVI/2018/EVI")
files <- list.files(pattern='.tif') #extraer listado de archivos
files
imageraster <- raster(files[4])#cargar imagen
imageraster
plot(imageraster)
rbrick <- imageraster  #asignar a la variable rbrick

#ciclo para compactar 23 imagenes por año
for (i in 2:length(files)) 
{ 
  imageraster <- raster(files[i])#cargar imagen
  #extraer nombre del archivo
  # acumular las 23 imágenes en una misma variable raster
  rbrick <- addLayer(rbrick,imageraster)
}

#generacion de stack a partir del brick generado 
class(rbrick)
names(rbrick)
rbrick
stack_brick <- stack(rbrick)
summary(stack_brick)

#Calculo de produccion acumulada por año
max_2018 <- calc(stack_brick, sum)
writeRaster(sum_2018, filename = "EVI_sCV_max2018.tif","~/ensayos/EVI_resultados/EVI_sCV", format= "GTiff", overwrite = T)

#Produccion primaria acumulada mediana (2009-2018)
setwd("~/ensayos/EVI_resultados/EVI_sCV")
files <- list.files(pattern='.tif') #extraer listado de archivos
files
imageraster <- raster(files[4])#cargar imagen
extent(imageraster)
plot(imageraster)
rbrick <- imageraster  #asignar a la variable rbrick

#ciclo para compactar 10 iamgenes de la produccion primaria acumulada (2009-2018)
for (i in 2:length(files)) 
{ 
  imageraster <- raster(files[i])#cargar imagen
  # acumular las 10 imágenes en una misma variable raster
  rbrick <- addLayer(rbrick,imageraster)
}
class(rbrick)
names(rbrick)
rbrick
stack_brick <- stack(rbrick)
plot(stack_brick)

#Calculo final de produccion primaria acumulada mediana de 10 años (2009-2018)
mean_sCV <- calc(stack_brick, median)
plot(mean_sCV, main= "produccion primaria acumulada mediana 2009 -2018")
writeRaster(mean_sCV, filename = "~/ensayos/EVI_resultados/EVI_sCV_mean.tif", format= "GTiff", overwrite = T)
summary(mean_sCV)


# 4 CALCULO DE LA PRODUCCION PRIMARIA MAXIMA Y MEDIANA POR DECADA

#Generar stack y brick de 23 imagenes por año
setwd("~/ensayos/EVI/2018/EVI")
#definir listas para Banda EVI
inpath <-"~/ensayos/EVI/2018/EVI"
evi.list <- list.files(path = inpath, pattern = glob2rx("*tif"), recursive = T)
evi.list
imageraster <- raster(evi.list[1])#cargar imagen
plot(imageraster)
rbrick <- imageraster  #asignar a la variable rbrick

#ciclo para compactar 23 imagenes por año de la produccion primaria maxima por año (2009-2018)
for (i in 2:length(evi.list)) 
{ 
  imageraster <- raster(evi.list[i])#cargar imagen
  names(imageraster) <- substr(evi.list[i],1, 10)#extraer nombre del archivo
  # acumular las 23 imágenes en una misma variable raster
  rbrick <- addLayer(rbrick,imageraster)
}
class(rbrick)
names(rbrick)
summary(rbrick)
setwd("~/ensayos/EVI_resultados")
stack_brick <- stack(rbrick_reclass)

#Calculo de la produccion primaria acumulada por año (2009-2018)
max_2019 <- calc(stack_brick, max)
plot(max_2018, main= "max reclasificado EVI 2018")
writeRaster(max_2008, filename = "maxtotal_2018.tif", format= "GTiff", overwrite = T)

#Produccion primaria acumulada mediana (2009-2018)
setwd("~/ensayos/EVI_resultados")
files <- list.files(pattern='.tif') #extraer listado de archivos
files
imageraster <- raster(files[4])#cargar imagen
extent(imageraster)
plot(imageraster)
rbrick <- imageraster  #asignar a la variable rbrick

#ciclo para compactar 10 iamgenes de la produccion primaria maxima (2009-2018)
for (i in 2:length(files)) 
{ 
  imageraster <- raster(files[i])#cargar imagen
  # acumular las 10 imágenes en una misma variable raster
  rbrick <- addLayer(rbrick,imageraster)
}
class(rbrick)
names(rbrick)
rbrick
stack_brick <- stack(rbrick)
plot(stack_brick)

#Calculo final de produccion primaria maxima mediana de 10 años (2009-2018)
max_median <- calc(stack_brick, median)
plot(max_median, main= "produccion primaria maxima mediana 2009 -2018")
writeRaster(max_median, filename = "~/ensayos/EVI_resultados/EVI_sCV_mean.tif", format= "GTiff", overwrite = T)
summary(max_median)