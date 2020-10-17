#TRABAJO DE TITULO: "DISTRIBICION POTENCIAL DE ESPECIES ENDEMICAS CON PROBLEMAS DE CONSERVACION EN EL DESIERTO COSTERO DE ANTOFAGASTA"
#NOMBRE: JUAN DOMINGO HUENUQUEO MARILLAN
#FECHA: 15/10/2020
#TEMA: ESTANDARIZACIÓN DE VARIABLES CON DIFERENTES EXTENSIÓN, RESOLUCIÓN, FORMATO DE ORIGEN Y APLICACION DE MASCARAS

#Librerias
library(raster) 
library(rgeos) 
library(rgdal) 

#ESTANDARIZACION DE VARIABLES PARTE III: CELDAS NULAS Y MASCARAS DE URBES-AGRICULTURA


# 11 APLICACION DE MASCARAS DE URBES Y ZONAS AGRICOLAS

#lista de variables estandarizadas
lista.variables <- list.files(path="~/ensayos/variables_crop",pattern='*.asc', full.names=TRUE)
lista.variables

#crear stack para la lista de variables estandarizadas
variables <- stack(lista.variables)
names(variables)
variables
plot(variables[[1]])

#capa vectorial de zonas urbanas
urbes <- readOGR(dsn=path.expand("~/DATOS/archivos/shapes/TESIS_MDE/cuad_mask"),
                 layer="cuad_mask")
urbes <- spTransform(urbes,"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") #reproyectar la capa vectorial con la proyeccion de las variables

plot(urbes)
projection(urbes) #consulta de proyeccion

#aplicacion de mascaras de urbes a las variables estandarizadas
variables<-mask(variables, urbes)
plot(variables[[1]])

#capa vectorial de zonas agricolas
agro <- readOGR(dsn=path.expand("~/DATOS/archivos/shapes/TESIS_MDE/agro_mask_cuad"),
                layer="agro_mask")
agro <- spTransform(agro,"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") #reproyectar la capa vectorial con la proyeccion de las variables

plot(agro)
projection(agro) #consulta de proyeccion 

#aplicacion de mascaras de zonas agricolas a las variables estandarizadas
variables<-mask(variables, agro)
plot(variables[[3]])
proj4string(variables) <- projection(raster())

# 12 GUARDAR VARIABLES ESTANDARIZADAS Y CON MASCARAS APLICADAS 

writeRaster(variables[["a.acum"]], filename="~/ensayos/variables/a.acum.asc", format="ascii", overwrite=TRUE) #1
writeRaster(variables[["aspec"]], filename="~/ensayos/variables/aspec.asc", format="ascii", overwrite=TRUE) #2
writeRaster(variables[["curv"]], filename="~/ensayos/variables/curv.asc", format="ascii", overwrite=TRUE) #3
writeRaster(variables[["dem30"]], filename="~/ensayos/variables/dem30.asc", format="ascii", overwrite=TRUE) #4
writeRaster(variables[["dir.dren"]], filename="~/ensayos/variables/dir.dren.asc", format="ascii", overwrite=TRUE) #5
writeRaster(variables[["est.median"]], filename="~/ensayos/variables/est.median.asc", format="ascii", overwrite=TRUE) #6
writeRaster(variables[["etp"]], filename="~/ensayos/variables/etp.asc", format="ascii", overwrite=TRUE) #7
writeRaster(variables[["evi.max"]], filename="~/ensayos/variables/evi.max.asc", format="ascii", overwrite=TRUE) #8
writeRaster(variables[["hills"]], filename="~/ensayos/variables/hills.asc", format="ascii", overwrite=TRUE) #9
writeRaster(variables[["hume"]], filename="~/ensayos/variables/hume.asc", format="ascii", overwrite=TRUE) #10
writeRaster(variables[["ihp"]], filename="~/ensayos/variables/ihp.asc", format="ascii", overwrite=TRUE) #11
writeRaster(variables[["ind.ari"]], filename="~/ensayos/variables/ind.ari.asc", format="ascii", overwrite=TRUE) #12
writeRaster(variables[["ipt"]], filename="~/ensayos/variables/ipt.asc", format="ascii", overwrite=TRUE) #13
writeRaster(variables[["nubo"]], filename="~/ensayos/variables/nubo.asc", format="ascii", overwrite=TRUE) #14
writeRaster(variables[["orient"]], filename="~/ensayos/variables/orient.asc", format="ascii", overwrite=TRUE) #15
writeRaster(variables[["pend"]], filename="~/ensayos/variables/pend.asc", format="ascii", overwrite=TRUE) #16
writeRaster(variables[["pp.acum"]], filename="~/ensayos/variables/pp.acum.asc", format="ascii", overwrite=TRUE) #17
writeRaster(variables[["pp.mean"]], filename="~/ensayos/variables/pp.mean.asc", format="ascii", overwrite=TRUE) #18
writeRaster(variables[["ppn.median"]], filename="~/ensayos/variables/ppn.median.asc", format="ascii", overwrite=TRUE) #19
writeRaster(variables[["relief"]], filename="~/ensayos/variables/relief.asc", format="ascii", overwrite=TRUE) #20
writeRaster(variables[["tan_curv"]], filename="~/ensayos/variables/tan_curv.asc", format="ascii", overwrite=TRUE) #21
writeRaster(variables[["temp.max"]], filename="~/ensayos/variables/temp.max.asc", format="ascii", overwrite=TRUE) #22
writeRaster(variables[["pp.mean"]], filename="~/ensayos/variables/pp.mean.asc", format="ascii", overwrite=TRUE) #23
writeRaster(variables[["temp.min"]], filename="~/ensayos/variables/temp.min.asc", format="ascii", overwrite=TRUE) #24
writeRaster(variables[["temp.mean"]], filename="~/ensayos/variables/temp.mean.asc", format="ascii", overwrite=TRUE) #27
writeRaster(variables[["tri"]], filename="~/ensayos/variables/tri.asc", format="ascii", overwrite=TRUE) #25
writeRaster(variables[["dir.pend.ew"]], filename="~/ensayos/variables/dir.pend.ew.asc", format="ascii", overwrite=TRUE) #26
writeRaster(variables[["dir.pend.ns"]], filename="~/ensayos/variables/dir.pend.ns.asc", format="ascii", overwrite=TRUE) #27
