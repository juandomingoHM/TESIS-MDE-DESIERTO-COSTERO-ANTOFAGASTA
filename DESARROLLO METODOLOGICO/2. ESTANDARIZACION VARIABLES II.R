#TRABAJO DE TITULO: "DISTRIBICION POTENCIAL DE ESPECIES ENDEMICAS CON PROBLEMAS DE CONSERVACION EN EL DESIERTO COSTERO DE ANTOFAGASTA"
#NOMBRE: JUAN DOMINGO HUENUQUEO MARILLAN
#FECHA: 15/10/2020
#TEMA: ESTANDARIZACIÓN DE VARIABLES CON DIFERENTES EXTENSIÓN, RESOLUCIÓN, FORMATO DE ORIGEN Y APLICACION DE MASCARAS

#LIBRERÍAS
library(raster) 
library(rgeos) 
library(rgdal) 

#ESTANDARIZACION DE VARIABLES PARTE II: CELDAS NULAS Y MASCARAS DE CHILE CONTINENTAL

# 7. MAPA DE CELDAS NULAS (MASCARA) GENERADO A PARTIR DE LA MULTIPLICACION DE TODAS LAS VARIABLES

valores.nulos=a.acum*aspec*curv*dem30*dir.dren*est.median*etp*evi.max*hills*
  hume*ihp*ind.ari*ipt*nubo*orient*pend*pp.acum*pp.mean*ppn.median*relief*
  tan_curv*temp.max*temp.mean*temp.min*tri*dir.pend.ew*dir.pend.ns   #mapa con celdas nulas
par(mfrow=c(1,1))
plot(valores.nulos)


# 8. ELIMINACION DE CELDAS NULAS A LAS VARIABLES ESTANDARIZADAS

#Brick que contiene todas las variables (27)
variables.brick<-brick(a.acum,aspec,curv,dem30,dir.dren,est.median,etp,evi.max,hills,
                       hume,ihp,ind.ari,ipt,nubo,orient,pend,pp.acum,pp.mean,ppn.median,relief,
                       tan_curv,temp.max,temp.mean,temp.min,tri,dir.pend.ew,dir.pend.ns)

#Mantener los nombres originales de las variables estandarizadas
names(variables.brick)<-c("a.acum","aspec","curv","dem30","dir.dren","est.median","etp","evi.max","hills",
                          "hume","ihp","ind.ari","ipt,nubo","orient","pend","pp.acum","pp.mean","ppn.median","relief",
                          "tan_curv","temp.max","temp.mean","temp.min","tri","dir.pend.ew","dir.pend.ns")
plot(variables.brick)

#aplicación de máscara con celdas con valores nulos (NA) a las variables estandarizadas
variables.brick<-mask(variables.brick, valores.nulos)
plot(variables.brick)

# 9. APLICACION DE MASCARA DE CHILE CONTINENTAL

#Aplicacion de mascara de chile contienetal a las varaibles estandarizadas
Chile <- readOGR(dsn=path.expand("~/DATOS/archivos/shapes/extent_norte"),
               layer="extent_norte")
plot(Chile)
projection(Chile)
variables.brick<-mask(variables.brick, Chile)
plot(variables.brick)
plot(variables.brick[[1]])
plot(Chile, add=T)

#cortar cada capa de variables estadarizadas que estan contenidas en el brick
variables.brick<-crop(x=variables.brick, y=extent(dem30))
plot(variables.brick)


# 10. GUARDAR VARIABLES CON MASCARA APLICADA
#guardamos las variables preparadas al disco duro
writeRaster(variables.brick[["area_acumulada"]], filename="~/ensayos/variables_crop/a.acum.asc", format="ascii", overwrite=TRUE) #1
writeRaster(variables.brick[["aspect"]], filename="~/ensayos/variables_crop/aspec.asc", format="ascii", overwrite=TRUE) #2
writeRaster(variables.brick[["profile_curvature"]], filename="~/ensayos/variables_crop/curv.asc", format="ascii", overwrite=TRUE) #3
writeRaster(variables.brick[["mosaico"]], filename="~/ensayos/variables_crop/dem30.asc", format="ascii", overwrite=TRUE) #4
writeRaster(variables.brick[["direccion_drenaje"]], filename="~/ensayos/variables_crop/dir.dren.asc", format="ascii", overwrite=TRUE) #5
writeRaster(variables.brick[["EVI_sCV_median"]], filename="~/ensayos/variables_crop/est.median.asc", format="ascii", overwrite=TRUE) #6
writeRaster(variables.brick[["ETP"]], filename="~/ensayos/variables_crop/etp.asc", format="ascii", overwrite=TRUE) #7
writeRaster(variables.brick[["EVI_Mmax_mean"]], filename="~/ensayos/variables_crop/evi.max.asc", format="ascii", overwrite=TRUE) #8
writeRaster(variables.brick[["hillshade"]], filename="~/ensayos/variables_crop/hills.asc", format="ascii", overwrite=TRUE) #9
writeRaster(variables.brick[["cr2mma_hum_relav"]], filename="~/ensayos/variables_crop/hume.asc", format="ascii", overwrite=TRUE) #10
writeRaster(variables.brick[["IPH_ind_topo_hume"]], filename="~/ensayos/variables_crop/ihp.asc", format="ascii", overwrite=TRUE) #11
writeRaster(variables.brick[["ia_martonne"]], filename="~/ensayos/variables_crop/ind.ari.asc", format="ascii", overwrite=TRUE) #12
writeRaster(variables.brick[["IPT"]], filename="~/ensayos/variables_crop/ipt.asc", format="ascii", overwrite=TRUE) #13
writeRaster(variables.brick[["nubo_mean"]], filename="~/ensayos/variables_crop/nubo.asc", format="ascii", overwrite=TRUE) #14
writeRaster(variables.brick[["orientacion2"]], filename="~/ensayos/variables_crop/orient.asc", format="ascii", overwrite=TRUE) #15
writeRaster(variables.brick[["pendiente"]], filename="~/ensayos/variables_crop/pend.asc", format="ascii", overwrite=TRUE) #16
writeRaster(variables.brick[["preac_sum"]], filename="~/ensayos/variables_crop/pp.acum.asc", format="ascii", overwrite=TRUE) #17
writeRaster(variables.brick[["preac_mean10"]], filename="~/ensayos/variables_crop/pp.mean.asc", format="ascii", overwrite=TRUE) #18
writeRaster(variables.brick[["ppn_median"]], filename="~/ensayos/variables_crop/ppn.median.asc", format="ascii", overwrite=TRUE) #19
writeRaster(variables.brick[["relieve"]], filename="~/ensayos/variables_crop/relief.asc", format="ascii", overwrite=TRUE) #20
writeRaster(variables.brick[["tangential_curvature"]], filename="~/ensayos/variables_crop/tan_curv.asc", format="ascii", overwrite=TRUE) #21
writeRaster(variables.brick[["temp_max"]], filename="~/ensayos/variables_crop/temp.max.asc", format="ascii", overwrite=TRUE) #22
writeRaster(variables.brick[["preac_mean10"]], filename="~/ensayos/variables_crop/pp.mean.asc", format="ascii", overwrite=TRUE) #23
writeRaster(variables.brick[["temp_min"]], filename="~/ensayos/variables_crop/temp.min.asc", format="ascii", overwrite=TRUE) #24
writeRaster(variables.brick[["temp_mean10"]], filename="~/ensayos/variables_crop/temp.mean.asc", format="ascii", overwrite=TRUE) #27
writeRaster(variables.brick[["TRI"]], filename="~/ensayos/variables_crop/tri.asc", format="ascii", overwrite=TRUE) #25
writeRaster(variables.brick[["FirstOrderPartialDerivative_EW"]], filename="~/ensayos/variables_crop/dir.pend.ew.asc", format="ascii", overwrite=TRUE) #26
writeRaster(variables.brick[["FirstOrderPartialDerivative_NS"]], filename="~/ensayos/variables_crop/dir.pend.ns.asc", format="ascii", overwrite=TRUE) #27
