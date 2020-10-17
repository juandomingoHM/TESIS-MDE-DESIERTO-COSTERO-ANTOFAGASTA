#TRABAJO DE TITULO: "DISTRIBICION POTENCIAL DE ESPECIES ENDEMICAS CON PROBLEMAS DE CONSERVACION EN EL DESIERTO COSTERO DE ANTOFAGASTA"
#NOMBRE: JUAN DOMINGO HUENUQUEO MARILLAN
#FECHA: 15/10/2020
#TEMA: ESTANDARIZACIÓN DE VARIABLES CON DIFERENTES EXTENSIÓN, RESOLUCIÓN, FORMATO DE ORIGEN Y APLICACION DE MASCARAS


#ESTANDARIZACION DE VARIABLES I: ESTANDARIZACION DE EXTENSION, RESOLUCION Y FORMATO

#Librerias
library(raster) 
library(rgeos) 
library(rgdal) 

# 1. IMPORTACIÓN DE VARIABLES 

#Variables relacionadas con la geometrían de terreno
dem30<-raster("~/ensayos/SRTM/mosaico.tif") #altitud (modelo de elevacion digital) 1
pend<-raster("~/ensayos/SRTM/pendiente.tif") #pendiente de terreno 2
orient<-raster("~/ensayos/SRTM/orientacion2.tif") #orientacion de terreno 3
relief<-raster("~/ensayos/SRTM/relieve.tif") #relieve de terreno 4
ipt<-raster("~/ensayos/SRTM/IPT.tif") #indice posicion topografica 5
hills<-raster("~/ensayos/SRTM/hillshade.tif") #hillshade 6
tri<-raster("~/ensayos/SRTM/TRI.tif") #indice rugosidad de terreno 7
dir.pend.ns<-raster("~/ensayos/SRTM/porcentual/FirstOrderPartialDerivative_NS.tif") #direccion pendiente norte sur 8
dir.pend.ew<-raster("~/ensayos/SRTM/porcentual/FirstOrderPartialDerivative_EW.tif") #direccion pendiente este oeste 9
tan_curv<-raster("~/ensayos/SRTM/porcentual/tangential_curvature.tif") #curvatura tangencial 10
curv<-raster("~/ensayos/SRTM/porcentual/profile_curvature.tif") #curvatura 11
aspec<-raster("~/ensayos/SRTM/porcentual/aspect.tif")#aspecto 12

#variables relacionadas con la geometría hidrográfica
ihp<-raster("~/ensayos/SRTM/hidro/IPH_ind_topo_hume.tif") #indice humedad topografica 13
dir.dren<-raster("~/ensayos/SRTM/hidro/direccion_drenaje.tif") #direccion drenaje 14
a.acum<-raster("~/ensayos/SRTM/hidro/area_acumulada.tif") #area acumulada 15

#Variables relacionadas con modelos climáticos
hume<-raster("~/ensayos/cr2/cr2mma_hum_relav.tif") #humedad relativa 16
etp<-raster("~/ensayos/cr2/evapo/ETP.tif") #evapotranspiracion potencial 17
temp.max<-raster("~/ensayos/cr2/evapo/temp_max.tif") #temperatura maxima 18
temp.min<-raster("~/ensayos/cr2/evapo/temp_min.tif") #temperatura minima 19
ind.ari<-raster("~/ensayos/cr2/prec_acumulada/ia_martonne.tif") #indice aridez martonne 1926 20
pp.acum<-raster("~/ensayos/cr2/prec_acumulada/preac_sum.tif") #precipitacion acumulada 21
pp.mean<-raster("~/ensayos/cr2/variables/preac_mean10.tif") #precipitacion media anual 22
temp.mean<-raster("~/ensayos/cr2/variables/temp_mean10.tif") #temperatura media 23
nubo<-raster("~/ensayos/nubo_resultados/nubo_mean.tif") #cobertura nubosidad 24

#Variable relacionadas con la ecología (a partir de la mediana de 10 años)
evi.max<-raster("~/MODIS/EVI_Mmax_mean.tif") #produccion primaria maxima 25
est.median<-raster("~/MODIS/EVI_sCV_median.tif") #produccion primaria acumulada 26
ppn.median<-raster("~/MODIS/ppn_median.tif") #produccion primaria media 27 

#Variable extraida de BIOCLIM como referencia para estandarizar a las variables segun su extension y resolucion
modelo<- raster("~/ensayos/modelo/modelo.asc") #capa base para modelar 28,

# 2. REVISION INVIDIDUAL DE PROJECCION DE CADA VARIABLE

projection(a.acum)#1
projection(aspec)#2
projection(curv)#3
projection(dem30)#4
projection(dir.dren)#5
projection(est.median)#6
projection(etp)#7
projection(evi.max)#8
projection(hills)#9
projection(hume)#10
projection(ihp)#11
projection(ind.ari)#12
projection(ipt)#13
projection(modelo)#14 sin proj
projection(nubo)#15
projection(orient)#16
projection(pend)#17
projection(pp.acum)#18
projection(pp.mean)#19
projection(ppn.median)#20
projection(relief)#21
projection(tan_curv)#22
projection(temp.max)#23 sin proj
projection(temp.mean)#24
projection(temp.min)#25 sin proj
projection(tri)#26
projection(dir.pend.ew)#27
projection(dir.pend.ns)#28

# 3. REVISION INVIDIDUAL DE EXTENSION DE CADA VARIABLE

extent(a.acum)#1
extent(aspec)#2
extent(curv)#3
extent(dem30)#4
extent(dir.dren)#5
extent(est.median)#6
extent(etp)#7
extent(evi.max)#8
extent(hills)#9
extent(hume)#10
extent(ihp)#11
extent(ind.ari)#12
extent(ipt)#13
extent(modelo)#14 sin proj
extent(nubo)#15
extent(orient)#16
extent(pend)#17
extent(pp.acum)#18
extent(pp.mean)#19
extent(ppn.median)#20
extent(relief)#21
extent(tan_curv)#22
extent(temp.max)#23 sin proj
extent(temp.mean)#24
extent(temp.min)#25 sin proj
extent(tri)#26
extent(dir.pend.ew)#27
extent(dir.pend.ns)#28)

# 4. IGUALAMOS EXTENSION Y RESOLUCION POR CADA VARIABLE

#Se iguala la resolucion y extension de acuerdo a la variable de modelo de BIOCLIM con metodo "bilinear"
a.acum<-resample(x=a.acum, y=modelo, method="bilinear") #1
aspec<-resample(x=aspec, y=modelo, method="bilinear") #2
curv<-resample(x=curv, y=modelo, method="bilinear") #3
dem30<-resample(x=dem30, y=modelo, method="bilinear") #4
dir.dren<-resample(x=dir.dren, y=modelo, method="bilinear") #5
est.median<-resample(x=est.median, y=modelo, method="bilinear") #6
etp<-resample(x=etp, y=modelo, method="bilinear") #7
evi.max<-resample(x=evi.max, y=modelo, method="bilinear") #8
hills<-resample(x=hills, y=modelo, method="bilinear") #9
hume<-resample(x=hume, y=modelo, method="bilinear") #10
ihp<-resample(x=ihp, y=modelo, method="bilinear") #11
ind.ari<-resample(x=ind.ari, y=modelo, method="bilinear") #12
ipt<-resample(x=ipt, y=modelo, method="bilinear") #13
#modelo<-resample(x=ipt, y=modelo, method="bilinear") #14 variable de modelo
nubo<-resample(x=nubo, y=modelo, method="bilinear") #15 
orient<-resample(x=orient, y=modelo, method="bilinear") #16
pend<-resample(x=pend, y=modelo, method="bilinear") #17
pp.acum<-resample(x=pp.acum, y=modelo, method="bilinear") #18
pp.mean<-resample(x=pp.mean, y=modelo, method="bilinear") #19
ppn.median<-resample(x=ppn.median, y=modelo, method="bilinear") #20
relief<-resample(x=relief, y=modelo, method="bilinear") #21
tan_curv<-resample(x=tan_curv, y=modelo, method="bilinear") #22
temp.max<-resample(x=temp.max, y=modelo, method="bilinear") #23
temp.mean<-resample(x=temp.mean, y=modelo, method="bilinear") #24
temp.min<-resample(x=temp.min, y=modelo, method="bilinear") #25
tri<-resample(x=tri, y=modelo, method="bilinear") #26
dir.pend.ew<-resample(x=dir.pend.ew, y=modelo, method="bilinear") #27
dir.pend.ns<-resample(x=dir.pend.ns, y=modelo, method="bilinear") #28

# 5. REVISION DE EXTENSION Y RESOLUCION LUEGO DE ESTANDARIZAR LAS VARIABLES

extent(a.acum)#revision de extension de una variable cualquiera
xres(a.acum)#reviision de resolucion de una variable cualquiera

# 6. GUARDAR VARIABLES ESTANDARIZADAS EN FORMATO ASCII

writeRaster(a.acum, filename="~/ensayos/variables_asc/a.acum.asc", format="ascii", overwrite=TRUE) #area acumulada
writeRaster(aspec, filename="~/ensayos/variables_asc/aspect.asc", format="ascii", overwrite=TRUE) #aspecto de terreno
writeRaster(curv, filename="~/ensayos/variables_asc/curv.asc", format="ascii", overwrite=TRUE) #curvatura de terreno
writeRaster(dem30, filename="~/ensayos/variables_asc/dem30.asc", format="ascii", overwrite=TRUE) #altitud (modelo de elevacion digital)
writeRaster(dir.dren, filename="~/ensayos/variables_asc/dir.dren.asc", format="ascii", overwrite=TRUE) #direccion de drenaje
writeRaster(est.median, filename="~/ensayos/variables_asc/est.median.asc", format="ascii", overwrite=TRUE) #produccion primaria acumulada
writeRaster(etp, filename="~/ensayos/variables_asc/etp.asc", format="ascii", overwrite=TRUE) #evapotranspiracion
writeRaster(evi.max, filename="~/ensayos/variables_asc/evi.max.asc", format="ascii", overwrite=TRUE) #produccion primaria maxima
writeRaster(hills, filename="~/ensayos/variables_asc/hills.asc", format="ascii", overwrite=TRUE) #hillshade
writeRaster(hume, filename="~/ensayos/variables_asc/hume.asc", format="ascii", overwrite=TRUE) #humedad relativa
writeRaster(ihp, filename="~/ensayos/variables_asc/ihp.asc", format="ascii", overwrite=TRUE) #indice humedad topografica
writeRaster(ind.ari, filename="~/ensayos/variables_asc/ind.ari.asc", format="ascii", overwrite=TRUE) #indice de aridez
writeRaster(ipt, filename="~/ensayos/variables_asc/ipt.asc", format="ascii", overwrite=TRUE) #indice de posicion topografica
writeRaster(nubo, filename="~/ensayos/variables_asc/nubo.asc", format="ascii", overwrite=TRUE) #cobertura nubosidad
writeRaster(orient, filename="~/ensayos/variables_asc/orient.asc", format="ascii", overwrite=TRUE) #orientacion de relieve
writeRaster(pend, filename="~/ensayos/variables_asc/pend.asc", format="ascii", overwrite=TRUE) #pendiente de terreno
writeRaster(pp.acum, filename="~/ensayos/variables_asc/pp.acum.asc", format="ascii", overwrite=TRUE) #precipitacion acumulada media
writeRaster(pp.mean, filename="~/ensayos/variables_asc/pp.mean.asc", format="ascii", overwrite=TRUE) #precipitacion media
writeRaster(ppn.median, filename="~/ensayos/variables_asc/ppn.median.asc", format="ascii", overwrite=TRUE) #produccion primaria media
writeRaster(relief, filename="~/ensayos/variables_asc/relief.asc", format="ascii", overwrite=TRUE) #relieve
writeRaster(tan_curv, filename="~/ensayos/variables_asc/tan_curv.asc", format="ascii", overwrite=TRUE) #curvatura tangencial de terreno
writeRaster(temp.max, filename="~/ensayos/variables_asc/temp.max.asc", format="ascii", overwrite=TRUE) #temperatura media
writeRaster(temp.min, filename="~/ensayos/variables_asc/temp.min.asc", format="ascii", overwrite=TRUE) #temperatura minima media
writeRaster(temp.mean, filename="~/ensayos/variables_asc/temp.mean.asc", format="ascii", overwrite=TRUE) #temperatura minima media
writeRaster(tri, filename="~/ensayos/variables_asc/tri.asc", format="ascii", overwrite=TRUE) #indice de rugosidad topografica
writeRaster(dir.pend.ew, filename="~/ensayos/variables_asc/dir.pend.ew.asc", format="ascii", overwrite=TRUE) #direccion de pendiente este-oeste
writeRaster(dir.pend.ns, filename="~/ensayos/variables_asc/dir.pend.ns.asc", format="ascii", overwrite=TRUE) #direccion de pendiente norte-sur