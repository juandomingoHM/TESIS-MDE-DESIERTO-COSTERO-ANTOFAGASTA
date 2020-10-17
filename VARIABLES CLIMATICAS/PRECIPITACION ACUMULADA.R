#TRABAJO DE TITULO: "DISTRIBICION POTENCIAL DE ESPECIES ENDEMICAS CON PROBLEMAS DE CONSERVACION EN EL DESIERTO COSTERO DE ANTOFAGASTA"
#NOMBRE: JUAN DOMINGO HUENUQUEO MARILLAN
#FECHA: 16/10/2020
#TEMA:  SCRIPT DE CALCULO DE PRECIPITACION ACUMULADA 2009-2018

#Librerias principales
library(sp)
library(raster)
library(rgdal)
library(GISTools)
library(wrspathrow)
library(rgeos)

# 1 PRECIPITACION ACUMULADA DE 10 AÑOS (2009-2018)

#lista de Bandas por año de precipitación mensual 12 meses (2009-2018)
archivos <-list.files("~/ensayos/cr2/variables", full.names = T, pattern = glob2rx("*.tif")) #lista EVI
archivos[[2]]

#Bandas mensuales por cada año de precpitacion acumulada

#2018
ras12 <-raster(archivos [2], band=480)#precmean, (banda 480 = dic)
ras11 <-raster(archivos [2], band=479) #precmean, (banda 479 = nov)
ras10 <-raster(archivos [2], band=478) #precmean, (banda 478 = oct)
ras9 <-raster(archivos [2], band=477) #precmean, (banda 477 = sep)
ras8 <-raster(archivos [2], band=476) #precmean, (banda 476 = ago)
ras7 <-raster(archivos [2], band=475) #precmean, (banda 475 = jul)
ras6 <-raster(archivos [2], band=474) #precmean, (banda 474 = jun)
ras5 <-raster(archivos [2], band=473) #precmean, (banda 473 = may)
ras4 <-raster(archivos [2], band=472) #precmean, (banda 472 = abr)
ras3 <-raster(archivos [2], band=471) #precmean, (banda 471 = mar)
ras2 <-raster(archivos [2], band=470) #precmean, (banda 470 = feb)
ras <-raster(archivos [2], band=469) #precmean, (banda 469 = ene)

#2017
ras13 <-raster(archivos [2], band=468)#precmean, (banda 480 = dic)
ras14 <-raster(archivos [2], band=467) #precmean, (banda 479 = nov)
ras15 <-raster(archivos [2], band=466) #precmean, (banda 478 = oct)
ras16<-raster(archivos [2], band=465) #precmean, (banda 477 = sep)
ras17 <-raster(archivos [2], band=464) #precmean, (banda 476 = ago)
ras18 <-raster(archivos [2], band=463) #precmean, (banda 475 = jul)
ras19 <-raster(archivos [2], band=462) #precmean, (banda 474 = jun)
ras20 <-raster(archivos [2], band=461) #precmean, (banda 473 = may)
ras21 <-raster(archivos [2], band=460) #precmean, (banda 472 = abr)
ras22 <-raster(archivos [2], band=459) #precmean, (banda 471 = mar)
ras23 <-raster(archivos [2], band=458) #precmean, (banda 470 = feb)
ras24 <-raster(archivos [2], band=457) #precmean, (banda 469 = ene)

#2016
ras25 <-raster(archivos [2], band=456)#precmean, (banda 480 = dic)
ras26 <-raster(archivos [2], band=455) #precmean, (banda 479 = nov)
ras27 <-raster(archivos [2], band=454) #precmean, (banda 478 = oct)
ras28 <-raster(archivos [2], band=453) #precmean, (banda 477 = sep)
ras29 <-raster(archivos [2], band=452) #precmean, (banda 476 = ago)
ras30 <-raster(archivos [2], band=451) #precmean, (banda 475 = jul)
ras31 <-raster(archivos [2], band=450) #precmean, (banda 474 = jun)
ras32 <-raster(archivos [2], band=449) #precmean, (banda 473 = may)
ras33 <-raster(archivos [2], band=448) #precmean, (banda 472 = abr)
ras34 <-raster(archivos [2], band=447) #precmean, (banda 471 = mar)
ras35 <-raster(archivos [2], band=446) #precmean, (banda 470 = feb)
ras36 <-raster(archivos [2], band=445) #precmean, (banda 469 = ene)

#2015
ras37 <-raster(archivos [2], band=444)#precmean, (banda 480 = dic)
ras38 <-raster(archivos [2], band=443) #precmean, (banda 479 = nov)
ras39 <-raster(archivos [2], band=442) #precmean, (banda 478 = oct)
ras40 <-raster(archivos [2], band=441) #precmean, (banda 477 = sep)
ras41 <-raster(archivos [2], band=440) #precmean, (banda 476 = ago)
ras42 <-raster(archivos [2], band=439) #precmean, (banda 475 = jul)
ras43 <-raster(archivos [2], band=438) #precmean, (banda 474 = jun)
ras44 <-raster(archivos [2], band=437) #precmean, (banda 473 = may)
ras45 <-raster(archivos [2], band=436) #precmean, (banda 472 = abr)
ras46 <-raster(archivos [2], band=435) #precmean, (banda 471 = mar)
ras47 <-raster(archivos [2], band=434) #precmean, (banda 470 = feb)
ras48 <-raster(archivos [2], band=433) #precmean, (banda 469 = ene)

#2014
ras49 <-raster(archivos [2], band=432)#precmean, (banda 480 = dic)
ras50 <-raster(archivos [2], band=431) #precmean, (banda 479 = nov)
ras51 <-raster(archivos [2], band=430) #precmean, (banda 478 = oct)
ras52 <-raster(archivos [2], band=429) #precmean, (banda 477 = sep)
ras53 <-raster(archivos [2], band=428) #precmean, (banda 476 = ago)
ras54 <-raster(archivos [2], band=427) #precmean, (banda 475 = jul)
ras55 <-raster(archivos [2], band=426) #precmean, (banda 474 = jun)
ras56 <-raster(archivos [2], band=425) #precmean, (banda 473 = may)
ras57 <-raster(archivos [2], band=424) #precmean, (banda 472 = abr)
ras58 <-raster(archivos [2], band=423) #precmean, (banda 471 = mar)
ras59 <-raster(archivos [2], band=422) #precmean, (banda 470 = feb)
ras60 <-raster(archivos [2], band=421) #precmean, (banda 469 = ene)

#2013
ras61 <-raster(archivos [2], band=420)#precmean, (banda 480 = dic)
ras62 <-raster(archivos [2], band=419) #precmean, (banda 479 = nov)
ras63 <-raster(archivos [2], band=418) #precmean, (banda 478 = oct)
ras64 <-raster(archivos [2], band=417) #precmean, (banda 477 = sep)
ras65 <-raster(archivos [2], band=416) #precmean, (banda 476 = ago)
ras66 <-raster(archivos [2], band=415) #precmean, (banda 475 = jul)
ras67 <-raster(archivos [2], band=414) #precmean, (banda 474 = jun)
ras68 <-raster(archivos [2], band=413) #precmean, (banda 473 = may)
ras69 <-raster(archivos [2], band=412) #precmean, (banda 472 = abr)
ras70 <-raster(archivos [2], band=411) #precmean, (banda 471 = mar)
ras71 <-raster(archivos [2], band=410) #precmean, (banda 470 = feb)
ras72 <-raster(archivos [2], band=409) #precmean, (banda 469 = ene)

#2012
ras73 <-raster(archivos [2], band=408)#precmean, (banda 480 = dic)
ras74 <-raster(archivos [2], band=407) #precmean, (banda 479 = nov)
ras75 <-raster(archivos [2], band=406) #precmean, (banda 478 = oct)
ras76 <-raster(archivos [2], band=405) #precmean, (banda 477 = sep)
ras77 <-raster(archivos [2], band=404) #precmean, (banda 476 = ago)
ras78 <-raster(archivos [2], band=403) #precmean, (banda 475 = jul)
ras79 <-raster(archivos [2], band=402) #precmean, (banda 474 = jun)
ras80<-raster(archivos [2], band=401) #precmean, (banda 473 = may)
ras81 <-raster(archivos [2], band=400) #precmean, (banda 472 = abr)
ras82 <-raster(archivos [2], band=399) #precmean, (banda 471 = mar)
ras83 <-raster(archivos [2], band=398) #precmean, (banda 470 = feb)
ras84 <-raster(archivos [2], band=397) #precmean, (banda 469 = ene)

#2011
ras85 <-raster(archivos [2], band=396)#precmean, (banda 480 = dic)
ras86 <-raster(archivos [2], band=395) #precmean, (banda 479 = nov)
ras87 <-raster(archivos [2], band=394) #precmean, (banda 478 = oct)
ras88 <-raster(archivos [2], band=393) #precmean, (banda 477 = sep)
ras89 <-raster(archivos [2], band=392) #precmean, (banda 476 = ago)
ras90 <-raster(archivos [2], band=391) #precmean, (banda 475 = jul)
ras91 <-raster(archivos [2], band=390) #precmean, (banda 474 = jun)
ras92 <-raster(archivos [2], band=389) #precmean, (banda 473 = may)
ras93 <-raster(archivos [2], band=388) #precmean, (banda 472 = abr)
ras94 <-raster(archivos [2], band=387) #precmean, (banda 471 = mar)
ras95 <-raster(archivos [2], band=386) #precmean, (banda 470 = feb)
ras96 <-raster(archivos [2], band=385) #precmean, (banda 469 = ene)

#2010
ras97 <-raster(archivos [2], band=384)#precmean, (banda 480 = dic)
ras98 <-raster(archivos [2], band=383) #precmean, (banda 479 = nov)
ras99 <-raster(archivos [2], band=382) #precmean, (banda 478 = oct)
ras100 <-raster(archivos [2], band=381) #precmean, (banda 477 = sep)
ras101 <-raster(archivos [2], band=380) #precmean, (banda 476 = ago)
ras102 <-raster(archivos [2], band=379) #precmean, (banda 475 = jul)
ras103 <-raster(archivos [2], band=378) #precmean, (banda 474 = jun)
ras104 <-raster(archivos [2], band=377) #precmean, (banda 473 = may)
ras105 <-raster(archivos [2], band=376) #precmean, (banda 472 = abr)
ras106<-raster(archivos [2], band=375) #precmean, (banda 471 = mar)
ras107<-raster(archivos [2], band=374) #precmean, (banda 470 = feb)
ras108 <-raster(archivos [2], band=373) #precmean, (banda 469 = ene)

#2009
ras109 <-raster(archivos [2], band=372)#precmean, (banda 480 = dic)
ras110 <-raster(archivos [2], band=371) #precmean, (banda 479 = nov)
ras111 <-raster(archivos [2], band=370) #precmean, (banda 478 = oct)
ras112 <-raster(archivos [2], band=369) #precmean, (banda 477 = sep)
ras113 <-raster(archivos [2], band=368) #precmean, (banda 476 = ago)
ras114 <-raster(archivos [2], band=367) #precmean, (banda 475 = jul)
ras115 <-raster(archivos [2], band=366) #precmean, (banda 474 = jun)
ras116 <-raster(archivos [2], band=365) #precmean, (banda 473 = may)
ras117 <-raster(archivos [2], band=364) #precmean, (banda 472 = abr)
ras118 <-raster(archivos [2], band=363) #precmean, (banda 471 = mar)
ras119<-raster(archivos [2], band=362) #precmean, (banda 470 = feb)
ras120 <-raster(archivos [2], band=361) #precmean, (banda 469 = ene)

#agregar proyeccion espacial
proj4string(precmean.10años) <- projection(raster())
projection(precmean.10años)
plot(precmean.10años)

#brick de precipitacion acumulada anual (2009-2018)
preac.acum.2009 <-brick(ras, ras2, ras3, ras4, ras5, ras6, ras7, ras8, ras9, ras10, ras11, ras12)
preac.acum.2010 <-brick(ras13, ras14,ras15, ras16, ras17, ras18, ras19, ras20, ras21, ras22, ras23, ras24)
preac.acum.2011 <-brick(ras25, ras26, ras27, ras28, ras29, ras30, ras31, ras32, ras33, ras34, ras35, ras36)
preac.acum.2012 <-brick(ras37, ras38, ras39, ras40,ras41, ras42, ras43, ras44, ras45, ras46, ras47, ras48)
preac.acum.2013 <-brick(ras49, ras50, ras51, ras52, ras53, ras54, ras55, ras56, ras57, ras58, ras59, ras60)
preac.acum.2014 <-brick(ras61, ras62, ras63, ras64, ras65, ras66, ras67, ras68, ras69, ras70, ras71, ras72)
preac.acum.2015 <-brick(ras73, ras74, ras75, ras76, ras77, ras78, ras79,ras80, ras81, ras82, ras83, ras84)
preac.acum.2016 <-brick(ras85, ras86, ras87, ras88, ras89, ras90, ras91, ras92, ras93, ras94, ras95, ras96)
preac.acum.2017 <-brick(ras97, ras98, ras99, ras100, ras101, ras102, ras103, ras104, ras105, ras106, ras107, ras108)
preac.acum.2018 <-brick(ras109, ras110, ras111, ras112, ras113, ras114, ras115, ras116, ras117, ras118, ras119, ras120)

#reproyeccion espacial por año (2009-2018)
proj4string(preac.acum.2009) <- projection(raster())
proj4string(preac.acum.2010) <- projection(raster())
proj4string(preac.acum.2011) <- projection(raster())
proj4string(preac.acum.2012) <- projection(raster())
proj4string(preac.acum.2013) <- projection(raster())
proj4string(preac.acum.2014) <- projection(raster())
proj4string(preac.acum.2015) <- projection(raster())
proj4string(preac.acum.2016) <- projection(raster())
proj4string(preac.acum.2017) <- projection(raster())
proj4string(preac.acum.2018) <- projection(raster())

projection(preac.acum.2009)
plot(preac.acum.2009)

#stack precipitacion acumulada anual 
setwd("~/ensayos/cr2/prec_acumulada")
stack_brick <- stack(preac.acum.2018)
mean_prec <- calc(stack_brick, sum)
plot(mean_prec)
writeRaster(mean_prec, filename = "preac_sum2018.tif", format= "GTiff", overwrite = T)

#precipitacion acumulada 2009 - 2018 FINAL
preac.acumulada <-list.files("~/ensayos/cr2/prec_acumulada", full.names = T, pattern = glob2rx("*.tif")) #lista EVI
preac.acumulada
stack_brick2 <- stack(preac.acumulada)
mean_prec2 <- calc(stack_brick2, mean)
plot(mean_prec2)
writeRaster(mean_prec2, filename = "preac_sum.tif", format= "GTiff", overwrite = T)


