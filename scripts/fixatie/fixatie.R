## Peilfixatie model Valthermond

setwd("/home/merijn/Documents/Nijmegen_Stu_As/Project_Valthermond/Scenariostudie/R")

require(rgdal)
require(rgeos)
require(raster)


source("scripts/fixatie/functie_co2_emissies_peilfixatie.R")
source("scripts/fixatie/functie_co2_emissies.R")
source("scripts/fixatie/functie_co2_som_per_interval.R")
source("scripts/fixatie/functie_veendikte.R")

## Laad bestanden

peilgebieden <- readOGR("source/peilgebieden_valthermond","peilgebieden_valthermond")
veendikteraster <- raster("source/veendikte.tif")
glg <- raster("source/glg.tif")
crs(glg) <- proj4string(veendikteraster)

## Voorbereiding

somrasterlist <- list()
eindrasterlist <- list()

glg_resample <- resample(glg,veendikteraster)
glg_crop <- crop(glg_resample,veendikteraster)
glg_mask <- mask(glg_crop, veendikteraster)

peilgebieden$co2som <- ''
peilgebieden$co2eind <- ''

## Geef nulwaarden voor waarden lager dan nul

glg_mask[glg_mask < 0] <- 0
veendikteraster[veendikteraster < 0] <- 0

for(j in 1:length(peilgebieden[,])){

## Neem veendikte per peilvak

veendiktepeilvak <- crop(veendikteraster,peilgebieden[j,])
veendiktepeilvak <- mask(veendiktepeilvak,peilgebieden[j,])
glgpeilvak <- crop(glg_mask,peilgebieden[j,])
glgpeilvak <- mask(glgpeilvak,peilgebieden[j,])

## Bereken co2 emissies per cell

co2eindraster <- veendiktepeilvak
co2eindraster[!is.na(co2eindraster)][] <- 1
co2eindstapraster <- co2eindraster
co2somraster <- co2eindraster * 30

for (i in 1:length(veendiktepeilvak)){
  if(!is.na(veendiktepeilvak[i]) & !is.na(glgpeilvak[i])){
  tabel <- co2_emissies_peilfixatie(glgpeilvak[i],veendiktepeilvak[i])
  co2eind <- tabel[1,2]
  co2eindraster[i] <- co2eind
  co2eindstap <- tabel[3,2]
  co2eindstapraster[i] <- co2eindstap
  co2som <- tabel[2,2]
  co2somraster[i] <- co2som
  }}

somrasterlist[[j]] <- co2somraster  
eindrasterlist[[j]] <- co2eindraster

## Bereken gemiddelden

co2som <- cellStats(somrasterlist[[j]], stat='mean', na.rm=TRUE)
co2eind <- cellStats(eindrasterlist[[j]], stat='mean', na.rm=TRUE)
co2eindstap <- cellStats(co2eindstapraster, stat = 'mean',na.rm=TRUE)

peilgebieden$co2som[j] <- co2som
peilgebieden$co2eind[j] <- co2eind
  
writeRaster(somrasterlist[[j]],paste("inter/somrasters/fixatie/somraster_fixatie_", peilgebieden$naam[j], ".tif", sep=''),datatype='FLT4S', overwrite=TRUE)
writeRaster(eindrasterlist[[j]],paste("inter/eindrasters/fixatie/eindraster_fixatie_", peilgebieden$naam[j],".tif", sep=''),datatype='FLT4S', overwrite=TRUE)

}

writeOGR(peilgebieden,dsn = "inter/shapes/fixatie", layer = "peilgebiedenemissiesfixatie", driver="ESRI Shapefile", overwrite_layer = TRUE)

## Combine rasters

somrastergroot <- do.call(merge, somrasterlist)
writeRaster(somrastergroot,"final/fixatie/fixatie_somrastergroot.tif",datatype='FLT4S', overwrite=TRUE)
eindrastergroot <- do.call(merge, eindrasterlist)
writeRaster(eindrastergroot,"final/fixatie/fixatie_eindrastergroot.tif",datatype='FLT4S', overwrite=TRUE)


