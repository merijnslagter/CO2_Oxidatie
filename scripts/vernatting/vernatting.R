setwd("/home/merijn/Documents/Nijmegen_Stu_As/Project_Valthermond/Scenariostudie/R")

# for(i in 1:B) sqrt(i)
# foreach(i = 1:B)  %do% sqrt(i)
# foreach(i = 1:B) %dopar% sqrt(i)

require(rgdal)
require(rgeos)
require(raster)
require(doMC)
require(doParallel)
require(foreach)

source("scripts/vernatting/functie_co2_emissies_vernatting.R")
source("scripts/vernatting/functie_co2_emissies.R")
source("scripts/vernatting/functie_co2_som_per_interval.R")
source("scripts/vernatting/functie_veendikte.R")


# laad peilgebieden, waarvan de waardes 999 en 0 verwijderd zijn.

peilgebieden <- readOGR("source/peilgebieden_valthermond","peilgebieden_valthermond")
veendikteraster <- raster("source/veendikte.tif")
glg <- raster("source/glg.tif")
crs(glg) <- proj4string(veendikteraster)

## PEILFIXATIE

somrasterlist <- list()
eindrasterlist <- list()
beginrasterlist <- list()

glg_resample <- resample(glg,veendikteraster)
glg_crop <- crop(glg_resample,veendikteraster)
glg_mask <- mask(glg_crop, veendikteraster)

peilgebieden$co2som <- ''
peilgebieden$co2eind <- ''
peilgebieden$co2begin <- ''

glg_mask[!is.na(glg_mask)] <- 0.4

registerDoParallel(cores = 3)

tic()

for(j in 1:length(peilgebieden[,])){

## Neem veendikte per peilvak

veendiktepeilvak <- crop(veendikteraster,peilgebieden[j,])
veendiktepeilvak <- mask(veendiktepeilvak,peilgebieden[j,])
veendiktepeilvak[veendiktepeilvak < 0] <- 0
peilgebieden_raster <- rasterize(peilgebieden[j,], veendiktepeilvak, field = "peil")
glgpeilvak <- crop(glg_mask,peilgebieden[j,])
glgpeilvak <- mask(glgpeilvak,peilgebieden[j,])

## Bereken co2 emissies per cell

co2eindraster <- veendiktepeilvak
co2eindraster[!is.na(co2eindraster)][] <- 1
co2somraster <- co2eindraster *30 

for (i in 1:length(veendiktepeilvak)) {
  if(!is.na(veendiktepeilvak[i]) & !is.na(glgpeilvak[i])){
  tabel <- co2_emissies_vernatting(glgpeilvak[i],veendiktepeilvak[i])
  co2eind <- tabel[1,2]
  co2eindraster[i] <- co2eind
  co2som <- tabel[2,2]
  co2somraster[i] <- co2som
  }}

somrasterlist[[j]] <- co2somraster  
eindrasterlist[[j]] <- co2eindraster

## Bereken gemiddelden

co2som <- cellStats(somrasterlist[[j]], stat='mean', na.rm=TRUE)
co2eind <- cellStats(eindrasterlist[[j]], stat='mean', na.rm=TRUE)

peilgebieden$co2som[j] <- co2som
peilgebieden$co2eind[j] <- co2eind
  
writeRaster(somrasterlist[[j]],paste("inter/somrasters/vernatting/somraster_vernatting_", peilgebieden$naam[j], ".tif", sep=''),datatype='FLT4S', overwrite=TRUE)
writeRaster(eindrasterlist[[j]],paste("inter/eindrasters/vernatting/eindraster_vernatting_", peilgebieden$naam[j],".tif", sep=''),datatype='FLT4S', overwrite=TRUE)

}

toc()

writeOGR(peilgebieden,dsn = "inter/shapes/vernatting", layer = "peilgebiedenemissiesvernatting", driver="ESRI Shapefile", overwrite_layer = TRUE)

## Combine rasters

somrastergroot <- do.call(merge, somrasterlist)
writeRaster(somrastergroot,"final/vernatting/vernatting_somrastergroot.tif",datatype='FLT4S', overwrite=TRUE)
eindrastergroot <- do.call(merge, eindrasterlist)
writeRaster(eindrastergroot,"final/vernatting/vernatting_eindrastergroot.tif",datatype='FLT4S', overwrite=TRUE)


getDoParWorkers()