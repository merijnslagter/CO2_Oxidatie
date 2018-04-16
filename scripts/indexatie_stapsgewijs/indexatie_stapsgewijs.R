setwd("/home/merijn/Documents/Nijmegen_Stu_As/Project_Valthermond/Scenariostudie/R")

require(rgdal)
require(rgeos)
require(raster)

source("scripts/indexatie_stapsgewijs/functie_co2_emissies_peilindexatie.R")
source("scripts/indexatie_stapsgewijs/functie_co2_emissies.R")
source("scripts/indexatie_stapsgewijs/functie_co2_som_per_interval.R")
source("scripts/indexatie_stapsgewijs/functie_veendikte.R")
source("scripts/indexatie_stapsgewijs/functie_veendikte_indexatie.R")

## Laad bronbestanden

peilgebieden <- readOGR("source/peilgebieden_valthermond","peilgebieden_valthermond")
veendikteraster <- raster("source/veendikte.tif")
glg <- raster("source/glg.tif")
crs(glg) <- proj4string(veendikteraster)

## Voorbereiding

glg_resample <- resample(glg,veendikteraster)
glg_crop <- crop(glg_resample,veendikteraster)
glg_mask <- mask(glg_crop, veendikteraster)

somrasterlist <- list()
eindrasterlist <- list()

peilgebieden$co2som <- ''
peilgebieden$co2eind <- ''

## Geef nulwaarden voor waarden lager dan nul

glg_mask[glg_mask < 0] <- 0
veendikteraster[veendikteraster < 0] <- 0

## Variabelen bepalen

verlaging <- 0.05
co2max <- 50
co2min <- 1

## Start loop

for(j in 1:length(peilgebieden[,])){

## Neem veendikte per peilvak

if(peilgebieden$naam[j] != ''){

veendiktepeilvak <- crop(veendikteraster,peilgebieden[j,])
veendiktepeilvak <- mask(veendiktepeilvak,peilgebieden[j,])
glgpeilvak <- crop(glg_mask,peilgebieden[j,])
glgpeilvak <- mask(glgpeilvak,peilgebieden[j,])

## Bepaal indexatie emissies

## Zet rasters klaar voor loep

glgpeilvaknat <- glgpeilvak
veendikterasternat <- veendiktepeilvak
veendiktebovenwaterrasternat <- veendikterasternat
veendiktebovenwaterrasternat[!is.na(veendiktebovenwaterrasternat)][] <- 0
co2rasternat <- veendiktepeilvak
co2rasternat[!is.na(co2rasternat)][] <- 0 
co2beginraster <- veendiktepeilvak
co2beginraster[!is.na(co2beginraster)][] <- 0 
co2somraster <- veendiktepeilvak
co2somraster[!is.na(co2somraster)][] <- 0

for (k in 1:6){

for (i in 1:length(veendikterasternat)){
   if(!is.na(veendikterasternat[i]) & !is.na(glgpeilvaknat[i])){    
      waterniveau <- glgpeilvaknat[i]
      veendiktenat <- veendikterasternat[i]
      if(waterniveau > veendiktenat + 0.2) {veendiktebovenwater <- veendiktenat}
      if(waterniveau < veendiktenat + 0.2) {veendiktebovenwater <- waterniveau - 0.2} 
      if(veendikterasternat[i] == veendiktepeilvak[i]){
	  co2begin <- 0.5 * veendiktebovenwater * 100
	  if(co2begin > co2max){co2begin <- co2max}
	  if(co2begin < co2min){co2begin <- co2min}
	  co2beginraster[i] <- co2begin
	  }
      veendiktebovenwaternat <- veendikte(veendiktebovenwater, aantalintervals = 1)
      veendiktebovenwaterrasternat[i] <- veendiktebovenwaternat
      co2nat <- 0.5 * veendiktebovenwaternat * 100
      if(co2nat > co2max){co2nat <- co2max}
      if(co2nat < co2min){co2nat <- co2min}
      co2rasternat[i] <- co2nat
      veendiktenat <- veendikteindexatie(waterniveau,veendiktenat,aantalintervals = 1 )
      veendikterasternat[i] <- veendiktenat
      }}

co2somraster <- co2somraster + (co2beginraster + co2rasternat)/2 * 5
co2beginraster <- co2rasternat
glgpeilvaknat <- glgpeilvaknat + verlaging
}

somrasterlist[[j]] <- co2somraster
eindrasterlist[[j]] <- co2rasternat

co2som <- cellStats(somrasterlist[[j]], stat='mean', na.rm=TRUE)
co2eind <- cellStats(eindrasterlist[[j]], stat='mean', na.rm=TRUE)

peilgebieden$co2som[j] <- co2som
peilgebieden$co2eind[j] <- co2eind


writeRaster(somrasterlist[[j]],paste("inter/somrasters/indexatie_stapsgewijs/somraster_indexatie_stapsgewijs_", peilgebieden$naam[j], ".tif", sep=''),datatype='FLT4S', overwrite=TRUE)
writeRaster(eindrasterlist[[j]],paste("inter/eindrasters/indexatie_stapsgewijs/eindraster_indexatie_stapsgewijs_", peilgebieden$naam[j],".tif", sep=''),datatype='FLT4S', overwrite=TRUE)

}
}

writeOGR(peilgebieden,dsn = "inter/shapes/indexatie_stapsgewijs", layer = "peilgebiedenemissiesindexatiestapsgewijs", driver="ESRI Shapefile", overwrite_layer = TRUE)

## Combine rasters

somrastergroot <- do.call(merge, somrasterlist)
writeRaster(somrastergroot,"final/indexatie_stapsgewijs/indexatie_stapsgewijs_somrastergroot.tif",datatype='FLT4S', overwrite=TRUE)
eindrastergroot <- do.call(merge, eindrasterlist)
writeRaster(eindrastergroot,"final/indexatie_stapsgewijs/indexatie_stapsgewijs_eindrastergroot.tif",datatype='FLT4S', overwrite=TRUE)
