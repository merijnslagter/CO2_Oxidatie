## Peilfixatie model Valthermond

setwd("/home/merijn/Documents/Nijmegen_Stu_As/Project_Valthermond/Scenariostudie/R")

require(rgdal)
require(rgeos)
require(raster)

## Laad bestanden

peilgebieden <- readOGR("source/peilgebieden_valthermond","peilgebieden_valthermond")
veendikteraster <- raster("source/veendikte.tif")
glg <- raster("source/glg.tif")
crs(glg) <- proj4string(veendikteraster)

## Variabelen definieren

co2max <- 50
co2min <- 1

## Voorbereiding files

beginrasterlist <- list()

glg_resample <- resample(glg,veendikteraster)
glg_crop <- crop(glg_resample,veendikteraster)
glg_mask <- mask(glg_crop, veendikteraster)

peilgebieden$co2begin <- ''

## Geef nulwaarden voor waarden lager dan nul

glg_mask[glg_mask < 0] <- 0
veendikteraster[veendikteraster < 0] <- 0

## Loop door elk peilvak

for(j in 1:length(peilgebieden[,])){

## Neem veendikte en glg per peilvak

veendiktepeilvak <- crop(veendikteraster,peilgebieden[j,])
veendiktepeilvak <- mask(veendiktepeilvak,peilgebieden[j,])

glgpeilvak <- crop(glg_mask,peilgebieden[j,])
glgpeilvak <- mask(glgpeilvak,peilgebieden[j,])

## bepaal emissies op punt 0

co2beginraster <- veendiktepeilvak
co2beginraster[!is.na(co2beginraster)] <- 1

for (i in 1:length(veendiktepeilvak)){
   if(!is.na(veendiktepeilvak[i]) & !is.na(glgpeilvak[i])){
      waterniveau <- glgpeilvak[i]
      veendikte <- veendiktepeilvak[i]
      if(waterniveau > veendikte + 0.2) {veendiktebovenwater <- veendikte}
      if(waterniveau < veendikte + 0.2) {veendiktebovenwater <- waterniveau - 0.2} 
      co2_emissies_begin <- 0.5 * veendiktebovenwater * 100
      if(co2_emissies_begin > co2max){co2_emissies_begin <- co2max}
      if(co2_emissies_begin < co2min){co2_emissies_begin <- co2min}
      co2beginraster[i] <- co2_emissies_begin
      }}
      
beginrasterlist[[j]] <- co2beginraster        
      
co2begin <- cellStats(beginrasterlist[[j]], stat='mean', na.rm=TRUE)     

peilgebieden$co2begin[j] <- co2begin

writeRaster(beginrasterlist[[j]],paste("inter/beginrasters/beginraster_", peilgebieden$naam[j], ".tif", sep=''),datatype='FLT4S', overwrite=TRUE)

}

writeOGR(peilgebieden,dsn = "inter/shapes/beginemissies", layer = "peilgebiedenbeginemissies", driver="ESRI Shapefile", overwrite_layer = TRUE)

## Combineer rasters

beginrastergroot <- do.call(merge, beginrasterlist)

writeRaster(beginrastergroot,"final/beginrastergroot.tif",datatype='FLT4S', overwrite=TRUE)