co2_som_per_interval <- function(veendikte_eerder, co2_eerder, aantalintervals = 1, tijdsinterval = 5){

returnlist <- ''

co2_later <- co2_emissies(veendikte_eerder, aantalintervals = aantalintervals)

emissies_ha_opgeteld_na_1t <- (co2_eerder + co2_later)/2 * tijdsinterval

veendikte_later <- veendikte(veendiktebovenwater = veendikte_eerder, aantalintervals = aantalintervals)

returnlist <- c(co2_later, emissies_ha_opgeteld_na_1t, veendikte_later)

return(returnlist)

}

