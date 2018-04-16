co2_som_per_interval <- function(veendikte_eerder, co2_eerder, co2max = 50, co2min = 1, aantalintervals = 1, tijdsinterval = 5){

returnlist <- ''

veendikte_later <- veendikte(veendiktebovenwater = veendikte_eerder, aantalintervals = aantalintervals)

co2_later <- 0.5 * veendikte_later * 100

if(co2_later > co2max){co2_later <- co2max}

if(co2_later < co2min){co2_later <- co2min}

emissies_ha_opgeteld_na_1t <- (co2_eerder + co2_later)/2 * tijdsinterval

returnlist <- c(co2_later, emissies_ha_opgeteld_na_1t, veendikte_later)

return(returnlist)

}

