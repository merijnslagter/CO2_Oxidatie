# bepaal emissies na intervals

co2_emissies <- function(veendiktebovenwater, emissieratio = 0.5, verminderratio = 0.9974, co2max = 50, co2min = 1, tijdsinterval = 5, aantalintervals = 6){

co2_emissies_na_t <- emissieratio * veendiktebovenwater * 100 * verminderratio ^ (tijdsinterval * aantalintervals)
if(co2_emissies_na_t > co2max){co2_emissies_na_t <- co2max}
if(co2_emissies_na_t < co2min){co2_emissies_na_t <- co2min}

return(co2_emissies_na_t)
}