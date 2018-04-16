# bepaal emissies na intervals

veendikte <- function(veendiktebovenwater, verminderratio = 0.9974, tijdsinterval = 5, aantalintervals = 6){

veendikte_na_t <- veendiktebovenwater * verminderratio ^ (tijdsinterval * aantalintervals)

return(veendikte_na_t)
}

