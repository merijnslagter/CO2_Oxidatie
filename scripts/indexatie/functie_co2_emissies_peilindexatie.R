co2_emissies_peilfixatie <- function(waterniveau, veendikte, co2max=50,tijdsinterval=5,aantalintervals=6,verminderratio=0.9974,emissieratio=0.5){

returnlist <- ''

ifelse(waterniveau > veendikte, veendiktebovenwater <- veendikte, veendiktebovenwater <- waterniveau)

# bepaal emissies op tijdstip nul

co2_emissies_begin <- emissieratio * veendiktebovenwater * 100

# bepaal emissies na alle intervals

# co2_na_alle_intervals <- co2_emissies(veendiktebovenwater) = anders

# bepaal emissies na eerste interval

output_na_1t <- co2_som_per_interval(veendiktebovenwater,co2_emissies_begin)

# bepaal emissies na tweede interval

output_na_2t <- co2_som_per_interval(output_na_1t[3], output_na_1t[1])

# bepaal emissies na derde interval

output_na_3t <- co2_som_per_interval(output_na_2t[3], output_na_2t[1])

# bepaal emissies na vierde interval

output_na_4t <- co2_som_per_interval(output_na_3t[3], output_na_3t[1])

# bepaal emissies na vijfde interval

output_na_5t <- co2_som_per_interval(output_na_4t[3], output_na_4t[1])

# bepaal emissies na zesde interval

output_na_6t <- co2_som_per_interval(output_na_5t[3], output_na_5t[1])

# tel de emissies per 5 jaar open

totaal_co2_emissies_ton_ha <- sum(output_na_1t[2],output_na_2t[2],output_na_3t[2],output_na_4t[2],output_na_5t[2],output_na_6t[2])

returnnames <- c("co2_na_alle_intervals", "totaal_co2_emissies_ton_ha")

returnlist <- c(co2_na_alle_intervals, totaal_co2_emissies_ton_ha)

returndf <- data.frame(returnnames,returnlist)

return(returndf)

}