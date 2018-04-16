veendikteindexatie <- function(waterniveau,veendiktenat, aantalintervals = 1){
  if(waterniveau > veendiktenat + 0.2){
	veendiktebovenwater <- veendiktenat
	veendiktenat <- veendikte(veendiktebovenwater, tijdsinterval = 5, aantalintervals = aantalintervals)
	}
  if(waterniveau < veendiktenat + 0.2){
	
	veendikteonderwater <- veendiktenat + 0.2 - waterniveau
	veendiktebovenwater <- waterniveau - 0.2
	veendiktenat <- veendikte(veendiktebovenwater, tijdsinterval = 5, aantalintervals = aantalintervals)
	veendiktenat <- veendiktenat + veendikteonderwater
	}
  return(veendiktenat)	
  }