veendikteindexatie <- function(waterniveau,veendiktenat, aantalintervals = 1){
  if(waterniveau > veendiktenat){
	veendiktebovenwater <- veendiktenat
	veendiktenat <- veendikte(veendiktebovenwater, tijdsinterval = 5, aantalintervals = aantalintervals)
	}
  if(waterniveau < veendiktenat){
	veendikteonderwater <- veendiktenat - waterniveau
	veendiktebovenwater <- waterniveau
	veendiktenat <- veendikte(veendiktebovenwater, tijdsinterval = 5, aantalintervals = aantalintervals)
	veendiktenat <- veendiktenat + veendikteonderwater
	}
  return(veendiktenat)	
  }