#!/usr/bin/Rscript

source("init_init.r")

deltaTime <- 30000 # in milliseconds


## plot histograms for occupany data
#plotHistOccupancy(dbResult)

## plot raw sensor data for each location:
#d_ply(dbResult, c("loc_id"), function(df) plotLocation(df, "location_basis") )



# for each loc_id, i.e. for each room do an analysis, train a model, plot curves, etc.:
locRes <- dlply(dbResult, c("loc_id"), function(df) {
	sensorDataTable <- prepareDataForLocationTable(df, deltaTime)
	stopifnot(all(sensorDataTable$presence==1 | sensorDataTable$presence==0))
	#add a new column containing first derivative for C0_2
	sensorDataTable$co2deriv <- firstDerivSecondOrderAccCentral(sensorDataTable$co2)
	
	#sensorDataTable encompasses one column per sensor type plus a timestamp column: 
	#print(head(sensorDataTable))
	
	#
	# code goes here
	#
	
	#needs to be commented out / for debugging purposes:
	#stop()

	return(sensorDataTable)
})

columnsToPlot <- c("co2deriv", "co2", "temperature", "presence")
## plot augmented/interpolated data for each location (takes a while depending on how small deltaTime is)
#mapply(function(df, nm) {plotSensorDataTable(df, nm, columnsToPlot)}, locRes, names(locRes))

plotSensorHistograms(locRes, c("co2deriv", "co2", "temperature"))