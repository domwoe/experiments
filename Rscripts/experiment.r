#!/usr/bin/Rscript

source("init_init.r")

deltaTime <- 300 # in seconds


## plot histograms for occupany data
plotHistOccupancy(dbResult)

## plot raw sensor data for each location:
#d_ply(dbResult, c("loc_id"), function(df) plotLocation(df, "location_basis") )



# for each loc_id, i.e. for each room do an analysis, train a model, plot curves, etc.:
locRes <- dlply(dbResult, c("loc_id"), function(df) {
	sensorDataTable <- prepareDataForLocationTable(df, deltaTime)
	stopifnot(all(sensorDataTable$presence==1 | sensorDataTable$presence==0))
	#add a new column containing first derivative for C0_2
	sensorDataTable$co2deriv <- firstDerivSpline(sensorDataTable$timestamp, sensorDataTable$co2)
	
	#sensorDataTable encompasses one column per sensor type plus a timestamp column: 
	#print(head(sensorDataTable))
	
	simpleMarkovModel <- SimpleMarkov(sensorData=sensorDataTable, steps=4)
	sensorDataTable$prediction <- predictFromModel(simpleMarkovModel, sensorDataTable)
	
	#needs to be commented out / for debugging purposes:
	#stop()
	return(sensorDataTable)
})


## plot augmented/interpolated data for each location (takes a while depending on how small deltaTime is)
columnsToPlot <- c("co2deriv", "co2", "humidity", "prediction", "presence")
mapply(function(df, nm) {plotSensorDataTable(df, nm, columnsToPlot)}, locRes, names(locRes))

plotSensorHistograms(locRes, c("co2deriv", "co2", "temperature", "humidity"))
