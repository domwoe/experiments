#!/usr/bin/Rscript

source("init_init.r")

deltaTime <- 300 # in seconds

## plot histograms for occupany data
plotHistOccupancy(allDataRaw)

## plot raw sensor data for each location:
d_ply(allDataRaw, c("loc_id"), function(df) plotLocation(df, "location_basis") )

dataPerRoom <- dlply(allDataRaw, c("loc_id"))

rasterDataPerRoom <- lapply(dataPerRoom, FUN=function(df) {
	sensorDataTable <- prepareDataForLocationTable(df, deltaTime)
	#add a new column containing first derivative for C0_2
	sensorDataTable$co2deriv <- derivSpline(sensorDataTable$timestamp, sensorDataTable$co2, deriv=1)
	sensorDataTable$co2deriv2 <- derivSpline(sensorDataTable$timestamp, sensorDataTable$co2, deriv=2)
	#add derivative for temperature
	sensorDataTable$tempDeriv <- derivSpline(sensorDataTable$timestamp, sensorDataTable$temperature)	
	#sensorDataTable encompasses one column per sensor type plus a timestamp column: 
	#print(head(sensorDataTable))
	return(sensorDataTable)
})

## scatter plot for window and door contacts
plotScatterPlotContacts(rasterDataPerRoom)

plotSensorHistograms(rasterDataPerRoom, c("co2deriv", "co2", "temperature", "tempDeriv", "humidity"))

cat("Train model and make prediction\n")
predictionPerRoom <- lapply(rasterDataPerRoom, FUN=function(df) {
	# pass data to to model and specify which sensors to use for prediction
	simpleMarkovModel <- SimpleMarkov(sensorData=df, sensors=c("co2", "co2deriv"))
	# input new data to model and make prediction
	pred <- predictFromModel(simpleMarkovModel, newSensorData=df)
	return(data.frame(timestamp=df$timestamp, presence=pred))
})

cat("Average error rate:\n")
lossPerRoom <- mapply(function(rasterDataPerRoomE, predictionPerRoomE) {
	lossMetrics(rasterDataPerRoomE$presence, predictionPerRoomE$presence)
}, rasterDataPerRoom, predictionPerRoom)
lossPerRoom <- as.data.frame(lossPerRoom)
lossPerRoom$measure <- factor(rownames(lossPerRoom))
print(lossPerRoom)
plotLoss(lossPerRoom)

#lossPerRoom <- mapply(function(dD, rD, pD) {
#	pred2smoothed <- meanBinaryLoss(rD$presence, pD$presence)
#	smoothed2raw <- meanBinaryLossRaw(dD[dD$unittypename=="presence", ], rD)
#	pred2raw <- meanBinaryLossRaw(dD[dD$unittypename=="presence", ], pD)
#	data.frame(pred2smoothed=pred2smoothed, smoothed2raw=smoothed2raw, pred2raw=pred2raw)
#}, dataPerRoom, rasterDataPerRoom, predictionPerRoom)
#lossPerRoom <- as.data.frame(lossPerRoom)
#lossPerRoom$measure <- factor(rownames(lossPerRoom))
#rownames(lossPerRoom) <- NULL
#print(lossPerRoom)
#plotLoss(lossPerRoom)

cat("Generating plots...\n")
## plot augmented/interpolated data for each location (takes a while depending on how small deltaTime is)
columnsToPlot <- c("co2deriv", "co2", "humidity", "presence")
mapply(function(rD, nm, pD, dD) plotSensorDataTable(rD, nm, pD, dD, columnsToPlot),
						rasterDataPerRoom,
						names(rasterDataPerRoom),
						predictionPerRoom,
						dataPerRoom )
cat("done.\n")


