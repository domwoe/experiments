#!/usr/bin/Rscript

source("init_init.r")

deltaTime <- 300 # in seconds


## plot histograms for occupany data
plotHistOccupancy(dbResult)

## plot raw sensor data for each location:
#d_ply(dbResult, c("loc_id"), function(df) plotLocation(df, "location_basis") )

dataPerRoom <- dlply(dbResult, c("loc_id"))

rasterDataPerRoom <- lapply(dataPerRoom, FUN=function(df) {
	sensorDataTable <- prepareDataForLocationTable(df, deltaTime)
	stopifnot(all(sensorDataTable$presence==1 | sensorDataTable$presence==0))
	#add a new column containing first derivative for C0_2
	sensorDataTable$co2deriv <- firstDerivSpline(sensorDataTable$timestamp, sensorDataTable$co2)	
	#sensorDataTable encompasses one column per sensor type plus a timestamp column: 
	#print(head(sensorDataTable))
	return(sensorDataTable)
})

plotSensorHistograms(rasterDataPerRoom, c("co2deriv", "co2", "temperature", "humidity"))

cat("Train model and make prediction\n")
predictionPerRoom <- lapply(rasterDataPerRoom, FUN=function(df) {
	# pass data to to model and specify which sensors to use for prediction
	simpleMarkovModel <- SimpleMarkov(sensorData=df, sensors=c("co2", "co2deriv", "humidity"))
	# input new data to model and make prediction
	pred <- predictFromModel(simpleMarkovModel, newSensorData=df)
	return(data.frame(timestamp=df$timestamp, presence=pred))
})

cat("Average binary error rate:\n")
lossPerRoom <- mapply(function(dD, rD, pD) {
	pred2smoothed <- meanBinaryLoss(rD$presence, pD$presence)
	smoothed2raw <- meanBinaryLossRaw(dD[dD$unittypename=="presence", ], rD)
	pred2raw <- meanBinaryLossRaw(dD[dD$unittypename=="presence", ], pD)
	data.frame(pred2smoothed=pred2smoothed, smoothed2raw=smoothed2raw, pred2raw=pred2raw)
}, dataPerRoom, rasterDataPerRoom, predictionPerRoom)
lossPerRoom <- as.data.frame(lossPerRoom)
lossPerRoom$measure <- factor(rownames(lossPerRoom))
rownames(lossPerRoom) <- NULL

print(lossPerRoom)
plotLoss(lossPerRoom)

cat("Generating plots...\n")
## plot augmented/interpolated data for each location (takes a while depending on how small deltaTime is)
columnsToPlot <- c("co2deriv", "co2", "humidity", "presence")
mapply(function(rD, nm, pD, dD) plotSensorDataTable(rD, nm, pD, dD, columnsToPlot),
						rasterDataPerRoom,
						names(rasterDataPerRoom),
						predictionPerRoom,
						dataPerRoom )
cat("done.\n")


