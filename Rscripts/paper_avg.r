#!/usr/bin/Rscript

source("init_init.r")
Sys.setenv(ScriptName = thisFile())


deltaTime <- 300 # in seconds

ma <- function(x, n=7) {
	i <- n %/% 2
	padded <- c(rep(x[1],i), x, rep(x[length(x)],i))
	convolve(padded, rep(1/n,n), type = "filter")
}


## plot histograms for occupany data
#plotHistOccupancy(allDataRaw)
#plotHistOccupancyPaper(allDataRaw)

prepareData <- function(dataPerRoomList) {
	dataPerRoomList <- dataPerRoomList[names(dataPerRoomList) %in% c("60","61","62")]
	lapply(dataPerRoomList, FUN=function(df) {
		sensorDataTable <- prepareDataForLocationTable(df, deltaTime)
		#add a new column containing first derivative for C0_2
		sensorDataTable$co2deriv <- derivSpline(sensorDataTable$timestamp, sensorDataTable$co2, deriv=1)
		sensorDataTable$co2deriv2 <- derivSpline(sensorDataTable$timestamp, sensorDataTable$co2, deriv=2)
		sensorDataTable$avgCo2 <- ma(sensorDataTable$co2)
		sensorDataTable$avgCo2deriv <- ma(sensorDataTable$co2deriv)
		return(sensorDataTable)
	})
}

#plotSensorHistograms(prepareData(dlply(allDataRaw, c("loc_id"))), c("co2deriv", "co2", "temperature", "co2deriv2", "humidity"))

#plotSensorHistogramsAntje(prepareData(dlply(allDataRaw, c("loc_id"))), c("co2", "co2deriv", "co2deriv2", "temperature", "humidity"))

trainingData <- cutoffUnusedDays(allDataRaw, timeFrames, "trainingStartDate", "trainingStopDate")
validationData <- cutoffUnusedDays(allDataRaw, timeFrames, "validationStartDate", "validationStopDate")

## plot raw sensor data for each location:
#d_ply(allDataRaw, c("loc_id"), function(df) plotLocation(df, "raw") )

#d_ply(trainingData, c("loc_id"), function(df) plotLocation(df, "training_basis") )
#d_ply(validationData, c("loc_id"), function(df) plotLocation(df, "validation_basis") )

dataPerRoomTraining <- dlply(trainingData, c("loc_id"))
dataPerRoomValidation <- dlply(validationData, c("loc_id"))

dataPerRoomTrainingAug <- prepareData(dataPerRoomTraining)
dataPerRoomValidationAug <- prepareData(dataPerRoomValidation)

cat("Train model:\n")
modelPerRoom <- lapply(dataPerRoomTrainingAug, FUN=function(df) {
	# pass data to to model and specify which sensors to use for prediction
	simpleMarkovModel <- SimpleMarkov(sensorData=df, sensors=c("co2", "co2deriv", "co2deriv2"))
	return(simpleMarkovModel)
})
nm <- names(modelPerRoom)
modelPerRoomAvg <- lapply(dataPerRoomTrainingAug, FUN=function(df) {
	# pass data to to model and specify which sensors to use for prediction
	simpleMarkovModel <- SimpleMarkov(sensorData=df, sensors=c("avgCo2", "avgCo2deriv"))
	return(simpleMarkovModel)
})
modelPerRoom <- c(modelPerRoom, modelPerRoomAvg)
nm_new <- c(nm, paste(nm, "avg", sep=""))
names(modelPerRoom) <- nm_new


cat("Make predictions.\n")
predictionPerRoomValidation <- mapply(function(model, validation) {
	pred <- predictFromModel(model, newSensorData=validation)
	return(data.frame(timestamp=validation$timestamp, presence=pred))
}, modelPerRoom, dataPerRoomValidationAug, SIMPLIFY=FALSE)

predictionPerRoomTraining <- mapply(function(model, training) {
	pred <- predictFromModel(model, newSensorData=training)
	return(data.frame(timestamp=training$timestamp, presence=pred))
}, modelPerRoom, dataPerRoomTrainingAug, SIMPLIFY=FALSE)


cat("Calculate metrics.\n")
lossPerRoomValidation <- mapply(function(validation, prediction) {
	lossMetrics(validation$presence, prediction$presence)
}, setNames(rep(dataPerRoomValidationAug,2), nm_new), predictionPerRoomValidation)
plotLoss(lossPerRoomValidation, title="Metrics Validation", filename="validationMetrics.pdf")


lossPerRoomTraining <- mapply(function(validation, prediction) {
	lossMetrics(validation$presence, prediction$presence)
}, setNames(rep(dataPerRoomTrainingAug,2), nm_new), predictionPerRoomTraining)
plotLoss(lossPerRoomTraining, title="Metrics Training", filename="trainingMetrics.pdf")


#cat("Generating plots...\n")
#columnsToPlot <- c("co2deriv", "co2", "co2deriv2", "presence")

#plotSingleDayPaperAntjeBad(dataPerRoomValidationAug, predictionPerRoomValidation, dataPerRoomValidation)



#mapply(function(rD, nm, pD, dD) plotSensorDataTable(rD, nm, pD, dD, columnsToPlot, prefix="training"),
#						dataPerRoomTrainingAug,
#						names(dataPerRoomTrainingAug),
#						predictionPerRoomTraining,
#						dataPerRoomTraining )
#mapply(function(rD, nm, pD, dD) plotSensorDataTable(rD, nm, pD, dD, columnsToPlot, prefix="validation"),
#						dataPerRoomValidationAug,
#						names(dataPerRoomValidationAug),
#						predictionPerRoomValidation,
#						dataPerRoomValidation )


cat("\n\ndone.\n")

