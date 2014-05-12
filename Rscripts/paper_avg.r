#!/usr/bin/Rscript

source("init_init.r")
Sys.setenv(ScriptName = thisFile())

deltaTime <- 300 # in seconds

flattenList <- function(l) {
	lts <- (sapply(l, class) == "list")
	if(any(lts)) {
		return( c(l[!lts], flattenList( do.call(c, l[lts]) ) ) )
	} else {
		return( l )
	}
}

ma <- function(x, n=7) {
	i <- n %/% 2
	padded <- c(rep(x[1],i), x, rep(x[length(x)],i))
	convolve(padded, rep(1/n,n), type = "filter")
}

prepareData <- function(dataPerRoomList) {
	dataPerRoomList <- dataPerRoomList[names(dataPerRoomList) %in% c("60","61","62")]
	lapply(dataPerRoomList, FUN=function(df) {
		sensorDataTable <- prepareDataForLocationTable(df, deltaTime)
		sensorDataTable$co2deriv <- derivSpline(sensorDataTable$timestamp, sensorDataTable$co2, deriv=1)
		sensorDataTable$co2deriv2 <- derivSpline(sensorDataTable$timestamp, sensorDataTable$co2, deriv=2)
		sensorDataTable$avgCo2 <- ma(sensorDataTable$co2)
		sensorDataTable$avgCo2deriv <- ma(sensorDataTable$co2deriv)
		sensorDataTable$avgCo2deriv2 <- ma(sensorDataTable$co2deriv2)
		return(sensorDataTable)
	})
}

trainingData <- cutoffUnusedDays(allDataRaw, timeFrames, "trainingStartDate", "trainingStopDate")
validationData <- cutoffUnusedDays(allDataRaw, timeFrames, "validationStartDate", "validationStopDate")

dataPerRoomTraining <- dlply(trainingData, c("loc_id"))
dataPerRoomValidation <- dlply(validationData, c("loc_id"))

dataPerRoomTrainingAug <- prepareData(dataPerRoomTraining)
dataPerRoomValidationAug <- prepareData(dataPerRoomValidation)

cat("Train models.\n")
sensorCombinations <- list(
				c("co2", "co2deriv"),
				c("co2", "co2deriv", "co2deriv2"),
				c("avgCo2", "avgCo2deriv"),
				c("avgCo2", "avgCo2deriv", "avgCo2deriv2")
			)
#names(sensorCombinations) <-  lapply(sensorCombinations, function(f) paste(f, collapse="-") )


models <- mapply(FUN=function(df, loc_id) {
	l <- list(
			# define configurations for which classification has to be run:
			lapply(sensorCombinations, function(s) {
				RunConf(model=SimpleMarkov(sensorData=df, sensors=s),
					sensorFeat=s, loc_id=loc_id) }),
			RunConf(model=alwaysUnoccupied(), loc_id=loc_id),
			RunConf(model=alwaysOccupied(), loc_id=loc_id)
		)
	return(l)
}, dataPerRoomTrainingAug, names(dataPerRoomTrainingAug), SIMPLIFY=FALSE)
models <- flattenList(models)
#classes <- unique(rapply(models, class, how="unlist"))

cat("Make predictions.\n")
models <- lapply(models, function(m) {
	m$trainingPred <- predictFromModel(m$model, newSensorData=dataPerRoomTrainingAug[[m$loc_id]])
	m$validationPred <- predictFromModel(m$model, newSensorData=dataPerRoomValidationAug[[m$loc_id]])
	
	m$trainingMetrics <- lossMetrics(dataPerRoomTrainingAug[[m$loc_id]]$presence, m$trainingPred)
	m$validationMetrics <- lossMetrics(dataPerRoomValidationAug[[m$loc_id]]$presence, m$validationPred)
	return(m)
})


lossMatrix <- rbind.fill(data.frame(dataset="training", ldply(models, function(m) m$trainingMetrics)))
lossMatrix <- rbind(lossMatrix,
		   rbind.fill(data.frame(dataset="validation", ldply(models, function(m) m$validationMetrics))))
print(lossMatrix)

cat("\n\ndone.\n")


