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

ma <- function(x, n=9) { # n must be odd
	i <- n %/% 2
	padded <- c(rep(x[1],i), x, rep(x[length(x)],i))
	ret <- convolve(padded, rep(1/n,n), type = "filter")
	stopifnot(length(ret) == length(x))
	return(ret)
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

entireData <- cutoffUnusedDays(allDataRaw, timeFrames, "trainingStartDate", "validationStopDate")
trainingData <- cutoffUnusedDays(allDataRaw, timeFrames, "trainingStartDate", "trainingStopDate")
validationData <- cutoffUnusedDays(allDataRaw, timeFrames, "validationStartDate", "validationStopDate")

dataPerRoomEntire <- dlply(entireData, c("loc_id"))
dataPerRoomTraining <- dlply(trainingData, c("loc_id"))
dataPerRoomValidation <- dlply(validationData, c("loc_id"))

dataPerRoomEntireAug <- prepareData(dataPerRoomEntire)
dataPerRoomTrainingAug <- prepareData(dataPerRoomTraining)
dataPerRoomValidationAug <- prepareData(dataPerRoomValidation)

cat("Train models.\n")
sensorCombinations <- list()
sensorCombinations[["60"]] <- c("co2deriv", "avgCo2deriv", "co2", "avgCo2")
sensorCombinations[["61"]] <- c("co2deriv", "avgCo2deriv", "co2deriv2")
sensorCombinations[["62"]] <- c("co2", "avgCo2")

models <- mapply(FUN=function(df, loc_id) {
	l <- list(
		RunConf(model=SVMwindowed(sensorData=df, sensors=sensorCombinations[[loc_id]]),
					sensorFeat=sensorCombinations[[loc_id]], loc_id=loc_id),

		RunConf(model=SVM(sensorData=df, sensors=sensorCombinations[[loc_id]]),
					sensorFeat=sensorCombinations[[loc_id]], loc_id=loc_id),

		RunConf(model=SimpleMarkov(sensorData=df, sensors=sensorCombinations[[loc_id]]),
					sensorFeat=sensorCombinations[[loc_id]], loc_id=loc_id),

		RunConf(model=ConditionalMarkov(sensorData=df, sensors=sensorCombinations[[loc_id]]),
					sensorFeat=sensorCombinations[[loc_id]], loc_id=loc_id)
		)
	return(l)
}, dataPerRoomTrainingAug, names(dataPerRoomTrainingAug), SIMPLIFY=FALSE)
models <- flattenList(models)

cat("Make predictions.\n")
models <- lapply(models, function(m) {
	m$trainingPred <- predictFromModel(m$model, newSensorData=dataPerRoomTrainingAug[[m$loc_id]])
	m$validationPred <- predictFromModel(m$model, newSensorData=dataPerRoomValidationAug[[m$loc_id]])
	m$unsupervisedPred <- predictUnsupervised(m$model, newSensorData=dataPerRoomEntireAug[[m$loc_id]])
	
	m$trainingMetrics <- lossMetrics(dataPerRoomTrainingAug[[m$loc_id]]$presence, m$trainingPred)
	m$validationMetrics <- lossMetrics(dataPerRoomValidationAug[[m$loc_id]]$presence, m$validationPred)
	m$unsupervisedMetrics <- lossMetrics(dataPerRoomEntireAug[[m$loc_id]]$presence, m$unsupervisedPred)
	return(m)
})

#predMatrixTraining <- data.frame(dataset="training", extractCombine(models, "trainingPred", transpose=TRUE))
#print(head(predMatrixTraining))

lossMatrixTraining <- data.frame(dataset="training", extractCombine(models, "trainingMetrics"))
lossMatrixValidation <- data.frame(dataset="validation", extractCombine(models, "validationMetrics"))
lossMatrixUnsupervised <- data.frame(dataset="unsupervised", extractCombine(models, "unsupervisedMetrics"))

print(lossMatrixValidation)

plotLossMatrixBEST(lossMatrixTraining, filename="lossMatTraining.pdf")
plotLossMatrixBEST(lossMatrixValidation, filename="lossMatValidation.pdf")
plotLossMatrixBEST(lossMatrixUnsupervised, filename="lossMatUnsupervised.pdf")

cat("Plot time series.\n")
# plot all time series
lapply(models, function(m) {
	plotTSforRC(	"t", m$loc_id,
			class(m$model),
			m$sensorFeat,
			dataPerRoomTrainingAug[[m$loc_id]],
			dataPerRoomTraining[[m$loc_id]],
			m$trainingPred
		   )
	plotTSforRC(	"v", m$loc_id,
			class(m$model),
			m$sensorFeat,
			dataPerRoomValidationAug[[m$loc_id]],
			dataPerRoomValidation[[m$loc_id]],
			m$validationPred
		   )
	plotTSforRC(	"u", m$loc_id,
			class(m$model),
			m$sensorFeat,
			dataPerRoomEntireAug[[m$loc_id]],
			dataPerRoomEntire[[m$loc_id]],
			m$unsupervisedPred
		   )		
})



cat("\n\ndone.\n")

