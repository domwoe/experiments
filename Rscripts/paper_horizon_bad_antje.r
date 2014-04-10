#!/usr/bin/Rscript

source("init_init.r")
Sys.setenv(ScriptName = thisFile())

deltaTime <- 300 # in seconds

dataAug <- ddply(allDataRaw, c("loc_id"), function(df) {
		sensorDataTable <- prepareDataForLocationTable(df, deltaTime)
		#add a new column containing first derivative for C0_2
		sensorDataTable$co2deriv <- derivSpline(sensorDataTable$timestamp, sensorDataTable$co2, deriv=1)
		sensorDataTable$co2deriv2 <- derivSpline(sensorDataTable$timestamp, sensorDataTable$co2, deriv=2)
		startG <- as.numeric(timeFrames[timeFrames$loc_id==unique(df$loc_id), "experimentStartDate"])
		stopG <- as.numeric(timeFrames[timeFrames$loc_id==unique(df$loc_id), "experimentStopDate"])
		dur <- stopG - startG
		sensorDataTable$day <- (sensorDataTable$timestamp - startG) %/% (24*3600) # compute day count
		ids <- ( sensorDataTable$day == dur %/% (24*3600) )
		sensorDataTable$day[ids] <- sensorDataTable$day[ids] - 1
		return(sensorDataTable)
	})

res <- ddply(dataAug, c("loc_id"), function(df) {
	#print(unique(df$loc_id))
	rl <- ldply(1:(max(df$day)-1), function(i) {
		#cat("P")
		#print(i)
		dataPerRoomTrainingAug <- df[df$day <= i, ]
		dataPerRoomValidationAug <- df[df$day > i, ]

		#print(str(dataPerRoomTrainingAug))
		#print(head(dataPerRoomTrainingAug))

		#print(str(dataPerRoomValidationAug))
		#print(head(dataPerRoomValidationAug))

		simpleMarkovModel <- SimpleMarkov(sensorData=dataPerRoomTrainingAug, sensors=c("co2", "co2deriv", "co2deriv2"))


		#print(str(simpleMarkovModel))
		predValidation <- predictFromModel(simpleMarkovModel, newSensorData=dataPerRoomValidationAug)
		#cat("fdsa\n")
		predTraining <- predictFromModel(simpleMarkovModel, newSensorData=dataPerRoomTrainingAug)



		lossPerRoomValidation <- lossMetrics(dataPerRoomValidationAug$presence, predValidation)
		lossPerRoomTraining <- lossMetrics(dataPerRoomTrainingAug$presence, predTraining)

		data.frame(tDay=i+1, validation=lossPerRoomValidation, training=lossPerRoomTraining, metric=names(lossPerRoomValidation))
	})
	data.frame(loc_id=unique(df$loc_id), rl)
})

plotLossDay(res)


cat("\n\ndone.\n")


