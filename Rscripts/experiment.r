#!/usr/bin/Rscript

source("init_init.r")

deltaTime <- 30000 # in milliseconds


# plot histograms for occupany data
plotHistOccupancy(dbResult)

# for each loc_id, i.e. for each room do an analysis, train a model, plot curves, etc.:
ddply(dbResult, c("loc_id"), function(df) {
	sensorDataInterp <- prepareDataForLocation(df, deltaTime)
	##### some output
	#print(head(df))
	#cat('\n\n')
	#print(head(sensorDataInterp))
	#
	# plot sensor data per location:
	# plotLocation(df, "location_basis")
	#
	#
	##### do something with useful with sensorDataInterp.
	##### data frame sensorDataInterp has 4 columns: sens_id, unittypename, reading, timestamp
	#
	#     code goes here...
	
	stop()
})

