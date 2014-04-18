#!/usr/bin/Rscript

source("init_init.r")
Sys.setenv(ScriptName = thisFile())

deltaTime <- 300 # in seconds
features <- c("co2deriv", "co2", "temperature", "co2deriv2", "humidity")

getFeatureRanking <- function(data, excludedQuant, nBins) {
	ddply(data, .(loc_id), function(df) {
		room <- ldply(features, function(col) {
			qnts <- quantile(df[, col], probs= c(excludedQuant, 1-excludedQuant) )
			keepIds <- df[, col] >= qnts[1] & df[, col] <= qnts[2]
			tmp <- df[keepIds, c("presence", col)]
			br <- seq(qnts[1], qnts[2], length.out=nBins+1)
			presenceCounts <- as.vector(table(tmp$presence))
			#print(presenceCounts)
			presenceEntropy <- entropy.empirical(presenceCounts, unit="log2")
			miCounts <- ddply(tmp, .(presence), function(dp) {
				hist(dp[, col], breaks=br, plot=FALSE)$counts
			})
			#print(miCounts)
			colMI <- mi.empirical(miCounts, unit="log2")
			data.frame(sensor=col, presenceEntropy=presenceEntropy, sensorMutualInfo=colMI,
				relMutualInfo=colMI/presenceEntropy)
		})
	})
}



dataAug <- ddply(allDataRaw, c("loc_id"), function(df) {
		sensorDataTable <- prepareDataForLocationTable(df, deltaTime)
		sensorDataTable$co2deriv <- derivSpline(sensorDataTable$timestamp,sensorDataTable$co2,deriv=1)
		sensorDataTable$co2deriv2 <- derivSpline(sensorDataTable$timestamp,sensorDataTable$co2,deriv=2)
		return(sensorDataTable)
	})


args <- expand.grid(excludedQuant=c(0.00,0.02,0.04), nBins=c(20,30,50) )

featureRanking <- mdply(args, getFeatureRanking, data=dataAug)

plotFeatureRanking(featureRanking)

plotHistOccupancy(allDataRaw)
