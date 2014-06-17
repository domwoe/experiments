#!/usr/bin/Rscript

source("init_init.r")
Sys.setenv(ScriptName = thisFile())

deltaTime <- 300 # in seconds
features <- c("co2deriv", "avgCo2deriv", "co2", "avgCo2", "co2deriv2", "avgCo2deriv2", "temperature", "humidity")


#ma <- function(x,n=5){as.vector(filter(x,rep(1/n,n), sides=2))}
ma <- function(x, n=7) {
	i <- n %/% 2
	padded <- c(rep(x[1],i), x, rep(x[length(x)],i))
	convolve(padded, rep(1/n,n), type = "filter")
}

getEntropyFeatureRanking <- function(data, excludedQuant, nBins) {
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

getCorrelation <- function(data) {
	ddply(data, .(loc_id), function(df) {
		corrM <- as.data.frame( cor(df[, features]) )
		corrM$feature <- rownames(corrM)
		return(corrM)
	})	
}

getPrinCompAnalysis <- function(data) {
	ddply(data, .(loc_id), function(df) {
		tmp <- scale(df[, features])
		princompObj <- princomp(tmp)
		vars <- princompObj$sdev^2
		vars <- vars/sum(vars)
		load <- unclass(princompObj$loadings)
		load <- as.data.frame(rbind(load, ProportionOfVar=vars))
		load$feature <- rownames(load)
		ret <- melt(load, id.vars="feature")
		return(ret)
	})	
}

dataAug <- ddply(allDataRaw, c("loc_id"), function(df) {
		sensorDataTable <- prepareDataForLocationTable(df, deltaTime)
		sensorDataTable$co2deriv <- derivSpline(sensorDataTable$timestamp,sensorDataTable$co2,deriv=1)
		sensorDataTable$co2deriv2 <- derivSpline(sensorDataTable$timestamp,sensorDataTable$co2,deriv=2)
		#print(filter(sensorDataTable$co2, rep(1,3)))
		sensorDataTable$avgCo2 <- ma(sensorDataTable$co2)
		sensorDataTable$avgCo2deriv <- ma(sensorDataTable$co2deriv)
		sensorDataTable$avgCo2deriv2 <- ma(sensorDataTable$co2deriv2)
		return(sensorDataTable)
	})

corrdf <- getCorrelation(dataAug)
plotCorrelationMatrix(corrdf, features)

princompAnalysis <- getPrinCompAnalysis(dataAug)
plotPrinCompAnalysis(princompAnalysis)

args <- expand.grid(excludedQuant=c(0.00,0.02,0.04), nBins=c(20,30,50) )
featureRanking <- mdply(args, getEntropyFeatureRanking, data=dataAug)
plotFeatureRanking(featureRanking)

featureRankingEI <- getEntropyFeatureRanking(data=dataAug, excludedQuant=0.02, nBins=30)
plotFeatureRankingEI(featureRankingEI)


plotHistOccupancy(allDataRaw)
