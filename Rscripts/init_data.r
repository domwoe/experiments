#!/usr/bin/env Rscript

firstDerivSecondOrderAccCentral <- function(vec) {
	# constant boundaries
	l <- length(vec)
	stopifnot(l >= 3)
	v <- -0.5*vec[c(1,1:(l-2),(l-2))] +  0.5*vec[c(3,3:l,l)]
	#v <- log(-v)
	stopifnot(length(v) == l)
	return(v)
}

derivSpline <- function(timestamps, vec, deriv=1) {
	#splineF <- sm.spline(timestamps, vec)
	splineF <- smooth.Pspline(x=timestamps, y=vec, norder=3, method=3) # we need order 3 according to descr
	newV <- predict(splineF, xarg=timestamps, nderiv = deriv)
	return(newV)
}

#interpolateAccordingToSensorID <- function(dbResult, startT, stopT, deltaT) {
#	interpolatedValues <- ddply(dbResult, c("sens_id", "unittypename"), function(df) {
#		timestampVec <- seq(from = startT, to = stopT, by = deltaT) #does not exceed stopTimestamp
#		apprList <- approx(x = df$timestamp, y = df$reading, xout = timestampVec, method="constant", rule = 2, f = 0, ties = mean)
#		res <- data.frame(reading=apprList$y, timestamp=apprList$x)
#	})
#}

#prepareDataForLocation <- function(df, deltaTime) {
#	stopifnot(length(unique(df$loc_id))==1) # data for one single room only
#	stopifnot(any(df$unittypename=="presence")) # must have an occupancy sensor
#	# make sure there's one sensor per unittype
#	ct <- count(df, c("unittypename", "sens_id"))
#	stopifnot(length(unique(ct$unittypename)) == length(unique(ct$sens_id)) ) 
#	startTimestamp <- min(df$timestamp[df$unittypename=="presence"])
#	stopTimestamp <- max(df$timestamp2[df$unittypename=="presence"])
#	sensorDataInterp <- interpolateAccordingToSensorID(df, startTimestamp, stopTimestamp, deltaTime)
#}

compressVec <- function(vec) {
	rtz <- rle(vec)
	pos <- cumsum(rtz$lengths)+1
	pos <- pos[-length(pos)] #remove last element
	pos <- c(1, pos)
	#pos <- unique(c(1,pos,pos+1,length(vec)))
	#pos <- pos[pos >= 0 & pos <= length(vec)]
	return(pos)
}

compressSensor <- function(df, compColumnName, timest1ColName, timest2ColName) {
	if(any(colnames(df) == timest2ColName)) {
		df[, colnames(df) == timest2ColName] <- NULL
	}
	stopifnot(any(colnames(df) == compColumnName))
	stopifnot(any(colnames(df) == timest1ColName))

	colnames(df)[colnames(df)==timest1ColName] <- "timest1" # rename temporarily
	lastRow <- df[nrow(df), ]
	df <- df[compressVec(df[, colnames(df) == compColumnName]), ]
	df$timest2 <- c(df$timest1[c(2:length(df$timest1))], lastRow$timest1)

	colnames(df)[colnames(df)=="timest1"] <- timest1ColName
	colnames(df)[colnames(df)=="timest2"] <- timest2ColName
	return(df)
}

removeConstantPartsFromSensor <- function(df, reading, threshold) {
	minMaxRows <- min(df$timestamp) == df$timestamp |
		      max(df$timestamp) == df$timestamp
	#print(df[minMaxRows, ])
	stopifnot(sum(minMaxRows)==2)
	idVec <- ((df$timestamp2 - df$timestamp) < threshold) & df$reading == reading & !minMaxRows
	df <- df[!idVec, ]
}

# parameters maxUnoccupiedGap and minOccupancyDuration are in seconds. They define the presence smoothing.
prepareDataForLocationTable <- function(df, deltaTime, maxUnoccupiedGap=600, minOccupancyDuration=600) {
	stopifnot(all(df$presence==1 | df$presence==0))
	stopifnot(length(unique(df$loc_id))==1) # data for one single room only
	stopifnot(any(df$unittypename=="presence")) # must have an occupancy sensor
	# make sure there's one sensor per unittype
	ct <- count(df, c("unittypename", "sens_id"))
	stopifnot(length(unique(ct$unittypename)) == length(unique(ct$sens_id)) ) 

	# smooth presence Data and store in as new sensor type
	presenceData <- df[df$unittypename=="presence", ]
	presenceData$unittypename = "smoothedPresence"
	#df <- df[df$unittypename!="presence", ] # do not delete presence data
	presenceData <- removeConstantPartsFromSensor(presenceData, 0, maxUnoccupiedGap)
	presenceData <- compressSensor(presenceData, "reading", "timestamp", "timestamp2")
	presenceData <- removeConstantPartsFromSensor(presenceData, 1, minOccupancyDuration)
	presenceData <- compressSensor(presenceData, "reading", "timestamp", "timestamp2")
	df <- rbind(df, presenceData) # add smoothed presence to data frame
	
	userdefM <- function(vec) {
		if(length(vec) != 1) {
			print(vec)
			stop()
		}
		stopifnot(length(vec)==1)
		vec
	}

	# discretize and raster 
	startTimestamp <- min(df$timestamp[df$unittypename=="presence"])
	stopTimestamp <- max(df$timestamp2[df$unittypename=="presence"])
	#print(c(startTimestamp, stopTimestamp))
	timestampVec <- seq(from = startTimestamp, to = stopTimestamp, by = deltaTime)
	interp <- dlply(df, .(unittypename), function(d) {
		stopifnot(all(d$reading==1 | d$reading==0) | !(unique(d$unittypename)=="presence"))
		#print(range(timestampVec))
		#stopifnot(all(d$timestamp == cummax(d$timestamp))) # make sure timestamp is ascending
		pind <- which(diff(d$timestamp)<=0)
		if(length(pind) > 0) {
			#print(pind)
			spind <- unique(sort((min(pind)-5):(max(pind)+5)))
			#print(spind)
			print(d[spind, ])
		}
		stopifnot(all(diff(d$timestamp)>0))
		apprList <- approx(x = d$timestamp, y = d$reading, xout = timestampVec, method="constant", rule = 2, f = 0, ties = max)
		#if(!all(!is.na(apprList$y)) ) {
		#	print(unique(d$unittypename))
		#	v <- apprList$x[is.na(apprList$y)]
		#	print(v)
		#}
		#stopifnot(all( !is.na(apprList$y) ))
		#print(unique(d$unittypename))
		stopifnot(all(apprList$y==1 | apprList$y==0) | !(unique(d$unittypename)=="presence" | unique(d$unittypename)=="smoothedPresence" ))
		return(apprList$y)
	})
	data.frame(timestamp=timestampVec, interp)
}

dbResult <- data.frame()
cacheFile <- "dbResult.txt"
if(!file.exists(cacheFile))
{
	cat("Fetching data from DB\n")
	## loads the PostgreSQL driver
	drv <- dbDriver("PostgreSQL")
	## Open a connection
	con <- dbConnect(drv, dbname="sensordb", user="joe", password="snags98live", host="213.165.92.187", port=5432)

	## Submits a statement
	##rs <- dbSendQuery(con, "select * from R_Users")
	## fetch all elements from the result set
	##fetch(rs,n=-1)

	options("scipen"=100)

	## Submit and execute the query
	dbResult <- dbGetQuery(con, paste("
	SELECT * FROM presence_co2;
		", sep=""))
	## Closes the connection
	dbDisconnect(con)
	## Frees all the resources on the driver
	dbUnloadDriver(drv)

	# remove early entries
	observationStart <- as.POSIXct("2014-01-20 22:00", origin="1970-01-01")
	dbResult <- dbResult[dbResult$timestamp > as.numeric(observationStart)*1000, ]


	# we have to subtract one hour from all presence indicator readings:
	# presence, doorcontact, windowcontact
	#dbResult[dbResult$unittypename %in% presenceFields, "timestamp"] <- dbResult[dbResult$unittypename %in% presenceFields, "timestamp"] - 1000*60*60 # subtract one hour # done on DB

	presenceVal <- dbResult$reading[dbResult$unittypename %in% c("doorcontact", "windowcontact", "presence")]
	stopifnot(all(presenceVal==1 | presenceVal==0))
	cat("check passed\n")

	dbResult$timestamp <- dbResult$timestamp %/% 1000 # convert to seconds

	# we add another column (timestamp2) to the data frame.
	# this allows us to plot rectangles/intervals later on
	ddplyCols <- c("sens_id")
	dbResult <- ddply(dbResult, ddplyCols, function(df) {
		#sort and select columns
		tmp <- df[with(df, order(timestamp)), setdiff(colnames(dbResult),ddplyCols)] 
		if(nrow(tmp) > 0) {
			tmp <- compressSensor(tmp, "reading", "timestamp", "timestamp2")
		}
		return(tmp)
	})

	#cache locally
	write.table(dbResult, file=cacheFile, sep="\t")
} else {
	cat("Loading data from local file\n")
	dbResult <- read.table(file=cacheFile, header=TRUE, sep="\t")
}
# transform timestamps to actual times / add one column each
dbResult$time1 <- as.POSIXct(dbResult$timestamp, origin="1970-01-01")
dbResult$time2 <- as.POSIXct(dbResult$timestamp2, origin="1970-01-01")












