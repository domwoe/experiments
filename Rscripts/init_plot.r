#!/usr/bin/env Rscript

plotLossMatrixBEST <- function(lossMat, filename) {
	pdff(file=filename, width=7, height=7)
	lM <- melt(lossMat, id.vars=c("dataset",".id","model","loc_id","sensorFeat") )
	#plt <- ggplot(data = lM[regexpr("always", lM$model) == -1, ],
	plt <- ggplot(data = lM,
			aes(y=value, x=variable, fill=variable))
	plt <- plt + geom_bar(stat="identity", width=0.5, position = position_dodge(width = 0.61), 				color="black", size=0.2)
	plt <- plt + facet_grid(model~sensorFeat+loc_id)
	plt <- plt + guides(fill="none")
	plt <- plt + theme_bw()
	plt <- plt + theme(legend.position = "top", axis.title.y = element_blank(),
			axis.title.x = element_blank(),
			strip.text.x = element_text(size=6),
			strip.text.y = element_text(size=6),
			axis.text.x = element_text(size=8, hjust=1, vjust=0.5, angle=90))
	print(plt)
	dev.off()
}

plotLossMatrix <- function(lossMat, filename) {
	pdff(file=filename, width=7, height=7)
	lM <- melt(lossMat, id.vars=c("dataset",".id","model","loc_id","sensorFeat") )
	d_ply(lM, "model", function(lMpart) {
		#plt <- ggplot(data = lM[regexpr("always", lM$model) == -1, ],
		plt <- ggplot(data = lMpart,
				aes(y=value, x=variable, fill=variable))
		plt <- plt + geom_bar(stat="identity", width=0.5, position = position_dodge(width = 0.61), 				color="black", size=0.2)
		plt <- plt + facet_grid(loc_id+model~sensorFeat)
		plt <- plt + guides(fill="none")
		plt <- plt + theme_bw()
		plt <- plt + theme(legend.position = "top", axis.title.y = element_blank(),
				axis.title.x = element_blank(),
				strip.text.x = element_text(size=6),
				strip.text.y = element_text(size=6),
				axis.text.x = element_text(size=8, hjust=1, vjust=0.5, angle=90))
		print(plt)
	})
	dev.off()
}

plotPrinCompAnalysis <- function(princompAnalysis) {
	pdff(file="prinComp.pdf", width=7, height=5)

	#tmp <- princompAnalysis[princompAnalysis$loc_id %in% c(60,61,62), ]
	#pVar <- setNames(tmp[tmp$feature == "ProportionOfVar", c("value", "loc_id", "variable")],
				#c("pVar", "loc_id", "variable"))
	#tmp <- merge(tmp[tmp$feature != "ProportionOfVar", ], pVar)

	princompAnalysis <- princompAnalysis[princompAnalysis$loc_id %in% c(60,61,62), ]
	plt <- ggplot(data = princompAnalysis[princompAnalysis$feature != "ProportionOfVar",],
			aes(y=value, x=feature, fill=feature))
	plt <- plt + geom_bar(stat="identity", color="black")
	plt <- plt + geom_text(data = princompAnalysis[princompAnalysis$feature == "ProportionOfVar",],
				mapping = aes(label=value), x=1, y=-0.8, size=2)
	plt <- plt + facet_grid(loc_id~variable)
	plt <- plt + guides(fill="none")
	plt <- plt + scale_y_continuous(limits=c(-1,1))
	plt <- plt + theme(legend.position = "top", axis.title.x = element_blank(),
			axis.text.x = element_text(hjust=1, vjust=0.5, angle=90))
	print(plt)
	dev.off()
}

plotFeatureRanking <- function(featureRanking) {
	pdff(file="featRank.pdf", width=8, height=6)
	plt <- ggplot(data = featureRanking[featureRanking$loc_id %in% c(60,61,62), ],
			aes(y=relMutualInfo, x=sensor, fill=sensor))
	plt <- plt + geom_bar(stat="identity", color="black")
	plt <- plt + facet_grid(loc_id~excludedQuant+nBins)
	plt <- plt + guides(fill="none")
	plt <- plt + theme(legend.position = "top", axis.title.x = element_blank(),
			axis.text.x = element_text(hjust=1, vjust=0.5, angle=90))
	print(plt)
	dev.off()
}

plotCorrelationMatrix <- function(corrdf, features) {
	pdff(file="corrMat.pdf", width=5, height=5)
	dat <- melt(corrdf[corrdf$loc_id %in% c(60,61,62), ], id.vars=c("loc_id", "feature") )
	dat$variable <- factor(as.character(dat$variable), features)
	dat$feature <- factor(as.character(dat$feature), rev(features))
	#print(str(dat))
	plt <- ggplot(data = dat, aes(y=feature, x=variable, fill=value))
	plt <- plt + geom_raster()
	plt <- plt + facet_grid(loc_id~.)
	plt <- plt + scale_fill_gradient2(low="red", mid="yellow", high="blue", limits=c(-1,1))
	plt <- plt + theme(legend.position = "right", axis.title.x = element_blank(), axis.title.y = element_blank(),
			axis.text.x = element_text(hjust=1, vjust=0.5, angle=90))
	print(plt)
	dev.off()
}

plotSingleDay <- function(prediction, rawData, startD, endD) {
	prediction$time <- as.POSIXct(prediction$timestamp, origin="1970-01-01")

	startDp <- as.POSIXct(startD, origin="1970-01-01")
	endDp <- as.POSIXct(endD, origin="1970-01-01") 
	
	prediction <- prediction[prediction$time >= startDp & prediction$time <= endDp, ]

	presenceLayer <- rawData[rawData$unittypename=="presence", ]
	presenceLayer$time1 <- as.POSIXct(presenceLayer$timestamp, origin="1970-01-01")
	presenceLayer$time2 <- as.POSIXct(presenceLayer$timestamp2, origin="1970-01-01")

	presenceLayer <- presenceLayer[(presenceLayer$time1 >= startDp & presenceLayer$time1 <= endDp) | (presenceLayer$time2 >= startDp & presenceLayer$time2 <= endDp), ]
	presenceLayer[presenceLayer$time1 < startDp, "time1"] <- startDp
	presenceLayer[presenceLayer$time2 > endDp, "time2"] <- endDp

	plt <- ggplot(data = prediction)
	plt <- plt + geom_rect(data=presenceLayer, mapping = aes(xmin = time1, xmax = time2, ymin = 0, ymax = reading), fill="red", color=NA, alpha=0.7)
	plt <- plt + geom_step(data = prediction, mapping=aes(x=time, y=presence), color="blue", size=0.8)
	plt <- plt + scale_x_datetime(labels = date_format("%H:%M"), expand = c(0.005, 0), limits = c(startDp, endDp) )
	plt <- plt + scale_y_continuous(breaks = c(0,1), labels = c("Unoccupied", "Occupied"), expand=c(0.0, 0.05) )
	plt <- plt + theme_bw()
	plt <- plt + theme(legend.position = "top", legend.title=element_blank(), panel.margin = unit(0.3, "cm"), strip.text.y = element_text(size = 8), axis.title.y = element_blank(), axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), panel.border = element_blank(), axis.ticks.length = unit(0.25, "cm"))
	#plt <- plt + guides(color="none")
	plt <- plt + labs(x = "", y = "")
	print(plt)

}

plotSingleDayPaperAntjeBad <- function(dataPerRoomValidationAug, predictionPerRoomValidation, dataPerRoomValidation, columnsToPlot) {
	pdff(file="singleDay_Antje_Bad.pdf", width=11.2, height=1.4)
	plotSingleDay(predictionPerRoomValidation[["60"]],
			dataPerRoomValidation[["60"]],
			"2014-02-10 00:00:00",
			"2014-02-11 00:00:00" )
	plotSingleDay(predictionPerRoomValidation[["61"]],
			dataPerRoomValidation[["61"]],
			"2014-02-10 00:00:00",
			"2014-02-11 00:00:00" )
	dev.off()
}

plotScatterPlotContacts <- function(rasterDataPerRoom) {
	filename <- paste("scatterPlotContacts.pdf", sep="")
	pdff(file=filename, width=7, height=5)
	lapply(rasterDataPerRoom, function(df) {
		cols <- intersect(colnames(df), c("presence", "windowcontact", "doorcontact"))
		if(length(cols) > 2) { # we have a window and door contact sensor
			ddf <- df[, c("timestamp", "co2", "co2deriv", "tempDeriv", "temperature", cols)]
			ddf <- modifyList(ddf, lapply(ddf[, c("presence", "windowcontact", "doorcontact")], function(x) as.factor(as.character(x)) ))
			#print(str(ddf))
			#plt <- ggplot(data=ddf, mapping=aes(x=sqrt(co2^2 + co2deriv^2), y=atan2(co2deriv,co2) ))
			plt <- ggplot(data=ddf, mapping=aes(x=co2deriv, y=co2))
			plt <- plt + geom_point(mapping=aes(order=windowcontact, fill=windowcontact, color=windowcontact, shape=windowcontact))
			plt <- plt + scale_shape_manual(values=c(21,24))
			#plt <- plt + scale_color_manual(values=c("black","yellow"))
			plt <- plt + scale_fill_manual(values=c("red", "blue"))
			#plt <- plt + scale_fill_gradient(low="red", high="blue")
			#plt <- plt + scale_fill_gradient2(low="blue", high="red")
			print(plt)
		}
	})
	dev.off()
}

plotLossDay <- function(df, filename="metricDay.pdf") {
	mlt <- melt(df, measure.vars=c("validation", "training"))
	print(head(mlt))
	pdff(file=filename, width=9, height=9)
	plt <- ggplot(data = mlt, aes(y=value, x=variable, fill=metric))
	plt <- plt + geom_bar(stat="identity", width=0.5, position = position_dodge(width = 0.61), color="black", size=0.2)
	plt <- plt + facet_grid(tDay ~ loc_id, scales="free_x")
	#plt <- plt + facet_grid(.~variable, scales="free_x")
	plt <- plt + theme_bw()
	plt <- plt + theme(axis.ticks.x=element_blank(), legend.position = "top", legend.title=element_blank())
	plt <- plt + labs(y="Metric Score", x="Locations")
	plt <- plt + scale_y_continuous(expand = c(0,0.01), limits = c(0,1.0))
	print(plt)
	dev.off()
}

plotLossPaper <- function(lpR, filename, title="Loss") {
	loc_ids <- c(11,12,16,14,47,45,46,44,60,62,61)
	ids <- c(1,2,3,4,5,6,7,8,9,10,11)
	names(loc_ids) <- as.character(ids)
	lpR <- as.data.frame(lpR)
	lpR$metric <- factor(rownames(lpR))
	colnames(lpR) <- sapply(colnames(lpR), function(n) {
		if(n %in% loc_ids) {
			return(names(loc_ids)[loc_ids==n])
		} else {
			return(n)
		}
	})
	lpR <- melt(lpR, id.vars=c("metric"))
	lpR$line <- as.numeric(as.numeric(as.character(lpR$variable)) >= 6)
	lpR$variable <- as.numeric(as.character(lpR$variable))

	pdff(file=filename, width=7, height=6.3)
	plt <- ggplot(data = lpR, aes(y=value, x=variable, fill=metric))
	plt <- plt + geom_bar(stat="identity", color="black", size=0.15, width=0.3, position = position_dodge(width = 0.42))
	plt <- plt + facet_wrap(~line, scales="free_x", ncol=1)
	plt <- plt + theme_bw()
	plt <- plt + theme(axis.ticks=element_blank(), legend.position = "top", legend.title=element_blank(), strip.text = element_blank(), strip.background = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.background = element_blank(), panel.border = element_blank(), axis.line = element_blank(), panel.grid.major.y = element_line(colour = "darkgray"), panel.margin=unit(0.9, "cm"), axis.title.y = element_text(vjust=0.25), axis.title.x = element_text(vjust=-0.5) )
	plt <- plt + labs(y="Metric Score", x="Room Id")
	plt <- plt + scale_y_continuous(expand = c(0,0), limits = c(0,1.0))
	#plt <- plt + scale_x_continuous()
	plt <- plt + scale_x_continuous(breaks = ids)
	print(plt)
	dev.off()
}

plotLoss <- function(lpR, title="Loss", filename="binLossRate.pdf") {
	cat(paste(title, "\n"))
	lpR <- as.data.frame(lpR)
	lpR$measure <- factor(rownames(lpR))
	print(lpR)
	lpR <- melt(lpR, id.vars=c("measure"))
	lpR$variable <- as.factor(as.character(lpR$variable))
	#print(lpR)
	pdff(file=filename, width=7, height=5)
	plt <- ggplot(data = lpR, aes(y=value, x=variable, fill=measure))
	plt <- plt + geom_bar(stat="identity", width=0.5, position = position_dodge(width = 0.61), color="black", size=0.2)
	#plt <- plt + facet_grid(.~variable, scales="free_x")
	plt <- plt + theme_bw()
	plt <- plt + theme(axis.ticks.x=element_blank(), legend.position = "top", legend.title=element_blank())
	plt <- plt + labs(y="Metric Score", x="Locations")
	plt <- plt + scale_y_continuous(expand = c(0,0.01), limits = c(0,1.0))
	print(plt)
	dev.off()
}

plotSensorHistogramsAntje <- function(ldf, columnsToPlot) {
	df <- rbind.fill(lapply(names(ldf), function(n) data.frame(loc_id=n, ldf[[n]]) )) #reassemble data frame
	df$presence <- factor(as.character(df$presence))
	#print(df[df$presence!=1 & df$presence!=0, ])
	stopifnot(all(df$presence==1 | df$presence==0))
	mlt <- melt(df, id.vars=c("loc_id", "presence"), measure.vars=columnsToPlot)
	num_bins <- 25
	#histD <- ddply(mlt, .(variable), function(x) {
	#	rg <- range(x$value)
	#	incr <- abs(rg[2]-rg[1])/num_bins
	#	b <- seq(from=rg[1], to=rg[2], by=incr)
	#	pHistD <- ddply(x, .(loc_id, presence), function(z) {
	#		h <- hist(z$value, breaks=b, plot=FALSE)
	#		ret <- data.frame(dens=(h$count/sum(h$count)),
	#			  mids=h$mids,
	#			  incr=incr)
	#		return(ret)
	#	})
	#})
	histD <- ddply(mlt, .(variable,loc_id), function(x) {
		rg <- range(x$value)
		incr <- abs(rg[2]-rg[1])/num_bins
		b <- seq(from=rg[1], to=rg[2], by=incr)
		pHistD <- ddply(x, .(presence), function(z) {
			h <- hist(z$value, breaks=b, plot=FALSE)
			ret <- data.frame(dens=(h$count/sum(h$count)),
				  mids=h$mids,
				  incr=incr)
			return(ret)
		})
	})
	#print(head(mlt))
	histD <- histD[histD$loc_id==60, ]
	filename <- paste("hist_sensorDataAntje.pdf", sep="")
	units <- setNames(c("Parts per million (ppm)", "Change in concentration per second", "Change in concentration per second²", "Degrees celsius", "Relative humidity in %"),
			  c("co2", "co2deriv", "co2deriv2", "temperature", "humidity")	)
	pdff(file=filename, width=3, height=1.5)
	lapply(dlply(histD, .(variable)), function (mltp) {
		var <- unique(mltp$variable)
		unitx <- units[names(units)==var]
		plt <- ggplot(data=mltp) + geom_bar(aes(fill=presence, x=mids, y=dens, width=incr*0.75), position="dodge", stat="identity") + theme_bw() +labs(x=unitx, y="Fraction") #+ scale_y_log10()
		plt <- plt + theme(legend.position = "bottom", axis.title = element_text(size=7), axis.text = element_text(size=6), axis.ticks = element_line(size=0.2), axis.title.y = element_text(vjust=0.2), axis.title.x = element_text(vjust=0.15), axis.ticks.length = unit(0.1, "cm"), panel.border = element_rect(fill = NULL, colour = "black", size = 0.2) )
		plt <- plt + guides(fill=FALSE)
		print(plt)
	})
	dev.off()
}

plotSensorHistograms <- function(ldf, columnsToPlot) {
	df <- rbind.fill(lapply(names(ldf), function(n) data.frame(loc_id=n, ldf[[n]]) )) #reassemble data frame
	df$presence <- factor(as.character(df$presence))
	#print(df[df$presence!=1 & df$presence!=0, ])
	stopifnot(all(df$presence==1 | df$presence==0))
	mlt <- melt(df, id.vars=c("loc_id", "presence"), measure.vars=columnsToPlot)
	num_bins <- 25
	#histD <- ddply(mlt, .(variable), function(x) {
	#	rg <- range(x$value)
	#	incr <- abs(rg[2]-rg[1])/num_bins
	#	b <- seq(from=rg[1], to=rg[2], by=incr)
	#	pHistD <- ddply(x, .(loc_id, presence), function(z) {
	#		h <- hist(z$value, breaks=b, plot=FALSE)
	#		ret <- data.frame(dens=(h$count/sum(h$count)),
	#			  mids=h$mids,
	#			  incr=incr)
	#		return(ret)
	#	})
	#})
	histD <- ddply(mlt, .(variable,loc_id), function(x) {
		rg <- range(x$value)
		incr <- abs(rg[2]-rg[1])/num_bins
		b <- seq(from=rg[1], to=rg[2], by=incr)
		pHistD <- ddply(x, .(presence), function(z) {
			h <- hist(z$value, breaks=b, plot=FALSE)
			ret <- data.frame(dens=(h$count/sum(h$count)),
				  mids=h$mids,
				  incr=incr)
			return(ret)
		})
	})
	#print(head(mlt))
	filename <- paste("hist_sensorData.pdf", sep="")
	pdff(file=filename, width=6, height=7)
	lapply(dlply(histD, .(variable)), function (mltp) {
		var <- unique(mltp$variable)
		plt <- ggplot(data=mltp) + geom_bar(aes(fill=presence, x=mids, y=dens, width=incr*0.75), position="dodge", stat="identity") + facet_wrap(~loc_id, scales="free", ncol=2) + theme_bw() +labs(x="Value", y="Fraction", title=var) #+ scale_y_log10()
		plt <- plt + theme(axis.title.x=element_blank(), legend.position = "bottom")
		print(plt)
	})
	dev.off()
}

plotHistOccupancy <- function(df) {
	presenceData <- df[df$unittypename == "presence" & df$reading == 1, ]
	#print(head(presenceData))
	presenceData$durationInSec <- (presenceData$timestamp2 - presenceData$timestamp)
	presenceData$durationInHours <- presenceData$durationInSec / 3600
	#presenceData$durationInSec <- as.POSIXct(presenceData$durationInSec, origin="1970-01-01")
	#print(head(presenceData))
	num_bins <- 16
	histData <- ddply(presenceData, .(loc_id), function(x) {
		m <- max(x$durationInHours)
		incr <- m/num_bins
		b <- seq(from=0, to=m, by=incr)
		h <- hist(x$durationInHours, breaks=b, plot=FALSE)
		ret <- data.frame(dens=(h$count/sum(h$count)),
				  mids=h$mids,
				  incr=incr,
				  loc_displayname=unique(x$loc_displayname))
		return(ret)
	})
	#hls <- lapply(dlply(presenceData, .(loc_id)), function(x) {
	#  rng <- max(x$durationInHours)
	#  bwidth <- rng/num_bins
	#  geom_histogram(data = x, aes(y=..count../sum(..count..)), binwidth=bwidth)
	#})
	filename <- paste("hist_occupancy.pdf", sep="")
	pdff(file=filename, width=7, height=9)
	plt <- ggplot(data=histData, aes(x=mids, y=dens, width=incr))
	plt <- plt + geom_bar(stat="identity", position="dodge")
	plt <- plt + facet_wrap(~loc_id+loc_displayname, scales="free", ncol = 2) + theme_bw()
	plt <- plt + labs(x="duration of occupied time interval [h]", y="Fraction of occupancy times per histogram bin", title="Occupancy histogram")
	#plt <- plt + scale_y_log10()
	#plt <- ggplot(data=presenceData, aes(x=durationInHours))
	#plt <- plt + hls
	#plt <- plt + facet_wrap(~loc_id+loc_displayname, scales="free", ncol = 2) + theme_bw() +labs(x="duration of occupied time interval [h]", y="Fraction of occupancy times", title="Occupancy histogram") #+ scale_x_datetime(labels = date_format("%H:%M"))
	#plt <- plt #+ scale_y_log10()
	print(plt)
	dev.off()
}

plotHistOccupancyPaper <- function(df) {
	loc_ids <- c(11,12,16,14,47,45,46,44,60,62,61)
	ids <- c(1,2,3,4,5,6,7,8,9,10,11)
	names(loc_ids) <- as.character(ids)
	df$loc_id <- sapply(df$loc_id, function(n) {
		if(n %in% loc_ids) {
			return(names(loc_ids)[loc_ids==n])
		} else {
			return(n)
		}
	})
	presenceData <- df[df$unittypename == "presence" & df$reading == 1, ]
	presenceData$durationInSec <- (presenceData$timestamp2 - presenceData$timestamp)
	presenceData$durationInHours <- presenceData$durationInSec / 3600

	pData <- ddply(presenceData, .(loc_id), function(x) c(median=median(x$durationInHours), mean=mean(x$durationInHours)) )
	pData <- melt(pData, id.vars=c("loc_id"))
	pData$loc_id <- as.numeric(as.character(pData$loc_id))
	filename <- paste("occupancy_bar_chart.pdf", sep="")
	pdff(file=filename, width=7, height=5)
	plt <- ggplot(data = pData, aes(y=value, x=loc_id, fill=variable))
	plt <- plt + geom_bar(stat="identity", color="black", size=0.15, width=0.3, position = position_dodge(width = 0.42))
	plt <- plt + theme_bw()
	plt <- plt + theme(axis.ticks=element_blank(), legend.position = "top", legend.title=element_blank(), strip.text = element_blank(), strip.background = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.background = element_blank(), panel.border = element_blank(), axis.line = element_blank(), panel.grid.major.y = element_line(colour = "darkgray"), panel.margin=unit(0.9, "cm"), axis.title.y = element_text(vjust=0.25), axis.title.x = element_text(vjust=-0.5) )
	plt <- plt + labs(y="Occupancy duration [h]", x="Room Id")
	plt <- plt + scale_x_continuous(breaks = ids)
	plt <- plt + scale_y_continuous(expand = c(0,0))
	print(plt)
	dev.off()
}


plotSensorDataTable <- function(rD, nm, pD, dD, columnsToPlot, prefix) {
	loc_id <- unique(dD$loc_id)
	stopifnot(length(loc_id)==1)
	stopifnot(loc_id == nm)
	loc_displayname <- unique(dD$loc_displayname)
	filename <- paste(prefix, "_", nm, ".pdf", sep="")
	pdff(file=filename, width=8, height=4)
	rD$prediction <- pD$presence
	mlt <- melt(rD, id.vars=c("timestamp"), measure.vars=c(columnsToPlot, "prediction"))
	mlt$time <- as.POSIXct(mlt$timestamp, origin="1970-01-01")
	mlt$timestamp <- NULL

	startTimestamp <- min(mlt$time)
	stopTimestamp <- max(mlt$time)

	presenceLayer <- dD[dD$unittypename=="presence", ]
	presenceLayer <- rbind.fill(lapply(c("prediction", "presence"), function(x) data.frame(presenceLayer, variable=x)))
	presenceLayer$time1 <- as.POSIXct(presenceLayer$timestamp, origin="1970-01-01")
	presenceLayer$time2 <- as.POSIXct(presenceLayer$timestamp2, origin="1970-01-01")

	startDateDay <- as.POSIXlt(startTimestamp, origin="1970-01-01")
	#startDateDay$mday <- startDateDay$mday -1
	startDateDay$hour = 0

	endDateDay <- startDateDay
	endDateDay$mday <- endDateDay$mday + 1
	#endDateDay$hour <- endDateDay$hour + 4

	while(stopTimestamp > startDateDay)
	{
		#mltPart <- mlt[mlt$time >= startDateDay & mlt$time <= endDateDay, ]
		plt <- ggplot(data = mlt)
		plt <- plt + geom_rect(data=presenceLayer, mapping = aes(xmin = time1, xmax = time2, ymin = -Inf, ymax = Inf*((reading*2)-1)), fill="green", color=NA, alpha=0.6)
		plt <- plt + geom_step(data = mlt, mapping=aes(x=time, y=value), color="brown", size=0.35)
		plt <- plt + facet_grid(variable~.,scales="free_y")
		plt <- plt + scale_x_datetime(labels = date_format("%d/%m %H:%M"), expand = c(0,0), limits = c(as.POSIXct(startDateDay), as.POSIXct(endDateDay)))
		plt <- plt + theme_bw()
		plt <- plt + theme(legend.position = "top", legend.title=element_blank(), panel.margin = unit(0.3, "cm"), strip.text.y = element_text(size = 8), axis.title.y = element_text(vjust=0.25), axis.title.x = element_blank())
		plt <- plt + guides(color="none")
		plotDate <- startDateDay
		plotDate$hour <- plotDate$hour + 6
		plt <- plt + labs(title = paste(loc_displayname, ":", nm, ":", format(plotDate, "%Y-%m-%d")), x = "", y = "Value")
		print(plt)

		startDateDay$mday <- startDateDay$mday + 1
		endDateDay$mday <- endDateDay$mday + 1
	}

	dev.off()
}

plotLocation <- function(df, filePrefix) {
	loc_id <- unique(df$loc_id)
	stopifnot(length(loc_id)==1)
	loc_displayname <- unique(df$loc_displayname)

	df$time1 <- as.POSIXct(df$timestamp, origin="1970-01-01")
	df$time2 <- as.POSIXct(df$timestamp2, origin="1970-01-01")

	presenceFields <- c("presence", "doorcontact", "windowcontact")
	contactFields <- c("doorcontact", "windowcontact")
	sensorFields <- setdiff(unique(df$unittypename), presenceFields)
	#print(sensorFields)

	presenceOverlay <- df[df$unittypename == "presence", ]
	colnames(presenceOverlay)[colnames(presenceOverlay)=="unittypename"] <- "presencetype"
	presenceOverlay <- rbind.fill(lapply(sensorFields, function(x) data.frame(presenceOverlay, unittypename=x)))

	#print(head(presenceOverlay))

	contactsOverlay <- df[df$unittypename %in% contactFields, ]
	colnames(contactsOverlay)[colnames(contactsOverlay)=="unittypename"] <- "contacttype"
	contactsOverlay$unittypename <- rep("contacts", nrow(contactsOverlay))

	sensorLayer <- df[df$unittypename %in% sensorFields, ] #we only need sensor fields now

	startTimestamp <- min(df$time1)
	stopTimestamp <- max(df$time1)

	startDateDay <- as.POSIXlt(startTimestamp, origin="1970-01-01")
	startDateDay$mday <- startDateDay$mday -1
	startDateDay$hour = 22

	endDateDay <- startDateDay
	endDateDay$mday <- endDateDay$mday + 1
	endDateDay$hour <- endDateDay$hour + 4

	filename <- paste(filePrefix, "_", loc_id, ".pdf", sep="")
	pdff(file=filename, width=8, height=4)

	while(stopTimestamp > startDateDay)
	{
		plt <- ggplot()
		plt <- plt + geom_rect(data = presenceOverlay, mapping = aes(xmin = time1, xmax = time2, ymin = -Inf, ymax = Inf*((reading*2)-1), fill=presencetype), color=NA, alpha=0.9)
		if(nrow(contactsOverlay)>0) {
			plt <- plt + geom_rect(data = contactsOverlay, mapping = aes(xmin = time1, xmax = time2, fill=contacttype, ymin = -Inf, ymax = Inf*((reading*2)-1), color=contacttype) )
			dummy <- data.frame(time1 = as.POSIXct(startDateDay), time2 = as.POSIXct(endDateDay), unittypename="contacts")
			plt <- plt + geom_rect(data = dummy, mapping = aes(xmin = time1, xmax = time2, ymin = 0, ymax = 1), fill=NA, color=NA, alpha=0)
		}
		colorz <-  c("presence" = "green", "doorcontact"="red", "windowcontact"="blue", "co2"="black", "temperature"="brown", "humidity"="darkgreen")
		plt <- plt + scale_fill_manual(values = colorz)
		plt <- plt + scale_color_manual(values = colorz)		
		plt <- plt + geom_step(data = sensorLayer, aes(x = time1, y = reading, color=unittypename), size=0.8)
		plt <- plt + facet_grid(unittypename~.,scales="free_y")
		plt <- plt + guides(color="none")
		plt <- plt + scale_x_datetime(labels = date_format("%d/%m %H:%M"), expand = c(0,0), limits = c(as.POSIXct(startDateDay), as.POSIXct(endDateDay)))
		plt <- plt + theme_bw()
		plt <- plt + theme(legend.position = "top", legend.title=element_blank(), panel.margin = unit(0.3, "cm"), strip.text.y = element_text(size = 8), axis.title.y = element_text(vjust=0.25), axis.title.x = element_blank())
		#axis.title.x = element_text(vjust=-0.05))

		plotDate <- startDateDay
		plotDate$hour <- plotDate$hour + 6
		plt <- plt + labs(title = paste(loc_displayname, ":", format(plotDate, "%Y-%m-%d")), x = "", y = "Value")
		print(plt)

		startDateDay$mday <- startDateDay$mday + 1
		endDateDay$mday <- endDateDay$mday + 1
	}
	dev.off()	
}


plotTSforRC <- function(prefix, location_id, modelName, sensorFeat, dataAug, dataRaw, prediction) {
	if(modelName %in% c("alwaysOccupied", "alwaysUnoccupied")) return(NULL)
	if(is.null(prediction) || length(prediction) < 1) return(NULL)

	dataAug$prediction <- prediction
	#print(head(dataAug))
	#print(head(dataRaw))
	loc_displayname <- unique(dataRaw$loc_displayname)
	filename <- paste(prefix, "_", location_id, "_", modelName, "_", paste(sensorFeat, collapse="-"), ".pdf", sep="")

	pdff(file=filename, width=8, height=4)

	mlt <- melt(dataAug, id.vars=c("timestamp"), measure.vars=c(sensorFeat, "prediction"))
	mlt$time <- as.POSIXct(mlt$timestamp, origin="1970-01-01")
	mlt$timestamp <- NULL

	#print(head(mlt))

	startTimestamp <- min(mlt$time)
	stopTimestamp <- max(mlt$time)

	presenceLayer <- dataRaw[dataRaw$unittypename=="presence", ]
	presenceLayer <- rbind.fill(lapply(c("prediction"), function(x) data.frame(presenceLayer, variable=x)))
	presenceLayer$time1 <- as.POSIXct(presenceLayer$timestamp, origin="1970-01-01")
	presenceLayer$time2 <- as.POSIXct(presenceLayer$timestamp2, origin="1970-01-01")

	startDateDay <- as.POSIXlt(startTimestamp, origin="1970-01-01")
	#startDateDay$mday <- startDateDay$mday -1
	startDateDay$hour = 0

	endDateDay <- startDateDay
	endDateDay$mday <- endDateDay$mday + 1
	#endDateDay$hour <- endDateDay$hour + 4

	while(stopTimestamp > startDateDay)
	{
		#mltPart <- mlt[mlt$time >= startDateDay & mlt$time <= endDateDay, ]
		plt <- ggplot(data = mlt)
		plt <- plt + geom_rect(data=presenceLayer, mapping = aes(xmin = time1, xmax = time2, ymin = -Inf, ymax = Inf*((reading*2)-1)), fill="green", color=NA, alpha=0.6)
		plt <- plt + geom_step(data = mlt, mapping=aes(x=time, y=value), color="brown", size=0.35)
		plt <- plt + facet_grid(variable~.,scales="free_y")
		plt <- plt + scale_x_datetime(labels = date_format("%d/%m %H:%M"), expand = c(0,0), limits = c(as.POSIXct(startDateDay), as.POSIXct(endDateDay)))
		plt <- plt + theme_bw()
		plt <- plt + theme(legend.position = "top", legend.title=element_blank(), panel.margin = unit(0.3, "cm"), strip.text.y = element_text(size = 8), axis.title.y = element_text(vjust=0.25), axis.title.x = element_blank())
		plt <- plt + guides(color="none")
		plotDate <- startDateDay
		plotDate$hour <- plotDate$hour + 6
		plt <- plt + labs(title = paste(loc_displayname, ":", location_id, ":", format(plotDate, "%Y-%m-%d")), x = "", y = "Value")
		print(plt)

		startDateDay$mday <- startDateDay$mday + 1
		endDateDay$mday <- endDateDay$mday + 1
	}

	dev.off()
}

