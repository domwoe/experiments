#!/usr/bin/env Rscript


plotScatterPlotContacts <- function(rasterDataPerRoom) {
	filename <- paste("scatterPlotContacts.pdf", sep="")
	pdf(file=filename, width=7, height=5)
	print(filename)
	lapply(rasterDataPerRoom, function(df) {
		cols <- intersect(colnames(df), c("presence", "windowcontact", "doorcontact"))
		if(length(cols) > 2) { # we have a window and door contact sensor
			ddf <- df[, c("timestamp", "co2", "co2deriv", cols)]
			ddf <- modifyList(ddf, lapply(ddf[, c("presence", "windowcontact", "doorcontact")], as.factor))
			print(str(ddf))
			#plt <- ggplot(data=ddf, mapping=aes(x=sqrt(co2^2 + co2deriv^2), y=atan2(co2deriv,co2) ))
			plt <- ggplot(data=ddf, mapping=aes(x=co2, y=co2deriv))
			plt <- plt + geom_point(mapping=aes(order=windowcontact, fill=windowcontact, color=windowcontact, shape=presence))
			plt <- plt + scale_shape_manual(values=c(21,24))
			plt <- plt + scale_color_manual(values=c("red","blue"))
			plt <- plt + scale_fill_manual(values=c("red", "blue"))
			print(plt)
		}
	})
	dev.off()
}

plotLoss <- function(lpR) {
	lpR <- melt(lpR, id.vars=c("measure"))
	lpR$variable <- factor(lpR$variable)
	#print(lpR)
	filename <- paste("binLossRate.pdf", sep="")
	pdf(file=filename, width=7, height=5)
	print(filename)
	plt <- ggplot(data = lpR, aes(y=value, x=variable, fill=measure))
	plt <- plt + geom_bar(stat="identity", width=0.5, position = position_dodge(width = 0.61), color="black", size=0.2)
	#plt <- plt + facet_grid(.~variable, scales="free_x")
	plt <- plt + theme_bw()
	plt <- plt + theme(axis.ticks.x=element_blank(), legend.position = "top", legend.title=element_blank())
	plt <- plt + labs(y="Binary error rate", x="Locations")
	plt <- plt + scale_y_continuous(expand = c(0,0.01), limits = c(0,1.0))
	print(plt)
	dev.off()
}

plotSensorHistograms <- function(ldf, columnsToPlot) {
	df <- rbind.fill(lapply(names(ldf), function(n) data.frame(loc_id=n, ldf[[n]]) )) #reassemble data frame
	df$presence <- factor(df$presence)
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
	pdf(file=filename, width=6, height=7)
	print(filename)
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
	print(filename)
	pdf(file=filename, width=7, height=9)
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

plotSensorDataTable <- function(rD, nm, pD, dD, columnsToPlot) {
	loc_id <- unique(dD$loc_id)
	stopifnot(length(loc_id)==1)
	stopifnot(loc_id == nm)
	loc_displayname <- unique(dD$loc_displayname)
	filename <- paste("locId_", nm, ".pdf", sep="")
	print(filename)
	pdf(file=filename, width=8, height=4)
	rD$prediction <- pD$presence
	mlt <- melt(rD, id.vars=c("timestamp"), measure.vars=c(columnsToPlot, "prediction"))
	mlt$time <- as.POSIXct(mlt$timestamp, origin="1970-01-01")
	mlt$timestamp <- NULL

	startTimestamp <- min(mlt$time)
	stopTimestamp <- max(mlt$time)

	presenceLayer <- dD[dD$unittypename=="presence", ]
	presenceLayer <- rbind.fill(lapply(c("prediction", "presence"), function(x) data.frame(presenceLayer, variable=x)))

	startDateDay <- as.POSIXlt(startTimestamp, origin="1970-01-01")
	startDateDay$mday <- startDateDay$mday -1
	startDateDay$hour = 22

	endDateDay <- startDateDay
	endDateDay$mday <- endDateDay$mday + 1
	endDateDay$hour <- endDateDay$hour + 4

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
	print(filename)
	pdf(file=filename, width=8, height=4)

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


