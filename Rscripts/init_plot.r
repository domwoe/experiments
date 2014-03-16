#!/usr/bin/env Rscript

plotHistOccupancy <- function(df) {	
	presenceData <- df[df$unittypename == "presence" & df$reading == 1, ]
	#print(head(presenceData))
	presenceData$durationInSec <- (presenceData$timestamp2 - presenceData$timestamp) %/% 1000
	#presenceData$durationInSec <- as.POSIXct(presenceData$durationInSec, origin="1970-01-01")
	#print(head(presenceData))
	num_bins <- 17
	hls <- lapply(dlply(presenceData, .(loc_id)), function(x, b) {
	  rng <- max(x$durationInSec)
	  bwidth <- rng/num_bins
	  geom_histogram(data = x, binwidth=bwidth)
	})
	filename <- paste("hist_occupancy.pdf", sep="")
	print(filename)
	pdf(file=filename, width=7, height=9)
	plt <- ggplot(data=presenceData, aes(x=durationInSec)) + hls + facet_wrap(~loc_id+loc_displayname, scales="free", ncol = 2) + theme_bw() #+ scale_x_datetime(labels = date_format("%H:%M"))

	print(plt)
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


