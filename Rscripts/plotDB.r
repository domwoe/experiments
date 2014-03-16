#!/usr/bin/Rscript

source("init_init.r")


cat("Plot some presence/occupancy data from the database\n")

presenceFields <- c("presence", "doorcontact", "windowcontact")
sensorFields <- c("temperature", "co2", "humidity")

presenceOverlay <- dbResult[dbResult$unittypename %in% c("presence"), ]
colnames(presenceOverlay)[colnames(presenceOverlay)=="unittypename"] <- "presencetype"
presenceOverlay <- rbind.fill(lapply(c("temperature", "co2", "humidity", "contacts"), function(x) data.frame(presenceOverlay, unittypename=x)))

contactsOverlay <- dbResult[dbResult$unittypename %in% c("doorcontact", "windowcontact"), ]
colnames(contactsOverlay)[colnames(contactsOverlay)=="unittypename"] <- "contacttype"
contactsOverlay$unittypename <- "contacts"

sensorLayer <- dbResult[dbResult$unittypename %in% sensorFields, ] #we only need sensor fields now

startDateDay <- as.POSIXlt("2014-01-28 22:00", origin="1970-01-01")
endDateDay <- startDateDay
endDateDay$mday <- endDateDay$mday + 1
endDateDay$hour <- endDateDay$hour + 4

while(max(sensorLayer$time1) > startDateDay)
{
	#sensorLayerP <- sensorLayer[as.POSIXlt(sensorLayer$time1) >= startDateDay & as.POSIXlt(sensorLayer$time1) <= endDateDay, ]
	sensorLayerP <- sensorLayer

	#contactsFilter <- (as.POSIXlt(contactsOverlay$time1) >= startDateDay & as.POSIXlt(contactsOverlay$time1) <= endDateDay) |
				(as.POSIXlt(contactsOverlay$time2) >= startDateDay & as.POSIXlt(contactsOverlay$time2) <= endDateDay)
	#contactsOverlayP <- contactsOverlay[contactsFilter, ]
	contactsOverlayP <- contactsOverlay

	#presenceFilter <- (as.POSIXlt(presenceOverlay$time1) >= startDateDay & as.POSIXlt(presenceOverlay$time1) <= endDateDay) |
				(as.POSIXlt(presenceOverlay$time2) >= startDateDay & as.POSIXlt(presenceOverlay$time2) <= endDateDay)
	#presenceOverlayP <- presenceOverlay[presenceFilter, ]
	presenceOverlayP <- presenceOverlay
	

	##output
	filenameDate <- startDateDay
	filenameDate$hour <- filenameDate$hour + 6
	filename <- paste("plotDB_", format(filenameDate, "%Y-%m-%d"), ".pdf", sep="")
	print(filename)
	pdf(file=filename, width=8, height=4)

	plotList <- lapply(unique(sensorLayerP$loc_id), function(x) {
		plt <- ggplot()
		
		if(nrow(presenceOverlayP[presenceOverlayP$loc_id == x,]) > 0) {
			plt <- plt + geom_rect(data = presenceOverlayP[presenceOverlayP$loc_id == x & presenceOverlayP$unittypename!="contacts",], mapping = aes(xmin = time1, xmax = time2, ymin = -Inf, ymax = Inf*((reading*2)-1), fill=presencetype), color=NA, alpha=0.9)
			#plt <- plt + geom_rect(data = presenceOverlayP[presenceOverlayP$loc_id == x,], mapping = aes(xmin = time1, xmax = time2, ymin = -Inf, ymax = Inf*((reading*2)-1), fill=presencetype), color=NA, alpha=0.7)
			if(nrow(contactsOverlayP[contactsOverlayP$loc_id == x,]) > 0) {
				#plt <- plt + geom_rect(data = presenceOverlayP[presenceOverlayP$loc_id == x & presenceOverlayP$unittypename=="contacts",], mapping = aes(xmin = time1, xmax = time2, ymin = -Inf, ymax = Inf*((reading*2)-1), fill=presencetype), color=NA, alpha=0.7)
				plt <- plt + geom_rect(data = contactsOverlayP[contactsOverlayP$loc_id == x,], mapping = aes(xmin = time1, xmax = time2, fill=contacttype, ymin = -Inf, ymax = Inf*((reading*2)-1), color=contacttype) )
				dummy <- data.frame(time1 = as.POSIXct(startDateDay), time2 = as.POSIXct(endDateDay), unittypename="contacts")
				plt <- plt + geom_rect(data = dummy, mapping = aes(xmin = time1, xmax = time2, ymin = 0, ymax = 1), fill=NA, color=NA, alpha=0)
			}	
		}
		colorz <-  c("presence" = "green", "doorcontact"="red", "windowcontact"="blue", "co2"="black", "temperature"="brown", "humidity"="darkgreen")
		plt <- plt + scale_fill_manual(values = colorz)
		plt <- plt + scale_color_manual(values = colorz)		
		plt <- plt + geom_step(data = sensorLayerP[sensorLayerP$loc_id == x,], aes(x = time1, y = reading, color=unittypename), size=0.8)
		plt <- plt + facet_grid(loc_id+loc_displayname+unittypename~.,scales="free_y")
		plt <- plt + guides(color="none")
		plt <- plt + scale_x_datetime(labels = date_format("%d/%m %H:%M"), expand = c(0,0), limits = c(as.POSIXct(startDateDay), as.POSIXct(endDateDay)))
		plt <- plt + theme_bw()
		plt <- plt + theme(legend.position = "top", legend.title=element_blank(), panel.margin = unit(0.3, "cm"), strip.text.y = element_text(size = 8),
		  axis.title.y = element_text(vjust=0.25), axis.title.x = element_text(vjust=-0.05))
		print(plt)
	})
	dev.off()

	startDateDay$mday <- startDateDay$mday + 1
	endDateDay$mday <- endDateDay$mday + 1
}




