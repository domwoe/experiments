#!/usr/bin/Rscript

# install all required packages and load them
options("repos"="http://cran.us.r-project.org")
list.of.packages <- c("ggplot2", "reshape2", "gridExtra", "grid", "plyr", "RPostgreSQL", "scales")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

cat("Plot some presence/occupancy data from the database\n")

presenceFields <- c("presence", "doorcontact", "windowcontact")
#presenceFieldsColors <- c("green", "red", "blue")
#presenceFieldsColors <- setNames(presenceFieldsColors, presenceFields)
#sensorFields <- setdiff(unique(dbResult$unittypename), presenceFields)
#presenceFields <- c("presence")
sensorFields <- c("temperature", "co2", "humidity")

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

	# we have to subtract one hour from all presence indicator readings: presence, doorcontact, windowcontact
	dbResult[dbResult$unittypename %in% presenceFields, "timestamp"] <- dbResult[dbResult$unittypename %in% presenceFields, "timestamp"] - 1000*60*60 # subtract one hour

	# we add another column (timestamp2) to the data frame. this allows us to plot rectangles/intervals later on
	#ddplyCols <- c("loc_id", "unittypename")
	ddplyCols <- c("sens_id")
	dbResult <- ddply(dbResult, ddplyCols, function(df) {
		tmp <- df[with(df, order(timestamp)), setdiff(colnames(dbResult),ddplyCols)] #sort and select columns
		if(nrow(tmp) > 0) {
			tmp$timestamp2 <- tmp$timestamp[c(2:length(tmp$timestamp),length(tmp$timestamp))]
		}
		return(tmp)
	})

	#cache locally
	write.table(dbResult, file=cacheFile, sep="\t")
} else {
	cat("Loading data from local file\n")
	dbResult <- data.frame()
	dbResult <- read.table(file=cacheFile, header=TRUE, sep="\t")
}
# transform timestamps to actual times / add one column each
dbResult$time1 <- as.POSIXct(dbResult$timestamp %/% 1000, origin="1970-01-01")
dbResult$time2 <- as.POSIXct(dbResult$timestamp2 %/% 1000, origin="1970-01-01")

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
	sensorLayerP <- sensorLayer[as.POSIXlt(sensorLayer$time1) >= startDateDay & as.POSIXlt(sensorLayer$time1) <= endDateDay, ]

	contactsFilter <- (as.POSIXlt(contactsOverlay$time1) >= startDateDay & as.POSIXlt(contactsOverlay$time1) <= endDateDay) |
				(as.POSIXlt(contactsOverlay$time2) >= startDateDay & as.POSIXlt(contactsOverlay$time2) <= endDateDay)
	contactsOverlayP <- contactsOverlay[contactsFilter, ]

	presenceFilter <- (as.POSIXlt(presenceOverlay$time1) >= startDateDay & as.POSIXlt(presenceOverlay$time1) <= endDateDay) |
				(as.POSIXlt(presenceOverlay$time2) >= startDateDay & as.POSIXlt(presenceOverlay$time2) <= endDateDay)
	presenceOverlayP <- presenceOverlay[presenceFilter, ]
	

	##output
	filenameDate <- startDateDay
	filenameDate$hour <- filenameDate$hour + 6
	filename <- paste("plotDB_", format(filenameDate, "%Y-%m-%d"), ".pdf", sep="")
	print(filename)
	pdf(file=filename, width=8, height=4)

	plotList <- lapply(unique(sensorLayerP$loc_id), function(x) {
		plt <- ggplot()
		#print(nrow(presenceOverlayP[presenceOverlayP$loc_id == x,]))
		#print(x)
		#print(head(presenceOverlayP[presenceOverlayP$loc_id == x,]))
		
		if(nrow(presenceOverlayP[presenceOverlayP$loc_id == x,]) > 0) {
			#print(head(presenceOverlayP[presenceOverlayP$loc_id == x,]))
			plt <- plt + geom_rect(data = presenceOverlayP[presenceOverlayP$loc_id == x & presenceOverlayP$unittypename!="contacts",], mapping = aes(xmin = time1, xmax = time2, ymin = -Inf, ymax = Inf*((reading*2)-1), fill=presencetype), color=NA, alpha=0.7)
			#plt <- plt + geom_rect(data = presenceOverlayP[presenceOverlayP$loc_id == x,], mapping = aes(xmin = time1, xmax = time2, ymin = -Inf, ymax = Inf*((reading*2)-1), fill=presencetype), color=NA, alpha=0.7)
			if(nrow(contactsOverlayP[contactsOverlayP$loc_id == x,]) > 0) {
				plt <- plt + geom_rect(data = presenceOverlayP[presenceOverlayP$loc_id == x & presenceOverlayP$unittypename=="contacts",], mapping = aes(xmin = time1, xmax = time2, ymin = -Inf, ymax = Inf*((reading*2)-1), fill=presencetype), color=NA, alpha=0.7)
				plt <- plt + geom_rect(data = contactsOverlayP[contactsOverlayP$loc_id == x,], mapping = aes(xmin = time1, xmax = time2, fill=contacttype, ymin = -Inf, ymax = Inf*((reading*2)-1), color=contacttype) )
				dummy <- data.frame(time1 = as.POSIXct(startDateDay), time2 = as.POSIXct(endDateDay), unittypename="contacts")
				plt <- plt + geom_rect(data = dummy, mapping = aes(xmin = time1, xmax = time2, ymin = 0, ymax = 1), fill=NA, color=NA, alpha=0)
			}	
		}
		colorz <-  c("presence" = "green", "doorcontact"="red", "windowcontact"="blue", "co2"="black", "temperature"="brown", "humidity"="darkgreen")
		plt <- plt + scale_fill_manual(values = colorz)
		plt <- plt + scale_color_manual(values = colorz)		
		plt <- plt + geom_line(data = sensorLayerP[sensorLayerP$loc_id == x,], aes(x = time1, y = reading, color=unittypename), size=0.8)
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




