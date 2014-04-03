#!/usr/bin/env Rscript

.Model <- setClass("Model", representation(sensorData="data.frame", sensors="character"))

Model <- function(sensorData=data.frame(), sensors=character(), ...) {
    new("Model", sensorData=sensorData, sensors=sensors, ...)
}

setGeneric("predictFromModel", function(object, newSensorData) {
  standardGeneric("predictFromModel")
})

setMethod("predictFromModel", signature("Model", "data.frame"), function(object, newSensorData) {
	cat("not implemented. abstract class.\n")
	stopifnot(FALSE)
})



.SimpleMarkov <- setClass("SimpleMarkov", contains="Model", representation(steps="numeric", model="ANY"))

SimpleMarkov  <- function(sensorData=data.frame(), sensors=character(), ...) {
	l <- length(sensorData$presence)
	#print(l)
	digr <- data.frame(first=sensorData$presence,second=sensorData$presence[c(2:l,l)])
	pMat <- as.matrix(table(digr))
	pMat <- pMat/rowSums(pMat)
	initC <- as.vector(table(sensorData$presence))
	initV <- initC/sum(initC)
	meanEstimate <- dlply(sensorData, .(presence), function(df) {
		cM <- colMeans(df[, colnames(df) %in% sensors])
		vec <- as.vector(cM)
		return(vec)
	})
	attr(meanEstimate,"split_type") <- NULL
	attr(meanEstimate,"split_labels") <- NULL
	sigmaEstimate <- dlply(sensorData, .(presence), function(df) {
		v <- diag(var(df[, colnames(df) %in% sensors])) # extract variances from co-variance matrix
		#v <- sqrt(v) # compute standard devitaion from variance // mhsmm needs variance!
		d <- diag(x=v) # create diagonal matrix
		colnames(d) <- names(v)
		rownames(d) <- names(v)
		return(d)
	})
	attr(sigmaEstimate,"split_type") <- NULL
	attr(sigmaEstimate,"split_labels") <- NULL
	b <- list(mu=meanEstimate,sigma=sigmaEstimate)
	hmmmodel <- hmmspec(init=initV, trans=pMat, parms.emission=b, dens.emission=dmvnorm.hsmm)
	new("SimpleMarkov", Model(sensorData, sensors), model=hmmmodel, ...)
}

setMethod("predictFromModel", signature("SimpleMarkov", "data.frame"), function(object, newSensorData) {
	#train <- simulate(object@model, nsim=50, seed=1234, rand.emis=rmvnorm.hsmm)
	#print(train)
	#print(class(train))
	#print(names(train))
	#print(train)
	#stop()
	newDat <- newSensorData[, colnames(newSensorData) %in% object@sensors]
	newDat <- as.matrix(newDat)
	# we have to create a list first before handing data to predict to avoid warnings and other stuff
	prediction <- predict.hmmspec(object@model,newdata=list(s=NA,x=newDat,N=nrow(newDat)),method="viterbi")
	#prediction <- predict.hmmspec(object@model,newdata=newDat,method="smoothed")
	#print(str(prediction))
	predictionNormalized <- prediction$s -1
	return(predictionNormalized)
})
















