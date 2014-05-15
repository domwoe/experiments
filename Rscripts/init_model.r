#!/usr/bin/env Rscript

.RunConf <- setClass("RunConf", contains="list")

RunConf <- function(model, loc_id, sensorFeat=c(), ...) {
    new("RunConf", list(model=model, loc_id=loc_id, sensorFeat=sensorFeat, ...))
}

getIdVec <- function(rc) {
	ret <- data.frame(model=class(rc$model),
			 loc_id=rc$loc_id,
			 sensorFeat=paste(rc$sensorFeat, collapse="\n"))
	return(ret)
}

extractCombine <- function(rcList, bindAttr) {
	dtf <- ldply(rcList, function(m) {
			idv <- getIdVec(m)
			if(!any(names(m) == bindAttr)) {
				return(NULL)
			}
			ret <- data.frame(idv, m[names(m) == bindAttr][[1]] )
			return(ret)
		})
}








.Model <- setClass("Model", representation(sensorData="data.frame", sensors="character", internalModel="ANY"))

Model <- function(sensorData=data.frame(), sensors=character(), internalModel, ...) {
    new("Model", sensorData=sensorData, sensors=sensors, internalModel=internalModel, ...)
}

setGeneric("predictFromModel", function(object, newSensorData) {
  standardGeneric("predictFromModel")
})

setGeneric("predictUnsupervised", function(object, newSensorData) {
  standardGeneric("predictUnsupervised")
})

setMethod("predictFromModel", signature("Model", "data.frame"), function(object, newSensorData) {
	cat("not implemented. abstract class.\n")
	stopifnot(FALSE)
})

setMethod("predictUnsupervised", signature("Model", "data.frame"), function(object, newSensorData) {
	cat("not implemented. abstract class.\n")
	stopifnot(FALSE)
})


.SimpleMarkov <- setClass("SimpleMarkov", contains="Model")

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
		v <- var(df[, colnames(df) %in% sensors]) # extract variances from co-variance matrix
		#v <- sqrt(v) # compute standard devitaion from variance // mhsmm needs variance!
		#d <- diag(x=v) # create diagonal matrix
		#colnames(d) <- names(v)
		#rownames(d) <- names(v)
		return(v)
	})
	attr(sigmaEstimate,"split_type") <- NULL
	attr(sigmaEstimate,"split_labels") <- NULL
	b <- list(mu=meanEstimate,sigma=sigmaEstimate)
	hmmmodel <- hmmspec(init=initV, trans=pMat, parms.emission=b, dens.emission=dmvnorm.hsmm)
	new("SimpleMarkov", Model(sensorData, sensors, hmmmodel), ...)
}

setMethod("predictFromModel", signature("SimpleMarkov", "data.frame"), function(object, newSensorData) {
	#train <- simulate(object@model, nsim=50, seed=1234, rand.emis=rmvnorm.hsmm)
	#print(train)
	#print(class(train))
	#print(names(train))
	#print(train)
	#stop()
	# are they normalized ?
	newDat <- newSensorData[, colnames(newSensorData) %in% object@sensors]
	newDat <- as.matrix(newDat)
	# we have to create a list first before handing data to predict to avoid warnings and other stuff
	prediction <- predict.hmmspec(object@internalModel,newdata=list(s=NA,x=newDat,N=nrow(newDat)),method="viterbi")
	#prediction <- predict.hmmspec(object@model,newdata=newDat,method="smoothed")
	#print(str(prediction))
	predictionNormalized <- prediction$s -1
	return(predictionNormalized)
})


setMethod("predictUnsupervised", signature("SimpleMarkov", "data.frame"), function(object, newSensorData) {
	initV <- c(1/2,1/2)
	pMat <- matrix(c(1/2,1/2,1/2,1/2),nrow=2)
	v <- var(newSensorData[, colnames(newSensorData) %in% object@sensors])
	#d <- diag(x=v) # create diagonal matrix
	#colnames(d) <- names(v)
	#rownames(d) <- names(v)
	sigmaEstimate <- list(v, v)

	varVec <- sqrt(diag(v))
	#sigmaFactor <- 1
	cM <- as.vector(colMeans(newSensorData[, colnames(newSensorData) %in% object@sensors]))
	meanEstimate <- list(cM-varVec, cM+varVec)

	b <- list(mu=meanEstimate,sigma=sigmaEstimate)
	hmmmodel <- hmmspec(init=initV, trans=pMat, parms.emission=b, dens.emission=dmvnorm.hsmm)
	#train <- simulate(hmmmodel, 10, seed = 123, rand.emis = rmvnorm.hsmm)
	newDat <- newSensorData[, colnames(newSensorData) %in% object@sensors]
	newDat <- as.matrix(newDat)
	newDatL <- list(s=NA,x=newDat,N=nrow(newDat))
	h1 <- hmmfit(newDatL,hmmmodel,mstep=mstep.mvnorm)
	#print(h1)
	prediction <- predict.hmm(h1,newdata=newDatL,method="viterbi")
	predictionNormalized <- prediction$s -1
	#predictionNormalized <- abs(prediction$s -2)
	#print(unique(predictionNormalized))
	# TODO: figure out mapping of states (note: they are not labelled). we need to label them. (seems correct at the moment)
	return(predictionNormalized)
})


.alwaysOccupied <- setClass("alwaysOccupied", contains="Model")
alwaysOccupied  <- function(sensorData=data.frame(), sensors=character(), ...) {
	new("alwaysOccupied", Model(sensorData, sensors, NA), ...)
}
setMethod("predictFromModel", signature("alwaysOccupied", "data.frame"), function(object, newSensorData) {
	return( rep(1, nrow(newSensorData)) )
})
setMethod("predictUnsupervised", signature("alwaysOccupied", "data.frame"), function(object, newSensorData) {
	return( predictFromModel(object, newSensorData ) )
})

.alwaysUnoccupied <- setClass("alwaysUnoccupied", contains="Model")
alwaysUnoccupied  <- function(sensorData=data.frame(), sensors=character(), ...) {
	new("alwaysUnoccupied", Model(sensorData, sensors, NA), ...)
}
setMethod("predictFromModel", signature("alwaysUnoccupied", "data.frame"), function(object, newSensorData) {
	return( rep(0, nrow(newSensorData)) )
})
setMethod("predictUnsupervised", signature("alwaysUnoccupied", "data.frame"), function(object, newSensorData) {
	return( predictFromModel(object, newSensorData ) )
})



.ConditionalMarkov <- setClass("ConditionalMarkov", contains="Model")

ConditionalMarkov  <- function(sensorData=data.frame(), sensors=character(), ...) {

	new("ConditionalMarkov", Model(sensorData, sensors, NA), ...)
}

setMethod("predictFromModel", signature("ConditionalMarkov", "data.frame"), function(object, newSensorData) {
	return(NULL)
})


setMethod("predictUnsupervised", signature("ConditionalMarkov", "data.frame"), function(object, newSensorData) {
	return(NULL)
})











