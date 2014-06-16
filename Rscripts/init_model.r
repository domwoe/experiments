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

extractCombine <- function(rcList, bindAttr, transpose=FALSE) {
	dtf <- ldply(rcList, function(m) {
			idv <- getIdVec(m)
			if(!any(names(m) == bindAttr)) {
				return(NULL)
			}
			retV <- m[names(m) == bindAttr][[1]]
			if(transpose) {
				#print(idv)
				#print(length(retV))
				retV <- setNames(retV, NULL)
				retV <- t(data.frame(retV))
			} else {
				retV <- data.frame(retV)
			}

			ret <- data.frame(idv, retV)
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


.SimpleMarkovUnsupervised <- setClass("SimpleMarkovUnsupervised", contains="Model")

SimpleMarkovUnsupervised  <- function(sensorData=data.frame(), sensors=character(), ...) {
	initV <- c(1/2,1/2)
	pMat <- matrix(c(1/2,1/2,1/2,1/2),nrow=2)
	v <- var(sensorData[, colnames(sensorData) %in% sensors])
	#d <- diag(x=v) # create diagonal matrix
	#colnames(d) <- names(v)
	#rownames(d) <- names(v)
	sigmaEstimate <- list(v, v)

	varVec <- sqrt(diag(v))
	#sigmaFactor <- 1
	cM <- as.vector(colMeans(sensorData[, colnames(sensorData) %in% sensors]))
	meanEstimate <- list(cM-varVec, cM+varVec)

	b <- list(mu=meanEstimate,sigma=sigmaEstimate)
	hmmmodel <- hmmspec(init=initV, trans=pMat, parms.emission=b, dens.emission=dmvnorm.hsmm)
	#train <- simulate(hmmmodel, 10, seed = 123, rand.emis = rmvnorm.hsmm)
	newDat <- sensorData[, colnames(sensorData) %in% sensors]
	newDat <- as.matrix(newDat)
	newDatL <- list(s=NA,x=newDat,N=nrow(newDat))
	h1 <- hmmfit(newDatL,hmmmodel,mstep=mstep.mvnorm)
	new("SimpleMarkovUnsupervised", Model(sensorData, sensors, h1), ...)
}

setMethod("predictFromModel", signature("SimpleMarkovUnsupervised", "data.frame"), function(object, newSensorData) {
	newDat <- newSensorData[, colnames(newSensorData) %in% object@sensors]
	newDat <- as.matrix(newDat)
	newDatL <- list(s=NA,x=newDat,N=nrow(newDat))
	prediction <- predict.hmm(object@internalModel,newdata=newDatL,method="viterbi")
	predictionNormalized <- prediction$s -1
	#predictionNormalized <- abs(prediction$s -2)
	#print(unique(predictionNormalized))
	# TODO: figure out mapping of states (note: they are not labelled). we need to label them. (seems correct at the moment)
	return(predictionNormalized)	
})






.SimpleMarkov <- setClass("SimpleMarkov", contains="Model")

SimpleMarkov  <- function(sensorData=data.frame(), sensors=character(), ...) {
	l <- length(sensorData$presence)
	#print(l)
	digr <- data.frame(first=sensorData$presence,second=sensorData$presence[c(2:l,l)]) # hmmm... double check this.... indices 1:(l-1) and 2:l maybe?
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
		v <- var(df[, colnames(df) %in% sensors]) 
		#v <- sqrt(v) # compute standard devitaion from variance // mhsmm needs variance!
		#d <- diag(x=v) # create diagonal matrix
		#colnames(d) <- names(v)
		#rownames(d) <- names(v)
		return(v)
	})
	attr(sigmaEstimate,"split_type") <- NULL
	attr(sigmaEstimate,"split_labels") <- NULL
	stopifnot(length(meanEstimate) > 1)
	stopifnot(length(sigmaEstimate) > 1)
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
	#
	newDat <- newSensorData[, colnames(newSensorData) %in% object@sensors]
	newDat <- as.matrix(newDat)
	# we have to create a list first before handing data to predict to avoid warnings and other stuff
	prediction <- predict.hmmspec(object@internalModel,newdata=list(s=NA,x=newDat,N=nrow(newDat)),method="viterbi")
	#print(exp(prediction$loglik))
	#stop()
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



.ConditionalMarkov <- setClass("ConditionalMarkov", contains="Model", representation(b="list", initV="vector"))

ConditionalMarkov  <- function(sensorData=data.frame(), sensors=character(), ...) {
	sensorData$hour <- (as.POSIXlt(sensorData$timestamp, origin="1970-01-01")$hour)
	#print(head(sensorData))

	initC <- as.vector(table(sensorData$presence))
	initV <- initC/sum(initC)

	meanEstimate <- dlply(sensorData, .(presence), function(df) {
		cM <- colMeans(df[, colnames(df) %in% sensors])
		vec <- as.vector(cM)
		return(vec)
	})

	sigmaEstimate <- dlply(sensorData, .(presence), function(df) {
		v <- var(df[, colnames(df) %in% sensors]) 
		return(v)
	})

	internalTransitionMats <- dlply(sensorData, .(hour), function(df) {
		#print(unique(df$hour))
		l <- nrow(df)
		digr <- data.frame(first=df$presence[1:(l-1)], second=df$presence[2:l])
		digr <- rbind(digr, c(1, 1), c(0, 1), c(1, 0), c(0, 0))
		pMat <- as.matrix(table(digr))
		#print(pMat)
		pMat <- pMat/rowSums(pMat)
		#print(pMat)
		#stop()	
	})

	b <- list(mu=meanEstimate, sigma=sigmaEstimate)
	new("ConditionalMarkov", Model(sensorData, sensors, internalTransitionMats), b=b, initV=initV, ...)
}

setMethod("predictFromModel", signature("ConditionalMarkov", "data.frame"), function(object, newSensorData) {
	#print(head(newSensorData))
	#print(object@sensors)
	#print(object@initV)
	#print(object@b)
	posdate <- as.POSIXlt(newSensorData$timestamp, origin="1970-01-01")
	newSensorData$hour <- posdate$hour
	newSensorData$day <- posdate$year * 366 + posdate$yday # compute unique day identifier (since 1900)
	# check for single row dfs:
	newSensorData <- ddply(newSensorData, .(day, hour), function(df) {
		if(nrow(df) == 1) {
			df[df$hour == 0, "day"] <- df[df$hour == 0, "day"] -1
			df$hour <- (df$hour -1) %% 24
			#print(df)
		}
		return(df)
	})
	predList <- dlply(newSensorData, .(day, hour), function(df) {
		h <- as.character(unique(df$hour))
		#day <- as.character(unique(df$day))
		#print(paste(h, day, sep=" : "))
		pMat <- (object@internalModel)[[h]]
		newDat <- df[, colnames(df) %in% object@sensors]
		newDat <- as.matrix(newDat)
		#print(nrow(newDat))
		#print(newDat)
		#print(df)
		hmmmodel <- hmmspec(init=object@initV,
					trans=pMat,
					parms.emission=object@b,
					dens.emission=dmvnorm.hsmm)
		prediction <- predict.hmmspec(hmmmodel,
					      newdata=list(s=NA,x=newDat,N=nrow(newDat)),method="viterbi")
			#print(names(hmmmodel))
			#print((prediction$s))
			#loglik <- prediction$loglik
			#lastF <- newDat[nrow(newDat), ]

			# todo. what if argument of logarithm becomes zero
			#lastS <- prediction$s[length(prediction$s)]
			#lastSlogProb <- log(dmvnorm(lastF, mean = object@b$mu[[lastS]],
			#				sigma = object@b$sigma[[lastS]]))

			#lastSother <- (lastS-2)*(-1) + 1
			#lastSlogProbOther <- log(dmvnorm(lastF, mean = object@b$mu[[lastSother]],
			#				sigma = object@b$sigma[[lastSother]]))
			#SecondLastS <- prediction$s[length(prediction$s)-1]
			#print(lastSlogProb)
			#print(lastS)
			#print(lastSother)
			#print(lastSlogProbOther)

			#print(SecondLastS)
			#stop()
		predictionNormalized <- prediction$s -1
		return(predictionNormalized)
	})
	predREt <- do.call(c, predList)
	return(predREt)
})


setMethod("predictUnsupervised", signature("ConditionalMarkov", "data.frame"), function(object, newSensorData) {
	#posdate <- as.POSIXlt(newSensorData$timestamp, origin="1970-01-01")
	#newSensorData$hour <- posdate$hour
	#print(head(newSensorData))

	#responseFormula = as.formula( paste(paste(object@sensors, collapse="+"),"~1", sep="") )
	#print(responseFormula)

	#sdf <- as.matrix(newSensorData[, colnames(newSensorData) %in% object@sensors])
	#print(head(sdf))
	#rModels <- list(
	#	list(
	#		# why is it not possible to enter a formula as in the 1D case. this is stupid..
	#		MVNresponse(sdf~1)
	#		#MVNresponse(formula=responseFormula,data=newSensorData)
	#	),
	#	list(
	#		MVNresponse(sdf~1,ps=c(1,1,1,1,1))
	#		#MVNresponse(formula=responseFormula,data=newSensorData)
	#	)
	#)
	#transition <- list()
	#transition[[1]] <- transInit(~hour,nstates=2,data=newSensorData)
	#transition[[2]] <- transInit(~hour,nstates=2,data=newSensorData)

	#instart=runif(2)
     	#inMod <- transInit(~1,ns=2,ps=instart,data=data.frame(1),family=multinomial("identity"))
	#inMod <- transInit(~1,nstates=2,data=data.frame(rep(1,1)),family=multinomial("identity"))
	#mod <- makeDepmix(response=rModels,transition=transition,homogeneous=FALSE,prior=inMod,ntimes=c(nrow(newSensorData)))
	#fmod <- fit(mod)

	return(NULL)
})




.SVM <- setClass("SVM", contains="Model")

SVM  <- function(sensorData=data.frame(), sensors=character(), ...) {
	sensorDatMat <- as.matrix(sensorData[, colnames(sensorData) %in% sensors])
	classVec <- as.factor(sensorData$presence)
	tbl <- table(classVec)
	w <- setNames(as.vector(tbl), names(tbl))
	w <- w/sum(w)
	svmObj <- ksvm(x=sensorDatMat, y=classVec, class.weights = w)
	new("SVM", Model(sensorData, sensors, svmObj), ...)
}

setMethod("predictFromModel", signature("SVM", "data.frame"), function(object, newSensorData) {
	newSensorDatMat <- as.matrix(newSensorData[, colnames(newSensorData) %in% object@sensors])
	pred <- predict(object@internalModel, newSensorDatMat, type="response")
	as.numeric(as.character(pred))
})


setMethod("predictUnsupervised", signature("SVM", "data.frame"), function(object, newSensorData) {
	return(NULL)
})

createWindows <- function(df, windowSize) {
	vvv <- c(rep(1, windowSize %/% 2), 1:nrow(df), rep(nrow(df), windowSize %/% 2))
	dfs <- lapply(df, function(col) {
		l <- lapply(0:(windowSize-1), function(offset) col[ vvv[offset+(1:nrow(df))] ])
		names(l) <- paste("p", 0:(windowSize-1), sep="")
		l
	})
	dfa <- data.frame(dfs)
}


.SVMwindowed <- setClass("SVMwindowed", contains="Model", representation(windowSize="numeric"))

SVMwindowed  <- function(sensorData=data.frame(), sensors=character(), windowSize=5, ...) {
	df <- sensorData[, colnames(sensorData) %in% sensors]
	sensorDatMat <- as.matrix(createWindows(df, windowSize))
	classVec <- as.factor(sensorData$presence)
	tbl <- table(classVec)
	w <- setNames(as.vector(tbl), names(tbl))
	w <- w/sum(w)
	svmObj <- ksvm(x=sensorDatMat, y=classVec, class.weights = w)
	new("SVMwindowed", Model(sensorData, sensors, svmObj), windowSize=windowSize, ...)
}

setMethod("predictFromModel", signature("SVMwindowed", "data.frame"), function(object, newSensorData) {
	df <- newSensorData[, colnames(newSensorData) %in% object@sensors]
	newSensorDatMat <- as.matrix(createWindows(df, object@windowSize))
	pred <- predict(object@internalModel, newSensorDatMat, type="response")
	as.numeric(as.character(pred))
})


setMethod("predictUnsupervised", signature("SVMwindowed", "data.frame"), function(object, newSensorData) {
	return(NULL)
})








