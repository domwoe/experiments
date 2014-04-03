#!/usr/bin/env Rscript

meanBinaryLoss <- function(vec1, vec2) {
	stopifnot(all(vec1 == 1 | vec1 == 0))
	stopifnot(all(vec2 == 1 | vec2 == 0))
	mean(abs(vec1-vec2))
}

lossMetrics <- function(truth, prediction) {
	#binLoss <- meanBinaryLoss(truth, prediction)
	p <- sum(truth == 1)
	n <- sum(truth == 0)
	tp <- sum(truth == 1 & prediction == 1)
	tn <- sum(truth == 0 & prediction == 0)
	fp <- sum(truth == 0 & prediction == 1)
	fn <- sum(truth == 1 & prediction == 0)
	c(Sensitivity=tp/p, Specificity=tn/n, Precision=tp/(tp+fp), Accuracy=(tp+tn)/(p+n),
	  F1score=2*tp/(2*tp + fp + fn) )
}

meanBinaryLossRaw <- function(rawDf, predDf) {
	ind <- findInterval(predDf$timestamp, rawDf$timestamp)
	stopifnot(length(ind)==nrow(predDf))
	meanBinaryLoss(rawDf$reading[ind], predDf$presence)
}




