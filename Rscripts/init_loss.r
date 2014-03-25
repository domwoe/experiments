#!/usr/bin/env Rscript

meanBinaryLoss <- function(vec1, vec2) {
	stopifnot(all(vec1 == 1 | vec1 == 0))
	stopifnot(all(vec2 == 1 | vec2 == 0))
	mean(abs(vec1-vec2))
}

meanBinaryLossRaw <- function(rawDf, predDf) {
	ind <- findInterval(predDf$timestamp, rawDf$timestamp)
	stopifnot(length(ind)==nrow(predDf))
	meanBinaryLoss(rawDf$reading[ind], predDf$presence)
}




