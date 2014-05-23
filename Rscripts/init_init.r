#!/usr/bin/env Rscript

# install all required packages and load them
options("repos"="http://cran.us.r-project.org")
#list.of.packages <- c("ggplot2", "reshape2", "gridExtra", "grid", "plyr", "RPostgreSQL", "scales", "methods", "pspline", "RHmm", "mhsmm")
list.of.packages <- c("ggplot2", "reshape2", "plyr", "RPostgreSQL", "pspline", "depmixS4", "mhsmm", "scales", "gridExtra", "entropy")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
retVal <- lapply(list.of.packages, require, character.only=T)
allPackagesLoaded <- all(as.logical(retVal))
stopifnot(allPackagesLoaded == TRUE)

Rfiles <- dir(path = ".", pattern = "^init_[[:alnum:]]+\\.r$", include.dirs=F)
RfilesSelfRemoved <- Rfiles[!(Rfiles %in% c("init_init.r"))]
print(RfilesSelfRemoved)
lapply(RfilesSelfRemoved, source)

#pdf <- function(...) {
#	cat("Please use pdff(...) instead of pdf(...). Stopping.\n")
#	stop()
#}


pdff <- function(filename, ...) {
	foldername <- paste(Sys.getenv("ScriptName"), "out", sep="_")
	dir.create(foldername, showWarnings = FALSE)
	filename <- paste(foldername, filename, sep="/")
	print(filename)
	pdf(file=filename, ...)
}

thisFile <- function() {
        cmdArgs <- commandArgs(trailingOnly = FALSE)
        needle <- "--file="
        match <- grep(needle, cmdArgs)
        if (length(match) > 0) {
                # Rscript
                return(sub(needle, "", cmdArgs[match]))
        } else {
                # 'source'd via R console
                return(normalizePath(sys.frames()[[1]]$ofile))
        }
}









