#!/usr/bin/env Rscript

# install all required packages and load them
options("repos"="http://cran.us.r-project.org")
list.of.packages <- c("ggplot2", "reshape2", "gridExtra", "grid", "plyr", "RPostgreSQL", "scales", "methods", "pspline", "RHmm", "mhsmm")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
retVal <- lapply(list.of.packages, require, character.only=T)
allPackagesLoaded <- all(as.logical(retVal))
stopifnot(allPackagesLoaded == TRUE)

Rfiles <- dir(path = ".", pattern = "^init_[[:alnum:]]+\\.r$", include.dirs=F)
RfilesSelfRemoved <- Rfiles[!(Rfiles %in% c("init_init.r"))]
print(RfilesSelfRemoved)
lapply(RfilesSelfRemoved, source)












