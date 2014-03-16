#!/usr/bin/env Rscript

# install all required packages and load them
options("repos"="http://cran.us.r-project.org")
list.of.packages <- c("ggplot2", "reshape2", "gridExtra", "grid", "plyr", "RPostgreSQL", "scales")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

Rfiles <- dir(path = ".", pattern = "^init_[[:alnum:]]+\\.r$", include.dirs=F)
RfilesSelfRemoved <- Rfiles[!(Rfiles %in% c("init_init.r"))]
print(RfilesSelfRemoved)
lapply(RfilesSelfRemoved, source)












