library(devtools)
library(pbapply)
library(deming)
library(pbmcapply)
library(IsoplotR)
library(data.table)
library(tictoc)
library(rlist)
library(dplyr)
library(loo)
library(bayclumpr)
source("functions.R")

synData$TempError <- ifelse(synData$TempError ==0, 1E-5, synData$TempError)
synData$D47error <- ifelse(synData$D47error ==0, 1E-5, synData$D47error)
targetColumns <- colnames(synData)[c(12)]
targetLevels<- names(which(table(synData[,targetColumns])>10))
synData <- synData[synData[,targetColumns] %in% targetLevels ,]

#targetLevels2 <- tapply(synData$Temperature, synData[,targetColumns], function(x) length(unique(x)))
#targetLevels <- names(which(targetLevels2>10))
#synData <- synData[synData[,targetColumns] %in% targetLevels ,]

SynthesisResults <- fitsinglePartitioned(
  calData = synData,
  targetColumns = targetColumns,
  prefix = paste0("Synthesis_",colnames(synData)[c(12)],"_", Sys.Date())
)


