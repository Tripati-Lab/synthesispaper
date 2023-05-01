library(devtools)
library(pbapply)
library(deming)
library(pbmcapply)
library(IsoplotR)
library(R2jags)
library(data.table)
library(tictoc)
library(rlist)
library(dplyr)
library(loo)
source("functions.R")

synData$TempError <- ifelse(synData$TempError ==0, 1E-5, synData$TempError)
synData$D47error <- ifelse(synData$D47error ==0, 1E-5, synData$D47error)
targetColumns <- colnames(synData)[c(14)]
targetLevels<- names(which(table(synData[,targetColumns])>10))
synData <- synData[synData[,targetColumns] %in% targetLevels ,]

SynthesisResults <- fitsinglePartitioned(
  calData = synData,
  targetColumns = targetColumns,
  prefix = paste0("Synthesis_",colnames(synData)[c(14)],"_", Sys.Date())
)
