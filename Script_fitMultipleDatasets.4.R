library(devtools)
library(pbapply)
library(deming)
library(pbmcapply)
library(IsoplotR)
library(R2jags)
library(data.table)
source("functions.R")

#Using sample data from the app
calData <-
  read.csv(
    "https://raw.githubusercontent.com/Tripati-Lab/BayClump/main/BayClump_calibration_template.csv"
  )
calData$Material2 <- calData$Material
calData$Material <- NULL
targetColumns <- c("Material2", "Mineralogy")

testResults <- fitsinglePartitioned(
  calData = calData,
  targetColumns = targetColumns,
  replicates = 2,
  generations = 1000,
  maxtry = 10,
  export = T,
  prefix = paste0("Test_", Sys.Date())
)

#Real analyses
# Using the version on gdrive?
# library(googlesheets4)
# synData <-as.data.frame(
#   read_sheet(
#     "https://docs.google.com/spreadsheets/d/1PZ_o0lA-bpOGG9e76o4bGQWjvMnqgH56aWXmdxVkr-k/edit?usp=sharing"
#   ))

#synData <- read.csv('RawData/Current List_Sep22_2021.csv')
synData <- read.csv('RawData/Current List_Sep27_2021.csv')

#targetColumns <- colnames(synData)[c(11:19)]
targetColumns <- colnames(synData)[c(14)]

SynthesisResults <- fitsinglePartitioned(
  calData = synData,
  targetColumns = targetColumns,
  replicates = 1000,
  generations = 10000,
  maxtry = 10,
  export = T,
  prefix = paste0("Synthesis_",colnames(synData)[c(14)],"_", Sys.Date())
)


