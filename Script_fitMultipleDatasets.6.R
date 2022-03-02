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

tic()
dumpfile <- file("script6messages.txt", open = "wt")
sink(dumpfile, type = "output")

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
#synData <- read.csv('RawData/Current List_Sep27_2021.csv')
#synData <- read.csv('RawData/Current List_Nov22_2021.csv')
#synData <- read.csv('RawData/Current List_Nov24_2021.csv')
#synData <- read.csv('RawData/Current List_Dec1_2021.csv')
#synData <- read.csv('RawData/Current List_Feb4_2022.csv')
synData <- read.csv('RawData/Current List_Mar1_2022.csv')


synData$TempError <- ifelse(synData$TempError ==0, 1E-5, synData$TempError)
synData$D47error <- ifelse(synData$D47error ==0, 1E-5, synData$D47error)
targetColumns <- colnames(synData)[c(16)]
targetLevels<- names(which(table(synData[,targetColumns])>10))
synData <- synData[synData[,targetColumns] %in% targetLevels ,]

SynthesisResults <- fitsinglePartitioned(
  calData = synData,
  targetColumns = targetColumns,
  replicates = 1000,
  generations = 50000,
  maxtry = 10,
  export = T,
  prefix = paste0("Synthesis_",colnames(synData)[c(16)],"_", Sys.Date())
)

sink()

sink(file = "script6time.txt", type = c("output", "message"))
toc()
sink()