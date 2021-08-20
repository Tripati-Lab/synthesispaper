library(devtools)
library(pbapply)
library(deming)
library(pbmcapply)
library(IsoplotR)
library(R2jags)
library(data.table)
library(googlesheets4)
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
synData <-
  read_sheet(
    "https://docs.google.com/spreadsheets/d/1PZ_o0lA-bpOGG9e76o4bGQWjvMnqgH56aWXmdxVkr-k/edit?usp=sharing"
  )
