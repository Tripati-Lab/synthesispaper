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
dumpfile <- file("script2messages.txt", open = "wt")
sink(dumpfile, type = "output")


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
#synData <- read.csv('RawData/Current List_Mar1_2022.csv')
#synData <- read.csv('RawData/Current List_Aug9_2022.csv')
#synData <- read.csv('RawData/Current List_Aug11_2022.csv')
synData <- read.csv('RawData/Current List_Aug15_2022.csv')
synData1 <- synData[synData$Mineralogy == "C" & synData$NaturalSynthetic == "Synthetic",]
synData2 <- synData[synData$Forams == "Foraminifera",]
synData <- rbind.data.frame(synData1, synData2)
synData <- synData[synData$Temperature > 9.57,] #Temps < 50C
synData$SynCalForam <- ifelse(synData$Forams == "Foraminifera", "Foraminifera", "Synthetic Calcite")
synData$TempError <- ifelse(synData$TempError ==0, 1E-5, synData$TempError)
synData$D47error <- ifelse(synData$D47error ==0, 1E-5, synData$D47error)
targetColumns <- colnames(synData)[c(36)]
targetLevels<- names(which(table(synData[,targetColumns])>10))
synData <- synData[synData[,targetColumns] %in% targetLevels ,]


SynthesisResults <- fitsinglePartitioned(
  calData = synData,
  targetColumns = targetColumns,
  replicates = 1000,
  generations = 50000,
  maxtry = 10,
  export = T,
  prefix = paste0("Synthesis_",colnames(synData)[c(36)],"TemoLess50C_", Sys.Date())
)

sink()

sink(file = "script2time.txt", type = c("output", "message"))
toc()
sink()

