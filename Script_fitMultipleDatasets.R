library(devtools)
library(pbapply)
library(deming)
library(pbmcapply)
library(IsoplotR)
library(R2jags)
library(data.table)
source("functions.R")

#Using some testing data
calData<-read.csv("https://raw.githubusercontent.com/Tripati-Lab/BayClump/main/BayClump_calibration_template.csv")
calData$Material2<-calData$Material
calData$Material<-NULL
targetColumns<-c("Material2", "Mineralogy")
testResults<-fitsinglePartitioned(calData=calData, 
                                  targetColumns=targetColumns, 
                                  replicates=2, 
                                  generations=1000, 
                                  maxtry=10, 
                                  export=T, 
                                  prefix=Sys.Date())

