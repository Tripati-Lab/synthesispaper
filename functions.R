library(devtools)
library(pbapply)
library(deming)
library(pbmcapply)
library(IsoplotR)
library(R2jags)
library(data.table)

source("https://raw.githubusercontent.com/Tripati-Lab/BayClump/main/Functions/fitClumpedRegressions.R")
source("https://raw.githubusercontent.com/Tripati-Lab/BayClump/main/Functions/clumPipe.R")
source("https://raw.githubusercontent.com/Tripati-Lab/BayClump/main/Functions/getR2Bayesian.R")
source("https://raw.githubusercontent.com/Tripati-Lab/BayClump/main/Functions/functionsSimulationsMixed.R")
source("https://raw.githubusercontent.com/Tripati-Lab/BayClump/main/Functions/functionsRegressionCI.R")
source("https://raw.githubusercontent.com/Tripati-Lab/BayClump/main/Functions/functionsRegressionCI.R")

fitsingleDataset<-function(data, replicates=2, generations=1000){

##Models
a<-simulateYork_measured(data=data, replicates=replicates)
b<-simulateLM_measured(data=data, replicates=replicates)
c<-simulateDeming(data=data, replicates=replicates)
d<-simulateLM_inverseweights(data=data, replicates=replicates)
e<-simulateBLM_measuredMaterial(data=data, replicates=replicates, generations=generations, isMixed=T)

SumTable<-rbind.data.frame(cbind(model='York',a, material=NA), 
                 cbind(model='LM',b, material=NA),
                 cbind(model='Deming',c, material=NA),
                 cbind(model='Weighted',d, material=NA),
                 cbind(model='Bayesian errors',e$BLM_Measured_errors, material=NA),
                 cbind(model='Bayesian no errors',e$BLM_Measured_no_errors, material=NA),
                 cbind(model='Bayesian mixed',e$BLMM_Measured_errors), make.row.names = F
                 )
attr(SumTable, "DICs") <- attr(e,"DICs")
return(SumTable)
}
fitsinglePartitioned <- function(calData, targetColumns, replicates=2 , generations=1000){
  sumPart<-lapply(targetColumns, function(x){
  calDataSelected<-calData
  calDataSelected$Material <- calData[,x]
  calDataSelected$Material <- as.numeric(as.factor(calDataSelected$Material))
  calDataSelected<-calDataSelected[calDataSelected$Material %in% names(table(calDataSelected$Material)>2),]
  #Full dataset
  dt<-fitsingleDataset(calDataSelected, replicates=replicates, generations=generations)
  #Partitioned by level
  subSampled<-lapply(unique(calDataSelected$Material), function(y){
    calDataSelectedgroup<-calDataSelected[calDataSelected$Material == y,]
    keep=T
    i=0
    k=0
    while ( keep ) {
      keep= if(i==0){if(k>100 ){F}else{T} }else{F}
      k=k+1
      tryCatch({
      resDS<-invisible(fitsingleDataset(calDataSelectedgroup, replicates=replicates, generations=generations))
      i=1
      return(resDS)
      }, error=function(e){})
    }
  } )
  names(subSampled)<-unique(calDataSelected$Material)
  
  fullDS<-rbind(cbind(.id='Full',dt) ,
        rbindlist(subSampled, idcol = T)
      )
  return(fullDS)
  
})
  
  names(sumPart)<- targetColumns
  sumPart<-rbindlist(sumPart,  idcol = T)
  colnames(sumPart)[1]<-'targetMaterialColumn'
  return(sumPart)
}






