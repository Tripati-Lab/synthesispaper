##Functions are sourced from BayClump

source(
  "https://raw.githubusercontent.com/Tripati-Lab/BayClump/dev/Functions/Calibration_BayesianNonBayesian.R"
)


##Basic functions

fitsingleDataset <- function(data,
                             replicates = 2) {
  ##Models
  a <- simulateYork_measured(data = data, replicates = replicates, samples=NULL)
  b <- simulateLM_measured(data = data, replicates = replicates, samples=NULL)
  c <- simulateDeming(data = data, replicates = replicates, samples=NULL)
  d <- simulateLM_inverseweights(data = data, replicates = replicates, samples=NULL)

    SumTable <- rbind.data.frame(
      cbind.data.frame(model = 'York', a),
      cbind.data.frame(model = 'LM', b),
      cbind.data.frame(model = 'Deming', c),
      cbind.data.frame(model = 'Weighted', d),
      make.row.names = F
    )
    
  return(SumTable)
}

fitsinglePartitioned <-
  function(calData,
           targetColumns,
           replicates = 2 ,
           generations = 1000,
           maxtry = 10,
           export = T,
           prefix = 'results') {
    
    
    sumPart <- lapply(targetColumns, function(x) {
      calDataSelected <- calData
      calDataSelected$Material <- calData[, x]
      key <- cbind.data.frame(original = calDataSelected$Material,
                              number = as.numeric(as.factor(calDataSelected$Material)))
      calDataSelected$Material <-
        as.numeric(as.factor(calDataSelected$Material))
      calDataSelected <-
        calDataSelected[calDataSelected$Material %in% names(table(calDataSelected$Material) >
                                                              2), ]
      #Full dataset
      full <- list("NonBayesian"=
        fitsingleDataset(data = calDataSelected,
                         replicates = replicates),
      
        "Bayesian"= fitClumpedRegressions(calibrationData=calDataSelected, 
                                 n.iter = generations, 
                                 burninFrac = 0.5,
                                 priors = "Informative",
                                 D47error = "D47error")
      )
      

      #Partitioned by level
      subSampled <-
        lapply(unique(calDataSelected$Material), function(y) {
          calDataSelectedgroup <-
            calDataSelected[calDataSelected$Material == y, ]
          calDataSelectedgroup$Material <- 1
          resDS <-
            invisible(
              list("NonBayesian"= fitsingleDataset(
                calDataSelectedgroup
              ),
              "Bayesian"=fitClumpedRegressions(calibrationData=calDataSelectedgroup, 
                                               n.iter = generations, 
                                               burninFrac = 0.5,
                                               priors = "Informative",
                                               D47error = "D47error")
              )
            )
          
          
        })
      
      names(subSampled) <- unique(calDataSelected$Material)
      
      ##Extract DICs
      DICs <- rbind.data.frame(
      cbind.data.frame("Material"="Full", data.frame(t(attr(full$Bayesian,"DICs")))),
      rbindlist(lapply(subSampled, function(x){
       data.frame(t(data.frame(attr(x$Bayesian,"DICs"))))
      } ), idcol = "Material")
      )

      ##Extract parameters (non-Bayesian)
      
      paramNonBayesian <- rbindlist(list(
      'Full'=full$NonBayesian %>% group_by(model)%>%
        summarise(meanBeta= mean(beta), sdBeta= sd(beta),
                  meanAlpha= mean(alpha), sdAlpha= sd(alpha), Material=NA),
      
      'subsets'=rbindlist(lapply(subSampled, function(x){
        if(!is.null(x) ){
       x$NonBayesian %>% group_by(model)%>%
          summarise(meanBeta= mean(beta), sdBeta= sd(beta),
                    meanAlpha= mean(alpha), sdAlpha= sd(alpha))
        }
      }), idcol = 'Material')
      ), fill = T, idcol = 'Dataset')
      
      ##Extract parameters (Bayesian)
      
      extractParamsBayesian <-  function(listBayesian, name, nameMaterial){
      
      nr<-nrow(listBayesian$Bayesian$BLM3_fit$BUGSoutput$summary)
      tdata <- listBayesian$Bayesian$BLM1_fit$BUGSoutput$summary[c(1:2),c(1:2)]
      a <- cbind.data.frame("Dataset"=name, 
                       "model"='BLM1_fit',
                       "meanBeta"=tdata[2,1],
                       "sdBeta"=tdata[2,2],
                       "meanAlpha"=tdata[1,1], 
                       "sdAlpha"=tdata[1,2], "Material"=ifelse(name!='subsets', NA, nameMaterial)
      )
      
      tdata <- listBayesian$Bayesian$BLM1_fit_NoErrors$BUGSoutput$summary[c(1:2),c(1:2)]
      b <- cbind.data.frame("Dataset"=name, 
                       "model"='BLM1_fit_NoErrors',
                       "meanBeta"=tdata[2,1],
                       "sdBeta"=tdata[2,2],
                       "meanAlpha"=tdata[1,1], 
                       "sdAlpha"=tdata[1,2], "Material"=ifelse(name!='subsets', NA, nameMaterial)
      )
      
      tdata <- listBayesian$Bayesian$BLM3_fit$BUGSoutput$summary[-c((nr-3):nr),c(1:2)]
      
      
      c<- if(nrow(tdata) ==2 ){
        
        cbind.data.frame("Dataset"=name, 
                         "model"='BLM3',
                         "meanBeta"=tdata[2,1],
                         "sdBeta"=tdata[2,2],
                         "meanAlpha"=tdata[1,1], 
                         "sdAlpha"=tdata[1,2], "Material"=ifelse(name!='subsets', NA, nameMaterial)
        )
        
      }else{
      
      do.call(rbind, lapply(1:(nrow(tdata)/2), function(x){
      cbind.data.frame("Dataset"=name, 
                       "model"='BLM3',
                       "meanBeta"=tdata[(x+2),1],
                       "sdBeta"=tdata[(x+2),2],
                       "meanAlpha"=tdata[(x),1], 
                       "sdAlpha"=tdata[x,2], 
                       "Material"=ifelse(name!='subsets', gsub("[^0-9]", "", row.names(tdata)[x]), nameMaterial) 
      )
      }))
      }
     
     rbind.data.frame(a,b,c)
     
      }
     
      paramBayesian <- rbind(
      extractParamsBayesian(full, "Full"),
      do.call(rbind,lapply(seq_along(subSampled), function(x){
        if(!is.null(subSampled[[x]])){
        extractParamsBayesian(listBayesian=subSampled[[x]], name='subsets', names(subSampled)[x])
        }
      }))
      )
      
      params <- rbind(paramBayesian,paramNonBayesian)
      params <- params[with(params, order(params$Dataset, params$model, params$Material)), ]
      colnames(params)
      params <- params[,c(1,2,7,3:6)]
      
      ##Convergence
      
      Convergence = rbindlist(list(
        "Full"=
      rbindlist(lapply(full$Bayesian, function(x){
        as.data.frame(x$BUGSoutput$summary)
      }), idcol = "Model"),
      "Subsets"=
      rbindlist( lapply(subSampled, function(y){
        rbindlist(lapply(y$Bayesian, function(x){
          as.data.frame(x$BUGSoutput$summary)
        }), idcol = "Model")
      }), idcol = 'Material')
      ), fill=T, idcol = "Dataset")
      
      key <- as.data.frame(table(key))
      key <- key[key$Freq != 0,]
      
      toRet <- list(
        'params'=params,
        'DICs'=DICs,
        'Convergence'=Convergence,
        'key'=key
      )
      
      return(toRet)
    })
    names(sumPart) <- targetColumns
    

    sumPars <- rbindlist(lapply(sumPart, function(x) x$params), idcol = 'targetColumns')
    DICs <- rbindlist(lapply(sumPart, function(x) x$DICs), idcol = 'targetColumns')
    Conv <- rbindlist(lapply(sumPart, function(x) x$Convergence), idcol = 'targetColumns')
    keys<-rbindlist(lapply(sumPart, function(x) x$key), idcol = 'targetColumns')
    

    condensed <-
      list(
        ParameterSummary = sumPars,
        DICs = DICs,
        keys = keys,
        Conv = Conv
      )
    
    if (export == T) {
      dir.create('Results', recursive = T, showWarnings = F)
      invisible(lapply(seq_along(condensed), function(x)
        write.csv(
          condensed[[x]],
          paste0("Results/", prefix, '_', names(condensed)[x], '.csv')
        )))
    } else{
      return(condensed)
    }
  }
