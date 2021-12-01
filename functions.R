##Functions are sourced from BayClump

#source(
#  "https://raw.githubusercontent.com/Tripati-Lab/BayClump/main/Functions/fitClumpedRegressions.R"
#)
source(
  "https://raw.githubusercontent.com/Tripati-Lab/BayClump/dev/Functions/fitClumpedRegressions.R"
)
source(
  "https://raw.githubusercontent.com/Tripati-Lab/BayClump/main/Functions/clumPipe.R"
)
source(
  "https://raw.githubusercontent.com/Tripati-Lab/BayClump/main/Functions/getR2Bayesian.R"
)
source(
  "https://raw.githubusercontent.com/Tripati-Lab/BayClump/dev/Functions/functionsSimulationsMixed.R"
)
source(
  "https://raw.githubusercontent.com/Tripati-Lab/BayClump/main/Functions/functionsRegressionCI.R"
)
source(
  "https://raw.githubusercontent.com/Tripati-Lab/BayClump/main/Functions/functionsRegressionCI.R"
)

##Basic functions

fitsingleDataset <- function(data,
                             replicates = 2,
                             generations = 1000,
                             isMixed=T) {
  ##Models
  a <- simulateYork_measured(data = data, replicates = replicates)
  b <- simulateLM_measured(data = data, replicates = replicates)
  c <- simulateDeming(data = data, replicates = replicates)
  d <- simulateLM_inverseweights(data = data, replicates = replicates)
  e <-
    simulateBLM_measuredMaterial(
      data = data,
      replicates = replicates,
      generations = generations,
      isMixed = isMixed
    )
  
  SumTable <- rbind.data.frame(
    cbind(model = 'York', a, material = NA),
    cbind(model = 'LM', b, material = NA),
    cbind(model = 'Deming', c, material = NA),
    cbind(model = 'Weighted', d, material = NA),
    cbind(
      model = 'Bayesian errors',
      e$BLM_Measured_errors,
      material = NA
    ),
    cbind(
      model = 'Bayesian no errors',
      e$BLM_Measured_no_errors,
      material = NA
    ),
    make.row.names = F
  )
  
  if( isMixed ) {
    SumTable <- rbind(SumTable,
                      cbind(model = 'Bayesian mixed', e$BLMM_Measured_errors)
    )
  }
  
  attr(SumTable, "DICs") <- attr(e, "DICs")
  attr(SumTable, "R2s") <- attr(e, "R2s")
  attr(SumTable, "Conv") <- attr(e, "Conv")
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
      dt1 <-
        fitsingleDataset(data = calData,
                         replicates = replicates,
                         generations = generations, isMixed = F)
      
      dt <-
        fitsingleDataset(data = calDataSelected,
                         replicates = replicates,
                         generations = generations)
      
      
      #Partitioned by level
      subSampled <-
        lapply(unique(calDataSelected$Material), function(y) {
          calDataSelectedgroup <-
            calDataSelected[calDataSelected$Material == y, ]
          keep = T
          i = 0
          k = 0
          while (keep) {
            keep = if (i == 0) {
              if (k > maxtry) {
                F
              } else{
                T
              }
            } else{
              F
            }
            k = k + 1
            tryCatch({
              resDS <-
                invisible(
                  fitsingleDataset(
                    calDataSelectedgroup,
                    replicates = replicates,
                    generations = generations,
                    isMixed = F
                  )
                )
              i = 1
              return(resDS)
            }, error = function(e) {
            })
          }
        })
      
      names(subSampled) <- unique(calDataSelected$Material)
      
      dtT <- cbind(.id = 'Full', dt)
      dtT <- dtT[dtT$model == "Bayesian mixed",]

      fullDS <- rbind(cbind(.id = 'Full', dt1) ,dtT,
                      rbindlist(subSampled, idcol = T))
      
      R2s <- lapply(subSampled, function(x)
        attr(x, 'R2s'))
      
      r2M<-attr(dt, "R2s")
      r2M <- r2M[r2M$Group.2 == 'BLM3_fit',]
      r2nM <- attr(dt1, "R2s")
      r2nM$Group.2 <- NA
      r2B <-rbind(r2M,r2nM)
      
      R2s <- list(r2B, R2s)
      
      DICs <- lapply(subSampled, function(x)
        attr(x, 'DICs'))
      
      DICm<-attr(dt, "DICs")[3,]
      DICnm<-attr(dt1, "DICs")
      
      DICsB <- rbind(DICnm,DICm)
      DICs <- list(DICsB, DICs)
      
      ##Now combine convergence
      ###For part
      Conv <- lapply(subSampled, function(x)
        attr(x, 'Conv') )
      names(Conv) <- names(subSampled)
      Conv <- Filter(Negate(is.null), Conv)
      
      rawConv <- lapply(seq_along(Conv),  function(y){
        x<-list.flatten(Conv[[y]])
        test<-lapply(seq_along(x), function(z){
          par<- as.data.frame(x[[z]])
          cbind.data.frame(parameter=row.names(par),par)
        })
        names(test) <-names(x)
        rbindlist(test, fill=T, idcol='Model', use.names=T)
        
      })
      
      names(rawConv) <- names(Conv)
      
      
      ###For the full
      
      fl<-lapply(list.flatten(attr(dt, "Conv")), function(z){
        par<- as.data.frame(z)
        cbind.data.frame(parameter=row.names(par),par)
      })
      fl1<-lapply(list.flatten(attr(dt1, "Conv")), function(z){
        par<- as.data.frame(z)
        cbind.data.frame(parameter=row.names(par),par)
      })
      
      fl <- fl[names(fl) == "BLM3_fit"]
      flc<-c(fl1,fl)
      
      flC<-rbindlist(flc, fill=T, idcol='Model', use.names=T)

      attr(fullDS, 'key') <- key
      attr(fullDS, 'R2s') <- R2s
      attr(fullDS, 'DICs') <- DICs
      attr(fullDS, 'Conv') <- list(flC,rawConv)
      
      return(fullDS)
    })
    
    
    #Keys
    keys <- lapply(sumPart, function(x)
      attr(x, 'key'))
    names(keys) <- targetColumns
    keys <- rbindlist(keys, idcol = T)
    samples <- as.data.frame(table(keys$original))
    colnames(samples)[1] <- 'original'
    
    samples$original <- as.numeric(samples$original)
    keys<-as.data.frame(keys)
    keys<- aggregate(numeric(nrow(keys)), keys[c(".id", "original","number")], length) 
    colnames(keys) <- c( "targetMaterialColumn",'OriginalCode',"NumericCode",'N')
    keys <- keys[order(keys$targetMaterialColumn),] 
    
    #R2s
    R2s <- lapply(sumPart, function(x)
      attr(x, 'R2s'))
    names(R2s) <- targetColumns
    R2SG <- sapply(1:length(targetColumns), function(y) {
      R2s <- lapply(R2s[[y]][-1], function(x)
        rbindlist(x, idcol = T))
    })
    R2full <- lapply(1:length(targetColumns), function(y) {
      R2s[[y]][[1]]
    })
    names(R2SG) <- targetColumns
    names(R2full) <- targetColumns
    
    R2SG <- rbindlist(R2SG, idcol = T)
    colnames(R2SG)[1] <- "targetMaterialColumn"
    R2full <- rbindlist(R2full, idcol = T)
    R2full$targetMaterialColumn <- R2full$.id
    
    R2s <- rbindlist(list(R2full, R2SG), fill = T)
    R2s <- R2s[, c(7, 1:6)]
    colnames(R2s)[c(2, 3, 4)] <- c("dataset", "R2 type", "model")
    
    
    #DICs
    DICs <- lapply(sumPart, function(x)
      attr(x, 'DICs'))
    names(DICs) <- targetColumns
    DICsG <- sapply(1:length(targetColumns), function(y) {
      DICs <- lapply(DICs[[y]][-1], function(x)
        rbindlist(x, idcol = T))
    })
    DICsfull <- lapply(1:length(targetColumns), function(y) {
      DICs[[y]][[1]]
    })
    names(DICsG) <- targetColumns
    names(DICsfull) <- targetColumns
    
    DICsG <- rbindlist(DICsG, idcol = T)
    colnames(DICsG)[1] <- "targetMaterialColumn"
    DICsfull <- rbindlist(DICsfull, idcol = T)
    DICsfull$targetMaterialColumn <- DICsfull$.id
    DICs <- rbind(DICsfull, DICsG)
    DICs <- DICs[, c(6, 1, 5, 2:4)]
    colnames(DICs)[c(2)] <- c("dataset")
    
    #Convergence (assuming one column at a time is processed)
    names(sumPart) <- targetColumns
    Conv <- lapply(sumPart, function(x)
      attr(x, 'Conv')  )
    
    comp<- list()
    comp[[1]] <- cbind.data.frame(dataset='Full', Conv[[1]][[1]])
    for( i in seq_along(Conv[[1]][[2]])){
      comp[[i+1]] <- cbind.data.frame(dataset= names(Conv[[1]][[2]])[i] , Conv[[1]][[2]][[i]])
    }
    
    Conv <- rbindlist(comp)

    
    #Parameters
    names(sumPart) <- targetColumns
    sumPart <- rbindlist(sumPart,  idcol = T)
    colnames(sumPart)[c(1, 2, 6)] <-
      c('targetMaterialColumn', 'dataset', 'BLMM_material')
    sumPart <- sumPart[, c(1, 2, 6, 3:5)]
    
    condensed <-
      list(
        ParameterSummary = sumPart,
        R2s = R2s,
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