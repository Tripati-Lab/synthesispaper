##Functions are sourced from BayClump

source(
  "https://raw.githubusercontent.com/Tripati-Lab/BayClump/main/Functions/fitClumpedRegressions.R"
)
source(
  "https://raw.githubusercontent.com/Tripati-Lab/BayClump/main/Functions/clumPipe.R"
)
source(
  "https://raw.githubusercontent.com/Tripati-Lab/BayClump/main/Functions/getR2Bayesian.R"
)
source(
  "https://raw.githubusercontent.com/Tripati-Lab/BayClump/main/Functions/functionsSimulationsMixed.R"
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
      
      fullDS <- rbind(cbind(.id = 'Full', dt) ,
                      rbindlist(subSampled, idcol = T))
      
      R2s <- lapply(subSampled, function(x)
        attr(x, 'R2s'))
      R2s <- list(attr(dt, "R2s"), R2s)
      
      DICs <- lapply(subSampled, function(x)
        attr(x, 'DICs'))
      DICs <- list(attr(dt, "DICs"), DICs)
      
      attr(fullDS, 'key') <- key
      attr(fullDS, 'R2s') <- R2s
      attr(fullDS, 'DICs') <- DICs
      
      return(fullDS)
    })
    
    #Keys
    keys <- lapply(sumPart, function(x)
      attr(x, 'key'))
    names(keys) <- targetColumns
    keys <- rbindlist(keys, idcol = T)
    keys <- keys[!duplicated(keys[, 1:3]), ]
    colnames(keys)[1] <- "targetMaterialColumn"
    
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
        keys = keys
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