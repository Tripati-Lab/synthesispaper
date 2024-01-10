#Read the dataset
synData <- read.csv('RawData/Current List_Daeron_Jan10_2024.csv')

##Basic functions

fitsingleDataset <- function(data,
                             replicates = 2) {
  ##Models
  a <- cal.york(data = data, replicates = replicates)
  b <- cal.ols(data = data, replicates = replicates)
  c <- cal.deming(data = data, replicates = replicates)
  d <- cal.wols(data = data, replicates = replicates)

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
           replicates = 100 ,
           generations = 20000,
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
      
        "Bayesian"= cal.bayesian(calibrationData=calDataSelected, 
                                 priors = "Uninformative")
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
                calDataSelectedgroup,
                replicates = replicates
              ),
              "Bayesian"=cal.bayesian(calibrationData=calDataSelectedgroup, 
                                               priors = "Uninformative")
              )
            )
        })
      
      names(subSampled) <- unique(calDataSelected$Material)
      
      ##Extract DICs
      a <- cbind.data.frame("Material"="Full", attr(full$Bayesian,"loo"))[,1:3]
      a <- data.frame(Model = row.names(a), a)
      
      DICs <- rbind.data.frame(
      a,
      rbindlist(lapply(subSampled, function(x){
       a <- data.frame(attr(x$Bayesian,"loo"))[,1:2]
      data.frame(Model = row.names(a), a)
      } ), idcol = "Material")
      )
      
      row.names(DICs) <- NULL

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
        
        BLM1_fit <- rstan::summary(listBayesian$Bayesian$BLM1_fit)$summary
        BLM3_fit <- rstan::summary(listBayesian$Bayesian$BLM3_fit)$summary
        BLM1_fit_NoErrors <- rstan::summary(listBayesian$Bayesian$BLM1_fit_NoErrors)$summary
        
        BLM1_fit <- BLM1_fit[-grep("log_lik", row.names(BLM1_fit)),]
        BLM3_fit <- BLM3_fit[-grep("log_lik", row.names(BLM3_fit)),]
        BLM1_fit_NoErrors <- BLM1_fit_NoErrors[-grep("log_lik", row.names(BLM1_fit_NoErrors)),]
        
        BLM1_fit <- head(BLM1_fit, -2);  BLM3_fit<- head(BLM3_fit, -2);  BLM1_fit_NoErrors<-  head(BLM1_fit_NoErrors, -2)
        
        a <- cbind.data.frame("Dataset"=name, 
                              "model"='BLM1_fit',
                              "meanBeta"=BLM1_fit[2,1],
                              "sdBeta"=BLM1_fit[2,3],
                              "meanAlpha"=BLM1_fit[1,1], 
                              "sdAlpha"=BLM1_fit[1,3], "Material"=ifelse(name!='subsets', NA, nameMaterial)
        )
        
        b <- cbind.data.frame("Dataset"=name, 
                              "model"='BLM1_fit_NoErrors',
                              "meanBeta"=BLM1_fit_NoErrors[2,1],
                              "sdBeta"=BLM1_fit_NoErrors[2,3],
                              "meanAlpha"=BLM1_fit_NoErrors[1,1], 
                              "sdAlpha"=BLM1_fit_NoErrors[1,3], "Material"=ifelse(name!='subsets', NA, nameMaterial)
        )
        
        nmat <- nrow(BLM3_fit)/2
        
        c <- if(nmat == 1 ){
          
          cbind.data.frame("Dataset"=name, 
                           "model"='BLM3',
                           "meanBeta"=BLM3_fit[2,1],
                           "sdBeta"=BLM3_fit[2,3],
                           "meanAlpha"=BLM3_fit[1,1], 
                           "sdAlpha"=BLM3_fit[1,3], "Material"=ifelse(name!='subsets', NA, nameMaterial)
          )
          
        }else{
          
          BLM3_fit_beta <- BLM3_fit[grep("beta", row.names(BLM3_fit)),]
          BLM3_fit_alpha <- BLM3_fit[-grep("beta", row.names(BLM3_fit)),]
          
          do.call(rbind, lapply(1:nmat, function(x){
            cbind.data.frame("Dataset"=name, 
                             "model"='BLM3',
                             "meanBeta"=BLM3_fit_beta[x,1],
                             "sdBeta"=BLM3_fit_beta[x,3],
                             "meanAlpha"=BLM3_fit_alpha[x,1], 
                             "sdAlpha"=BLM3_fit_alpha[x,3], 
                             "Material"=ifelse(name!='subsets', gsub("[^0-9]", "", row.names(BLM3_fit)[x]), nameMaterial) 
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
      params <- params[,c(1,2,7,3:6)]
      
      key <- as.data.frame(table(key))
      key <- key[key$Freq != 0,]
      
      toRet <- list(
        'params'=params,
        'DICs'=DICs,
        'key'=key
      )
      
      return(toRet)
    })
    names(sumPart) <- targetColumns
    

    sumPars <- rbindlist(lapply(sumPart, function(x) x$params), idcol = 'targetColumns')
    DICs <- rbindlist(lapply(sumPart, function(x) x$DICs), idcol = 'targetColumns')
    keys<-rbindlist(lapply(sumPart, function(x) x$key), idcol = 'targetColumns')
    

    condensed <-
      list(
        ParameterSummary = sumPars,
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
