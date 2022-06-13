library(data.table)

files <- list.files("Results", full.names = TRUE, recursive = FALSE)
fileNames <- list.files("Results", full.names = FALSE, recursive = FALSE)

filesFull <- lapply(files, read.csv)
names(filesFull) <- fileNames

##Create full dataset
tFiles <- filesFull[grep("Parameter", fileNames)]
fDS <- lapply(tFiles, function(x) x[is.na(x$Material),])
pg1 <- rbindlist(fDS)[,-c(1,5)]

##Create column-level datasets
tDs <- unique(sapply(strsplit(fileNames, "_"), `[[`, 2))

Procesed <- lapply(tDs, function(x){
  tFiles <- filesFull[grep(x, fileNames)]
  key <- tFiles[[grep("keys", names(tFiles))]]
  dic <- tFiles[[grep("DICs", names(tFiles))]]
  par <- tFiles[[grep("Parameter", names(tFiles))]]
  par <- merge(key, par, by.x = "number", by.y = 'Material', all = TRUE)
  par.full <- par[is.na(par$Freq),-c(1:6)]
  par <- par[!is.na(par$original),-c(1,2,3,6,7)]
  dic <- merge(key, dic, by.x = "number", by.y = 'Material', all = TRUE)[,-c(1,2,3,6,7)]
  dic[is.na(dic$original),1] <- "Full"
  
  par <- par[order(par$original),]
  colnames(par)[1] <- "Material"
  colnames(dic)[1] <- "Material"
  
  list('Full'= par.full, 'Subsets' = par, 'DIC'= dic)
  
})
names(Procesed) <- tDs

##Export
dir.create("Processed")
write.csv(pg1, "Processed/Full.csv")
lapply(seq_along(Procesed), function(y){
  dir.create(paste0("Processed/", names(Procesed)[y]))
  lapply(seq_along(Procesed[[y]]), function(x){
    write.csv(Procesed[[y]][[x]], paste0("Processed/", names(Procesed)[y], "/", names(Procesed[[y]])[x], ".csv"))
  })
})

