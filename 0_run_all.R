#This script is used to run all the analyses in this folder
library(pbapply)
targetScripts <- list.files(pattern = "Script_fitMultipleDatasets")
file.remove("Error.txt")
pblapply(targetScripts, function(x){
  tryCatch({
    source(x)
  }, error=function(e){ 
    write(x, "Error.txt", append = TRUE)
    })
})
