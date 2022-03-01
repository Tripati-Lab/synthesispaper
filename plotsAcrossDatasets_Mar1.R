library(data.table)
library(ggplot2)

files <- list.files("Results/Results_Feb26", pattern = "ParameterSummary.csv", full.names = T)[-9]
filesCSV<- lapply(files, read.csv)
names(filesCSV) <- files
ds2<- rbindlist(filesCSV, idcol="File")
ds2 <- ds2[ds2$dataset == 'Full',]


##Distributions

p <- ggplot(data=ds2)+
  geom_density(aes(x=intercept, fill=File, color=File), alpha=0.5) +
  geom_density(aes(x=intercept), alpha=0.5)+
  facet_wrap(~model, scales = 'free_y')

q <- ggplot(data=ds2)+
  geom_density(aes(x=slope, fill=File, color=File), alpha=0.5) +
  geom_density(aes(x=slope), alpha=0.5)+
  facet_wrap(~model, scales = 'free_y')

pdf('PlotsResults/FullAcrossRuns_intercept.pdf', 20, 10)
print(p)
dev.off()

pdf('PlotsResults/FullAcrossRuns_slope.pdf', 20, 10)
print(q)
dev.off()


