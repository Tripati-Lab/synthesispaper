library(ggplot2)
synData <-
  read_sheet(
    "https://docs.google.com/spreadsheets/d/1PZ_o0lA-bpOGG9e76o4bGQWjvMnqgH56aWXmdxVkr-k/edit?usp=sharing"
  )
targetColumns <- colnames(synData)[c(11:20)]

dir.create('RawPlots', recursive = T, showWarnings = F)

for (x in 1:length(targetColumns)) {
  pdf(paste0("RawPlots/", targetColumns[x], '.pdf'))
  group <- factor(unlist(synData[, targetColumns[x]]))
  
  print(
    ggplot(synData, aes(
      y = D47, x = Temperature, color = group
    )) +
      geom_errorbar(aes(
        ymin = D47 - D47error, ymax = D47 + D47error
      )) +
      geom_errorbarh(
        aes(xmin = Temperature - TempError, xmax = Temperature + TempError)
      ) +
      geom_point(alpha = 0.5, size = 2) +
      ggtitle(targetColumns[x]) +
      theme_bw()
  )
  
  for (q in 1:length(unique(group))) {
    y = unique(group)[q]
    print(
      ggplot(synData[group == y, ], aes(y = D47, x = Temperature)) +
        geom_errorbar(aes(
          ymin = D47 - D47error, ymax = D47 + D47error
        )) +
        geom_errorbarh(
          aes(xmin = Temperature - TempError, xmax = Temperature + TempError)
        ) +
        geom_point(size = 2) +
        ggtitle(paste0(targetColumns[x], ', group: ', y)) +
        theme_bw()
    )
  }
  dev.off()
}
