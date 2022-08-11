library(ggplot2)
source(
  "https://raw.githubusercontent.com/Tripati-Lab/BayClump/main/Functions/Calibration_BayesianNonBayesian.R"
)

b = 0.039
b_sd = 0.0002
a = 0.154
a_sd = 0.0031

ds <- cbind.data.frame("beta" = rnorm(100, b, b_sd), "alpha"=rnorm(100, a, a_sd))
raw_ds_ci <- RegressionSingleCI(ds, 1, 10)[[1]]

ggplot(raw_ds_ci, aes(x = x))+
  geom_line(aes(y = median_est)) + 
  geom_line(aes(y = ci_lower_est), lty = 2, col='grey') +
  geom_line(aes(y = ci_upper_est), lty = 2, col='grey') +
  theme_bw()

write.csv(raw_ds_ci, "raw_ds_ci.csv")

