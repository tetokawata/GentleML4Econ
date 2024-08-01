set.seed(1)
library(tidyverse)

Data = read_csv("Public.csv")

estimatr::lm_robust(
  Price ~ Reform,
  Data
)

estimatr::lm_robust(
  Price ~ Reform,
  Data,
  alpha = 0.005
)
