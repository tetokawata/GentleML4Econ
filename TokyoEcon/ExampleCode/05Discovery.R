set.seed(1)
library(tidyverse)

Data = read_csv("Public.csv")

Group = sample(
  1:nrow(Data),
  round(nrow(Data)/2)
)

Train = Data[Group,]
Test = Data[-Group,]

Train_D1 = Data[Group & Data$Reform == 1,]
Train_D0 = Data[Group & Data$Reform == 0,]

ModelY = ranger::ranger(
  Price ~ Size + BuildYear + District + StationDistance,
  Train
)
ModelD = ranger::ranger(
  Reform ~ Size + BuildYear + District + StationDistance,
  Train
)

ModelY_D1 = ranger::ranger(
  Price ~ Size + BuildYear + District + StationDistance,
  Train_D1
)

ModelY_D0 = ranger::ranger(
  Price ~ Size + BuildYear + District + StationDistance,
  Train_D0
)

Test$PredY = predict(ModelY, Test)$predictions
Test$PredD = predict(ModelD, Test)$predictions
Test$PredY_D1 = predict(ModelY_D1, Test)$predictions
Test$PredY_D0 = predict(ModelY_D0, Test)$predictions
Test$PredTau = Test$PredY_D1 - Test$PredY_D0

Test = mutate(Test,
              ResY = Price - PredY,
              ResD = Reform - PredD)

estimatr::lm_robust(ResY ~ ResD, Test)

Cutoff = quantile(Test$PredTau, probs = 0.8)

estimatr::lm_robust(ResY ~ ResD, Test, subset = PredTau > Cutoff)
estimatr::lm_robust(ResY ~ ResD, Test, subset = PredTau < Cutoff)






