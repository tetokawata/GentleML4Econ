set.seed(1)
library(tidyverse)

Data = read_csv("Public.csv")

Group = sample(
  1:nrow(Data),
  round(nrow(Data)/2)
)

Train = Data[Group,]
Test = Data[-Group,]

ModelY = ranger::ranger(
  Price ~ Size + StationDistance + BuildYear,
  Train)

PredY = predict(ModelY,Test)

ModelD = ranger::ranger(
  Reform ~ Size + StationDistance + BuildYear,
  Train)

PredD = predict(ModelD,Test)

Test$ResY = Test$Price - PredY$predictions
Test$ResD = Test$Reform - PredD$predictions

estimatr::lm_robust(ResY ~ ResD, Test) # R Learner

estimatr::lm_robust(Price ~ Reform, Data) # Simple Difference

estimatr::lm_robust(Price ~ Reform + Size + StationDistance + BuildYear, Data) # OLS

hist(Data$Size)

estimatr::lm_robust(ResY ~ ResD + 
                      ResD:scale(Size) + 
                      ResD:scale(BuildYear) + 
                      ResD:scale(StationDistance),  
                    Test)

## 13時55分再開

