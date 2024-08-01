set.seed(1)
library(tidyverse)

Data = read_csv("Public.csv")
Group = sample(
  1:2732,
  1400
)
Train = Data[Group,]
Test = Data[-Group,]

Model = rpart::rpart(
  Price ~ .,
  Train)

Pred = predict(Model, Test)
mean((Test$Price - Pred)^2)

rpart.plot::rpart.plot(Model)

# Bad Example

Pred = predict(Model, Train)
mean((Train$Price - Pred)^2)

## Too long bad

ModelLong = rpart::rpart(
  Price ~ .,
  Train,
  control = rpart::rpart.control(
    maxdepth = 30, # Set maximum number of split
    cp = 0 # no pruning
  )
)

Pred = predict(ModelLong, Test)
mean((Test$Price - Pred)^2)

# Random Forest (ranger)

ModelRF = ranger::ranger(Price ~ ., 
                         Train)

Pred = predict(ModelRF, Test)
mean((Test$Price - Pred$predictions)^2)

# ctr (command) + s
# ctr(command) + A -> ctr(command) + Enter

# 10時12分再開