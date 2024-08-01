library(tidyverse)

Data = read_csv("Public.csv")

Model = rpart::rpart(
  Price ~ Size + BuildYear + District,
  Data
)

rpart.plot::rpart.plot(Model)

ModelShort = rpart::rpart(
  Price ~ Size + BuildYear + District,
  Data,
  control = rpart::rpart.control(
    maxdepth = 2, # Set maximum number of split
    cp = 0 # no pruning
  )
)

rpart.plot::rpart.plot(ModelShort)


# rpart.plot をインストール

# ctr(command) + A -> ctr(command) + Enter

# 10時12分再開