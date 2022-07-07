# ITEM 1

set.seed(727838)

library(tidyverse)
library(rsample)
library(glmnet)

df <- ISLR::Carseats
df$ShelveLoc <- ifelse(df$ShelveLoc=='Bad', as.factor(0),
                       ifelse(df$ShelveLoc=='Medium',as.factor(1), as.factor(2)))
df$Urban <- ifelse(df$Urban=='No',as.factor(0),as.factor(1))
df$US <- ifelse(df$US=='No',as.factor(0),as.factor(1))


split <- initial_split(df, prop=0.6)

tre <- training(split)
tes <- testing(split)

x_tre <- tre[,-1] |> as.matrix()
y_tre <- tre[,1]

x_tes <- tes[,-1] |> as.matrix()
y_tes <- tes[,1]

# ITEM 2

ajuste_mq <- glmnet(x_tre, y_tre, alpha=0, lambda=0)

cv_lasso <- cv.glmnet(x_tre, y_tre, alpha=1)
ajuste_lasso <- glmnet(x_tre, y_tre, alpha=1)



