## Lendo pacotes

set.seed(1)

library(tidyverse)
library(rsample)
library(knitr)
library(kableExtra)
library(caret)
library(FNN)
library(rpart)
library(rpart.plot)
library(randomForest)

## Lendo Dados

df <- ISLR::Carseats |>
  mutate(US=as.factor(US),
         Urban=as.factor(Urban),
         ShelveLoc=as.factor(ShelveLoc))

## Divisão entre treino e teste

split <- initial_split(df, prop=0.6)

tre <- training(split)
tes <- testing(split)

x_tre <- model.matrix(Sales~., tre)
y_tre <- tre[,1]

x_tes <- model.matrix(Sales~., tes)
y_tes <- tes[,1]

### ITEM C

# KNN

ajuste_knn <- train(
  x=x_tre,
  y=y_tre,
  method = 'knn'
)

plot(ajuste_knn)

ajuste_knn <- knn.reg(train=x_tre, test=x_tes, y=y_tre, k=9)

# Florest Aleatória

ajuste_flor <- randomForest(x=x_tre, y=y_tre, mtry = round(ncol(df)/3),
                            importance=TRUE)

plot(ajuste_flor)

varImpPlot(ajuste_flor)

# Arvore de Regressao

ajuste_arv <- rpart(Sales~., data=tre, method='anova')
melhor_cp <- ajuste_arv$cptable[which.min(ajuste_arv$cptable[,'xerror']),'CP']
ajuste_arv <- prune(ajuste_arv, cp=0.027)

rpart.plot(ajuste_arv)
