## Lendo pacotes

set.seed(1)

library(progress)
library(tidyverse)
library(rsample)
library(knitr)
library(kableExtra)
library(caret)
library(FNN)
library(rpart)
library(rpart.plot)
library(randomForest)
library(locfit)

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
  method = 'knn',
  tuneLength = 20
)

plot(ajuste_knn)

ajuste_knn <- knn.reg(train=x_tre, test=x_tes, y=y_tre, k=17)

# Floresta Aleatória

ajuste_flor <- randomForest(x=x_tre, y=y_tre, mtry = round(ncol(df)/3),
                            importance=TRUE)

plot(ajuste_flor)

varImpPlot(ajuste_flor)

# Arvore de Regressao

tre_arv <- data.frame(y_tre, x_tre[,-1])

ajuste_arv <- rpart(y_tre~., data=tre_arv, method='anova')
melhor_cp <- ajuste_arv$cptable[which.min(ajuste_arv$cptable[,'xerror']),'CP']
ajuste_arv <- prune(ajuste_arv, cp=0.027)

rpart.plot(ajuste_arv)

# Nadaraya Watson

nw_func <- function(x_tes, x_tre, y, h){

  x_tes <- scale(x_tes)
  x_tre <- scale(x_tre)

  k <- function(h,d){(1/(h*sqrt(2*pi)))*exp(-0.5* (d/h)^2)}

  dis <- fields::rdist(x_tes[,-1], x_tre[,-1])

  y_pred <- numeric(ncol(x_tes))

  for (i in 1:nrow(dis)) {

    kx <- numeric(ncol(dis))

    for (j in 1:ncol(dis)) {

      kx[j] <- k(h, dis[i,j])

    }

    wx <- kx/sum(kx)

    y_pred[i] <- sum(wx*y)

  }

  return(y_pred)

}

funcao_risco <- function(y_pred, y_obs){

  w <- (y_pred-y_obs)^2
  sigma <- var(w)
  risco <- mean(w)
  liminf <- risco - (2*sqrt((1/length(w))*sigma))
  limsup <- risco + (2*sqrt((1/length(w))*sigma))

  return(data.frame(risco, liminf, limsup))
}

melhor_h_nw <- function(x_tre, y_tre, seed=1){

  set.seed(seed)

  # Embaralhando dados

  df <- data.frame(y_tre,x_tre)
  df <- df[sample(1:nrow(df)),]

  # Definindo kfolds com k=5

  size <- round(nrow(df)/5)

  kfoldlist <- list(
    df[1:size,],
    df[(size+1):(2*size),],
    df[(2*size+1):(3*size),],
    df[(3*size+1):(4*size),],
    df[(4*size+1):(5*size),]
  )

  # Definindo vetor de h para ser testado

  h <- seq(0.4,10,0.02)

  # Dataframe com resultados

  result <- data.frame(
    h=h,
    risco=numeric(length(h))
  )

  pb <- progress_bar$new(
    total=length(h),
    format = "[:bar] :percent eta: :eta  elapsed: :elapsed")

  for (jj in 1:length(h)) {

    hresult <- numeric(5)

    for (ii in 1:5) {

      df_tre <- do.call(rbind.data.frame, kfoldlist[-ii])

      df_tes <- kfoldlist[[ii]]

      ypred <- nw_func(x_tes=df_tes[,-1],
                       x_tre=df_tre[,-1],
                       y=df_tre[,1],
                       h=h[jj])

      hresult[ii] <- funcao_risco(y_pred=ypred, y_obs=df_tes[,1])$risco[1]

    }

    result$risco[jj] <- mean(hresult)

    pb$tick()
    Sys.sleep(1 / length(h))

  }

  return(result)

}

resultados_h <- melhor_h_nw(x_tre=x_tre,y_tre=y_tre)

resultados_h |>
  ggplot()+
  aes(x=h,y=risco)+
  geom_line()+
  labs(x='H', y='Risco Estimado',
       title='H x Risco Estimado',
       subtitle = 'via validação-cruzada no conjunto de treino')+
  theme_minimal()

ajuste_nw <- nw_func(x_tes, x_tre, y_tre, h=resultados_h$h[1])

funcao_risco(ajuste_nw, y_tes)
funcao_risco(ajuste_knn$pred, y_tes)
funcao_risco(predict(ajuste_flor, x_tes), y_tes)
funcao_risco(predict(ajuste_arv, data.frame(x_tes)), y_tes)
