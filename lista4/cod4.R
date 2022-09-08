tre_nn <- tre |>
  mutate(
    across(age_quant:rdw, .fns=scale),
    result=as.factor(ifelse(result==0, 'negativo', 'positivo'))
  )

tes_nn <- tes |>
  mutate(
    across(age_quant:rdw, .fns=scale),
    result=as.factor(ifelse(result==0, 'negativo', 'positivo'))
  )

nnetGrid <- expand.grid(.size=7,
                        .decay=c(0,.01,.1,.2,.3,.4,.5,1,2))

ctrl <- trainControl(method='repeatedcv',
                     repeats=5,
                     classProbs = TRUE)

nnetFit <- train(result~.,
                 data=tre_nn,
                 method='nnet',
                 metric='Accuracy',
                 tuneGrid=nnetGrid,
                 trControl=ctrl,
                 maxit=1000)

library(xgboost)

set.seed(57)

dtrain <- xgb.DMatrix(data = as.matrix(tre[,-1]), label = (as.numeric(pull(tre[,1]))-1))
bstDMatrix <- xgboost(data = dtrain, max.depth = 2, eta = 1, nthread = 2, nrounds = 1000, objective = "binary:logistic")

acc <- mean(as.numeric(pred > 0.5) == (as.numeric(pull(tes[,1]))-1))
print(paste("test-acc=", acc))
