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
params <- list(booster = "gbtree",
               objective = "binary:logistic", eta=0.3, gamma=0,
               max_depth=6, min_child_weight=1, subsample=1,
               colsample_bytree=1)

xgbcv <- xgb.cv(params = params, data = dtrain, nrounds = 100,
                          nfold = 5, showsd = T, stratified = T,
                          print_every_n = 10, early_stopping_rounds = 20,
                          maximize = F, eval_metric='error')

set.seed(1)
eta <- seq(0,20,0.1)
conv_eta <-  data.frame(eta, error=numeric(length(eta)))
for(i in 1:length(eta)){
  set.seed(57)
  params=list(booster='gbtree', eta = eta[i], colsample_bytree=1,
              subsample = 1, max_depth = 6,
              objective='binary:logistic', gamma=0, eval.metric='error')
  xgb=xgboost(data=dtrain, nrounds = 17, params = params)
  conv_eta[i,2] = xgb$evaluation_log$train_error[17]
}

conv_eta = data.frame(iter=1:500, conv_eta)
conv_eta = melt(conv_eta, id.vars = "iter")
ggplot(data = conv_eta) + geom_line(aes(x = iter, y = value, color = variable))

bstDMatrix <- xgboost(data = dtrain, max.depth = 2, eta = 1, nthread = 2, nrounds = 1000, objective = "binary:logistic")

pred <- predict(bstDMatrix, as.matrix(tes[,-1]), type='prob')

acc <- mean(as.numeric(pred > 0.5) == (as.numeric(pull(tes[,1]))-1))
print(paste("test-acc=", acc))

pred_lasso <- predict(ajuste_lasso, x_tes, type='response') |> c()

pred_knn <- predict(ajuste_knn, x_tes, type='prob')[,2] |> c()

pred_arv <- predict(ajuste_arv, tes, type='prob')[,2] |>
  c()

pred_flor <- predict(ajuste_flor, x_tes, type='prob')[,2] |>
  c()

pred_nn <- predict(ajuste_nn, tes_nn, type='prob')[,2] |>
  c()

pred_xgb <- predict(ajuste_xgb, dtes, type='prob')

pred = prediction(pred_arv, tes$result)
perf = performance(pred,"tpr","fpr")
plot(perf)

TPRfromROCR = unlist(perf@y.values)
FPRfromROCR = unlist(perf@x.values)

plot(TPRfromROCR~FPRfromROCR, col="royalblue",
     main="Curva ROC",type="o", pch=16, xlab = "Taxa de Falso Positivos",
     ylab = "Taxa de Verdadeiro Positivos");abline(0,1);grid()
