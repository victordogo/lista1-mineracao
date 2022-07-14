# ITEM 1

set.seed(727838)

library(tidyverse)
library(rsample)
library(glmnet)
library(ggrepel)
library(forcats)

df <- ISLR::Carseats
df$US <-  as.factor(df$US)
df$Urban <- as.factor(df$Urban)
df$ShelveLoc <- as.factor(df$ShelveLoc)

# Divisao treino e teste

split <- initial_split(df, prop=0.6)

tre <- training(split)
tes <- testing(split)

x_tre <- model.matrix(Sales~., tre)
y_tre <- tre[,1]

x_tes <- model.matrix(Sales~., tes)
y_tes <- tes[,1]

# ITEM 2

## Ajuste Minimos Quadrados

ajuste_mq <- glmnet(x_tre, y_tre, alpha=0, lambda=0)

## Ajuste Lasso

cv_lasso <- cv.glmnet(x_tre, y_tre, alpha=1)
ajuste_lasso <- glmnet(x_tre, y_tre, alpha=1, lambda = cv_lasso$lambda.1se)

## Erro x Lambda Lasso

tibble(
  lambda=cv_lasso$lambda,
  risco=cv_lasso$cvm
) |>
  ggplot()+
  aes(x=lambda, y=risco)+
  geom_line()+
  geom_vline(xintercept = cv_lasso$lambda.1se)+
  annotate(geom = 'text', y=5, x=0.25,
           label=paste0('lambda = ', round(ajuste_lasso$lambda,5)))+
  theme_minimal()+
  labs(title='Risco x Lambda estimado',
       subtitle = 'com Lambda escolhido')

## Apresentando melhor lambda (cv_lasso$lambda.1se)

lambdas <- cv_lasso$glmnet.fit$lambda

lam <- lambdas |>
  as.data.frame() |>
  mutate(penalty=names(cv_lasso$glmnet.fit$a0)) %>%
  rename(lambda=1)

results <- cv_lasso$glmnet.fit$beta |>
  as.matrix() |>
  as.data.frame() |>
  rownames_to_column() |>
  gather(penalty,coefficients,-rowname) |>
  left_join(lam)

result_labels <- results |>
  group_by(rowname) |>
  filter(lambda==ajuste_lasso$lambda) |>
  ungroup()

ggplot()+
  geom_line(data=results, aes(lambda, coefficients,
                              group=rowname, color=rowname), show.legend = FALSE)+
  scale_x_log10()+
  geom_text(data=result_labels, aes(0.01, coefficients,
                                    label=rowname, color=rowname),
            nudge_x=-.06, show.legend = FALSE)+
  geom_vline(xintercept = ajuste_lasso$lambda)+
  annotate(geom = 'text', y=0.4, x=0.16,
           label=paste0('lambda = ', round(ajuste_lasso$lambda,5)))+
  theme_minimal()

# ITEM 3

coef_mq <- coefficients(ajuste_mq) |>
  round(3)

coef_lasso <- coefficients(ajuste_lasso) |>
  round(3)

tibble(
  var=names(coef_mq[,1]),
  val=coef_mq[,1]
) |>
  mutate(sinal=ifelse(val<0, 'negativo', 'positivo'),
         var=fct_reorder(var, val)) |>
  arrange(val) |>
  filter(var!='(Intercept)') |>
  ggplot()+
  aes(x=val, y=var, fill=sinal)+
  geom_bar(stat='identity', show.legend = FALSE)+
  theme_minimal()+
  labs(x='Estimativa', y='',
       title='Estimativas do Ajuste de Mínimos Quadrados')

tibble(
  var=names(coef_lasso[,1]),
  val=coef_lasso[,1]
) |>
  mutate(sinal=ifelse(val<0, 'negativo', 'positivo'),
         var=fct_reorder(var, val)) |>
  arrange(val) |>
  filter(var!='(Intercept)') |>
  ggplot()+
  aes(x=val, y=var, fill=sinal)+
  geom_bar(stat='identity', show.legend = FALSE)+
  theme_minimal()+
  labs(x='Estimativa', y='',
       title='Estimativas do Ajuste Lasso')

# ITEM 4

funcao_risco <- function(y_pred, y_obs){
  w <- (y_pred-y_obs)^2
  sigma <- var(w)
  risco <- mean(w)
  liminf <- risco - (2*sqrt((1/length(w))*sigma))
  limsup <- risco + (2*sqrt((1/length(w))*sigma))

  return(tibble(risco, liminf, limsup))
}

y_pred_mq <- predict(ajuste_mq, x_tes)
y_pred_lasso <- predict(ajuste_lasso, x_tes)

risco_mq <- funcao_risco(y_pred_mq, y_tes)
risco_lasso <- funcao_risco(y_pred_lasso, y_tes)

tibble(
  Estimativa=c('Risco', 'Limite Inferior', 'Limite Superior'),
  `Mínimos Quadrados`=unlist(c(risco_mq)),
  `Lasso`=unlist(c(risco_lasso))
)

