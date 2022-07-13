# ITEM 1

set.seed(727838)

library(tidyverse)
library(rsample)
library(glmnet)
library(ggrepel)

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

# x_tre <- model.matrix(~.-1, tre[,-1])
# y_tre <- tre[,1] |> pull()
#
# x_tes <- model.matrix(~.-1, tes[,-1])
# y_tes <- tes[,1] |> pull()

# ITEM 2

## Ajuste Minimos Quadrados

ajuste_mq <- glmnet(x_tre, y_tre, alpha=0, lambda=0)

## Ajuste Lasso

cv_lasso <- cv.glmnet(x_tre, y_tre, alpha=1)
ajuste_lasso <- glmnet(x_tre, y_tre, alpha=1, lambda = cv_lasso$lambda.1se)

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
  annotate(geom = 'text', y=0.15, x=0.3,
           label=paste0('lambda = ', round(ajuste_lasso$lambda,5)))+
  theme_minimal()

# ITEM 3

coefficients(ajuste_mq) |>
  round(3)

coefficients(ajuste_lasso) |>
  round(3)
