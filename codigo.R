# Lendo dados

library(tidyverse)

df <- readr::read_csv('lista1/worldDevelopmentIndicators.csv') |>
  select(-CountryName) |>
  rename(y=LifeExpectancy,
         x=GDPercapita) |>
  mutate(x = (x-min(x))/(max(x)-min(x))) ## ITEM 1: Normalizando covariavel

## ITEM 2

#

formulas <- list()

for (p in 1:30) {

  if(p==1){
    formulas[[p]] <- "y~sin(2*pi*x)+cos(2*pi*x)"
  } else {
    formulas[[p]] <- paste0(formulas[[p-1]],
                             "+sin(2*",p,
                             "*pi*x)+cos(2*",
                             p, "*pi*x)")
  }
}

tbl_result <- tibble(
  p=1:30,
  mse=as.double(1:30)
)



for (p in 1:length(formulas)) {

  erros <- NULL

  model <- NULL

  for (ii in 1:nrow(df)) {

    model <- lm(formulas[[p]], data=df[-ii,])

    erros <- c(erros,
               (df[ii,1] - predict(model, df[ii,]))^2)
  }

  tbl_result[p,2] <- mean(unlist(erros))

}

## ITEM 3

tbl_result |>
  ggplot()+
  aes(x=p,y=mse)+
  geom_line(size=1.25)+
  theme_minimal()+
  labs(x = 'P', y = "Mean Squared Error",
       title='Estimativa da Função de Risco x Valor de P')

tbl_result |>
  ggplot()+
  aes(x=p,y=mse)+
  geom_line(size=1.25)+
  geom_label(aes(label=paste0('mse = ', round(mse,3))))+
  coord_cartesian(ylim=c(40,50), xlim=c(1,6))+
  scale_x_continuous(breaks = 1:6)+
  theme_minimal()+
  labs(x = 'P', y = "Mean Squared Error",
       title='Estimativa da Função de Risco x Valor de P')

