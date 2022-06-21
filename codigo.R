# Lendo dados

library(tidyverse)

df <- readr::read_csv('worldDevelopmentIndicators.csv') |>
  select(-CountryName) |>
  rename(y=LifeExpectancy,
         x=GDPercapita) |>
  mutate(x = (x-min(x))/(max(x)-min(x))) ## ITEM 1: Normalizando covariavel

## ITEM 2

lista_modelos <- list()

for (p in 1:30) {

}
