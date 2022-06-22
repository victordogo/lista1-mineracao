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


