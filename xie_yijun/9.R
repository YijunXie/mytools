library(gapminder)
library(tidyverse)

## save existing parameer settings
opar = par(pch = 19)

jdat = gapminder %>% 
  filter(country = c("Eritrea","Nepal","Chad","Jamaica",
                     "Cuba","Costa Rica","Norway","Germany"))

## restore parameter settings
par(opar)