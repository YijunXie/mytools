Homework\_05\_Factor\_and\_figure\_management
================
Yijun Xie
October 21, 2016

``` r
# load files and packages
library(gapminder)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(forcats)
library(gridExtra)
```

Factor management
=================

drop factors
------------

``` r
# drop Oceania
levels(gapminder$continent)
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"   "Oceania"

``` r
dat = gapminder %>% 
  filter(continent != "Oceania") %>% 
  droplevels()

# Oceania is dropped
levels(dat$continent)
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"

``` r
# further check if all rows related to Oceania is dropped
(length(unique(gapminder$year)) * 
  length(unique(gapminder[gapminder$continent == "Oceania",]$country))
  == nrow(gapminder) - nrow(dat))
```

    ## [1] TRUE

reorder and arrange
-------------------

``` r
# Select data for countries in Africa 
# whose life expectancy is less than 50 years in 2007.
# using arrange()
# before arrange
dat_arr_before = dat %>% 
  filter(continent == "Africa" & lifeExp <  50) %>% 
  filter(year == 2007) %>% 
  droplevels()

p1 = dat_arr_before %>% ggplot(mapping = aes(x = gdpPercap, y = country)) +
  geom_point(aes(size = gdpPercap))
# after arrange
dat_arr_after = dat %>% 
  filter(continent == "Africa" & lifeExp <  50) %>% 
  arrange(desc(gdpPercap)) %>% 
  filter(year == 2007)%>% 
  droplevels()

p2 = dat_arr_after %>% ggplot(mapping = aes(x = gdpPercap, y = country)) +
  geom_point(aes(size = gdpPercap))

grid.arrange(p1, p2, nrow=2)
```

![](Homework_05_Factor_and_figure_management_files/figure-markdown_github/unnamed-chunk-3-1.png)

Therefore, the `arrange()` funciton does not affect the data itself.

``` r
country_fct = fct_reorder(dat_arr_before$country, 
                          dat_arr_before$gdpPercap)

p3 = dat_arr_before %>% 
  ggplot(mapping = aes(x = gdpPercap, y = country_fct)) +
  geom_point(aes(size = gdpPercap))

grid.arrange(p1, p3, nrow=2)
```

![](Homework_05_Factor_and_figure_management_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
dat_fct_after = cbind(country = country_fct,dat_arr_before[,-1])
```

The `fct_reorder()` function will change the plot.

``` r
# reorder the levels of continent by standard deviation
# of the lifeExp in this continent
fct_reorder(dat$continent, dat$lifeExp, sd) %>% 
  levels()
```

    ## [1] "Europe"   "Africa"   "Americas" "Asia"

``` r
# reorder the level of countries by the difference in 
# lifeExp using customized function

difference = function(vec){
  min = vec[which.min(vec)]
  max = vec[which.max(vec)]
  return(max-min)
}

fct_reorder(dat$country, dat$lifeExp, difference) %>% 
  levels() %>% head()
```

    ## [1] "Norway"           "Liberia"          "Denmark"         
    ## [4] "Netherlands"      "Congo, Dem. Rep." "Sweden"

File I/O
========

.csv
----

``` r
write_csv(dat_arr_after, "arr_after.csv")
write_csv(dat_fct_after, "fct_after.csv")

d1 = read_csv("arr_after.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   country = col_character(),
    ##   continent = col_character(),
    ##   year = col_integer(),
    ##   lifeExp = col_double(),
    ##   pop = col_integer(),
    ##   gdpPercap = col_double()
    ## )

``` r
d2 = read_csv("fct_after.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   country = col_character(),
    ##   continent = col_character(),
    ##   year = col_integer(),
    ##   lifeExp = col_double(),
    ##   pop = col_integer(),
    ##   gdpPercap = col_double()
    ## )

``` r
str(d1$country)
```

    ##  chr [1:18] "South Africa" "Angola" "Swaziland" "Nigeria" ...

``` r
str(d2$country)
```

    ##  chr [1:18] "Angola" "Burundi" "Central African Republic" ...

.rds
----

``` r
saveRDS(dat_arr_after, "arr_after.RDS")
saveRDS(dat_fct_after, "fct_after.RDS")

d1 = readRDS("arr_after.RDS")
d2 = readRDS("fct_after.RDS")
levels(d1$country) %>% head()
```

    ## [1] "Angola"                   "Burundi"                 
    ## [3] "Central African Republic" "Congo, Dem. Rep."        
    ## [5] "Cote d'Ivoire"            "Guinea-Bissau"

``` r
levels(d2$country) %>% head()
```

    ## [1] "Congo, Dem. Rep."         "Liberia"                 
    ## [3] "Burundi"                  "Zimbabwe"                
    ## [5] "Guinea-Bissau"            "Central African Republic"

dput/dget
---------

``` r
dput(dat_arr_after, "arr_after.txt")
dput(dat_fct_after, "fct_after.txt")

d1 = dget("arr_after.txt")
d2 = dget("fct_after.txt")
levels(d1$country) %>% head()
```

    ## [1] "Angola"                   "Burundi"                 
    ## [3] "Central African Republic" "Congo, Dem. Rep."        
    ## [5] "Cote d'Ivoire"            "Guinea-Bissau"

``` r
levels(d2$country) %>% head()
```

    ## [1] "Congo, Dem. Rep."         "Liberia"                 
    ## [3] "Burundi"                  "Zimbabwe"                
    ## [5] "Guinea-Bissau"            "Central African Republic"

After reloading the data, `.csv` file will load factors as characters, while the other two methods will keep the origianl factors.

Visulization
============

``` r
# This is plotted above
p3 = dat_arr_before %>% 
  ggplot(mapping = aes(x = gdpPercap, y = country_fct)) +
  geom_point(aes(size = gdpPercap))

require(ggthemes)
```

    ## Loading required package: ggthemes

``` r
new_country_names = levels(country_fct)
new_country_colors =
  country_colors[
    which(new_country_names%in%names(country_colors))]

# country_colors theme
p_c = p3 + aes(color = new_country_colors);p_c
```

![](Homework_05_Factor_and_figure_management_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
# WSJ stype theme
p_w = p3 + theme_wsj();p_w
```

![](Homework_05_Factor_and_figure_management_files/figure-markdown_github/unnamed-chunk-9-2.png)

Writing fitures to file
=======================

``` r
setwd("/home/xyj/Documents/STAT 545/xie_yijun/homework_05")
# vector format
ggsave("p_w.pdf",p_w,scale = 2,dpi = 300)
```

    ## Saving 14 x 10 in image

``` r
# raster format
ggsave("p_c.png",p_c,scale = 1,dpi = 25)
```

    ## Saving 7 x 5 in image

link to the vector format with large scale and high resolution:

[p\_w.pdf](https://github.com/STAT545-UBC/xie_yijun/blob/master/homework_05/p_w.pdf)

link to the raster format with smaller scale and low resolution:

[p\_c.png](https://github.com/STAT545-UBC/xie_yijun/blob/master/homework_05/p_c.png)

Something more
==============

Mapping the country to the color of national flag

``` r
countries = c("Canada","Japan","China","United States","United Kingdom")

new_dat = gapminder %>% 
  filter(country %in% countries) %>% 
  droplevels()

new_dat$country %>% levels()
```

    ## [1] "Canada"         "China"          "Japan"          "United Kingdom"
    ## [5] "United States"

``` r
new_dat$country %>% fct_recode(
  "blue, red, white" = "United States",
  "blue, red, white" = "United Kingdom",
  "red, white" = "Japan",
  "red, white" = "Canada",
  "red, yellow" = "China") %>% 
  levels()
```

    ## [1] "red, white"       "red, yellow"      "blue, red, white"

Reflection: In this homework we practiced how to deal with factors and figures. Factors have been my nightmare for me for a long time, especially when you thought they are characters. Hopefully after this class I will no longer be afraid of factors.
