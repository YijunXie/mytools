explore gpaminder with the tidyverse
================
XieYijun
September 20, 2016

explore gapminder with the tidyverse
------------------------------------

``` r
library(tidyverse)
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
library(tibble)
library(gapminder)
```

display data
============

``` r
gapminder
```

    ## # A tibble: 1,704 × 6
    ##        country continent  year lifeExp      pop gdpPercap
    ##         <fctr>    <fctr> <int>   <dbl>    <int>     <dbl>
    ## 1  Afghanistan      Asia  1952  28.801  8425333  779.4453
    ## 2  Afghanistan      Asia  1957  30.332  9240934  820.8530
    ## 3  Afghanistan      Asia  1962  31.997 10267083  853.1007
    ## 4  Afghanistan      Asia  1967  34.020 11537966  836.1971
    ## 5  Afghanistan      Asia  1972  36.088 13079460  739.9811
    ## 6  Afghanistan      Asia  1977  38.438 14880372  786.1134
    ## 7  Afghanistan      Asia  1982  39.854 12881816  978.0114
    ## 8  Afghanistan      Asia  1987  40.822 13867957  852.3959
    ## 9  Afghanistan      Asia  1992  41.674 16317921  649.3414
    ## 10 Afghanistan      Asia  1997  41.763 22227415  635.3414
    ## # ... with 1,694 more rows

``` r
str(gapminder)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1704 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

``` r
glimpse(gapminder)
```

    ## Observations: 1,704
    ## Variables: 6
    ## $ country   <fctr> Afghanistan, Afghanistan, Afghanistan, Afghanistan,...
    ## $ continent <fctr> Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asi...
    ## $ year      <int> 1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992...
    ## $ lifeExp   <dbl> 28.801, 30.332, 31.997, 34.020, 36.088, 38.438, 39.8...
    ## $ pop       <int> 8425333, 9240934, 10267083, 11537966, 13079460, 1488...
    ## $ gdpPercap <dbl> 779.4453, 820.8530, 853.1007, 836.1971, 739.9811, 78...

``` r
head(gapminder)
```

    ## # A tibble: 6 × 6
    ##       country continent  year lifeExp      pop gdpPercap
    ##        <fctr>    <fctr> <int>   <dbl>    <int>     <dbl>
    ## 1 Afghanistan      Asia  1952  28.801  8425333  779.4453
    ## 2 Afghanistan      Asia  1957  30.332  9240934  820.8530
    ## 3 Afghanistan      Asia  1962  31.997 10267083  853.1007
    ## 4 Afghanistan      Asia  1967  34.020 11537966  836.1971
    ## 5 Afghanistan      Asia  1972  36.088 13079460  739.9811
    ## 6 Afghanistan      Asia  1977  38.438 14880372  786.1134

``` r
tail(gapminder)
```

    ## # A tibble: 6 × 6
    ##    country continent  year lifeExp      pop gdpPercap
    ##     <fctr>    <fctr> <int>   <dbl>    <int>     <dbl>
    ## 1 Zimbabwe    Africa  1982  60.363  7636524  788.8550
    ## 2 Zimbabwe    Africa  1987  62.351  9216418  706.1573
    ## 3 Zimbabwe    Africa  1992  60.377 10704340  693.4208
    ## 4 Zimbabwe    Africa  1997  46.809 11404948  792.4500
    ## 5 Zimbabwe    Africa  2002  39.989 11926563  672.0386
    ## 6 Zimbabwe    Africa  2007  43.487 12311143  469.7093

Let's filter to just certain rows. Let's select just certain columns.

``` r
(canada = filter(gapminder, country == 'Canada'))
```

    ## # A tibble: 12 × 6
    ##    country continent  year lifeExp      pop gdpPercap
    ##     <fctr>    <fctr> <int>   <dbl>    <int>     <dbl>
    ## 1   Canada  Americas  1952  68.750 14785584  11367.16
    ## 2   Canada  Americas  1957  69.960 17010154  12489.95
    ## 3   Canada  Americas  1962  71.300 18985849  13462.49
    ## 4   Canada  Americas  1967  72.130 20819767  16076.59
    ## 5   Canada  Americas  1972  72.880 22284500  18970.57
    ## 6   Canada  Americas  1977  74.210 23796400  22090.88
    ## 7   Canada  Americas  1982  75.760 25201900  22898.79
    ## 8   Canada  Americas  1987  76.860 26549700  26626.52
    ## 9   Canada  Americas  1992  77.950 28523502  26342.88
    ## 10  Canada  Americas  1997  78.610 30305843  28954.93
    ## 11  Canada  Americas  2002  79.770 31902268  33328.97
    ## 12  Canada  Americas  2007  80.653 33390141  36319.24

``` r
filter(gapminder, year > 2000)
```

    ## # A tibble: 284 × 6
    ##        country continent  year lifeExp      pop  gdpPercap
    ##         <fctr>    <fctr> <int>   <dbl>    <int>      <dbl>
    ## 1  Afghanistan      Asia  2002  42.129 25268405   726.7341
    ## 2  Afghanistan      Asia  2007  43.828 31889923   974.5803
    ## 3      Albania    Europe  2002  75.651  3508512  4604.2117
    ## 4      Albania    Europe  2007  76.423  3600523  5937.0295
    ## 5      Algeria    Africa  2002  70.994 31287142  5288.0404
    ## 6      Algeria    Africa  2007  72.301 33333216  6223.3675
    ## 7       Angola    Africa  2002  41.003 10866106  2773.2873
    ## 8       Angola    Africa  2007  42.731 12420476  4797.2313
    ## 9    Argentina  Americas  2002  74.340 38331121  8797.6407
    ## 10   Argentina  Americas  2007  75.320 40301927 12779.3796
    ## # ... with 274 more rows

``` r
filter(gapminder, continent == 'Europe', year == 2007)
```

    ## # A tibble: 30 × 6
    ##                   country continent  year lifeExp      pop gdpPercap
    ##                    <fctr>    <fctr> <int>   <dbl>    <int>     <dbl>
    ## 1                 Albania    Europe  2007  76.423  3600523  5937.030
    ## 2                 Austria    Europe  2007  79.829  8199783 36126.493
    ## 3                 Belgium    Europe  2007  79.441 10392226 33692.605
    ## 4  Bosnia and Herzegovina    Europe  2007  74.852  4552198  7446.299
    ## 5                Bulgaria    Europe  2007  73.005  7322858 10680.793
    ## 6                 Croatia    Europe  2007  75.748  4493312 14619.223
    ## 7          Czech Republic    Europe  2007  76.486 10228744 22833.309
    ## 8                 Denmark    Europe  2007  78.332  5468120 35278.419
    ## 9                 Finland    Europe  2007  79.313  5238460 33207.084
    ## 10                 France    Europe  2007  80.657 61083916 30470.017
    ## # ... with 20 more rows

``` r
filter(gapminder, country == 'Bulgaria' | country == 'Albania')
```

    ## # A tibble: 24 × 6
    ##    country continent  year lifeExp     pop gdpPercap
    ##     <fctr>    <fctr> <int>   <dbl>   <int>     <dbl>
    ## 1  Albania    Europe  1952  55.230 1282697  1601.056
    ## 2  Albania    Europe  1957  59.280 1476505  1942.284
    ## 3  Albania    Europe  1962  64.820 1728137  2312.889
    ## 4  Albania    Europe  1967  66.220 1984060  2760.197
    ## 5  Albania    Europe  1972  67.690 2263554  3313.422
    ## 6  Albania    Europe  1977  68.930 2509048  3533.004
    ## 7  Albania    Europe  1982  70.420 2780097  3630.881
    ## 8  Albania    Europe  1987  72.000 3075321  3738.933
    ## 9  Albania    Europe  1992  71.581 3326498  2497.438
    ## 10 Albania    Europe  1997  72.950 3428038  3193.055
    ## # ... with 14 more rows

``` r
filter(gapminder, country %in% c("Bulgaria","Albania"))
```

    ## # A tibble: 24 × 6
    ##    country continent  year lifeExp     pop gdpPercap
    ##     <fctr>    <fctr> <int>   <dbl>   <int>     <dbl>
    ## 1  Albania    Europe  1952  55.230 1282697  1601.056
    ## 2  Albania    Europe  1957  59.280 1476505  1942.284
    ## 3  Albania    Europe  1962  64.820 1728137  2312.889
    ## 4  Albania    Europe  1967  66.220 1984060  2760.197
    ## 5  Albania    Europe  1972  67.690 2263554  3313.422
    ## 6  Albania    Europe  1977  68.930 2509048  3533.004
    ## 7  Albania    Europe  1982  70.420 2780097  3630.881
    ## 8  Albania    Europe  1987  72.000 3075321  3738.933
    ## 9  Albania    Europe  1992  71.581 3326498  2497.438
    ## 10 Albania    Europe  1997  72.950 3428038  3193.055
    ## # ... with 14 more rows

``` r
select(gapminder, year, lifeExp)
```

    ## # A tibble: 1,704 × 2
    ##     year lifeExp
    ##    <int>   <dbl>
    ## 1   1952  28.801
    ## 2   1957  30.332
    ## 3   1962  31.997
    ## 4   1967  34.020
    ## 5   1972  36.088
    ## 6   1977  38.438
    ## 7   1982  39.854
    ## 8   1987  40.822
    ## 9   1992  41.674
    ## 10  1997  41.763
    ## # ... with 1,694 more rows

``` r
select(
  filter(gapminder, country == 'Canada'), 
  year, lifeExp)
```

    ## # A tibble: 12 × 2
    ##     year lifeExp
    ##    <int>   <dbl>
    ## 1   1952  68.750
    ## 2   1957  69.960
    ## 3   1962  71.300
    ## 4   1967  72.130
    ## 5   1972  72.880
    ## 6   1977  74.210
    ## 7   1982  75.760
    ## 8   1987  76.860
    ## 9   1992  77.950
    ## 10  1997  78.610
    ## 11  2002  79.770
    ## 12  2007  80.653

``` r
filter(select(gapminder,year,lifeExp,country),country == 'Canada')
```

    ## # A tibble: 12 × 3
    ##     year lifeExp country
    ##    <int>   <dbl>  <fctr>
    ## 1   1952  68.750  Canada
    ## 2   1957  69.960  Canada
    ## 3   1962  71.300  Canada
    ## 4   1967  72.130  Canada
    ## 5   1972  72.880  Canada
    ## 6   1977  74.210  Canada
    ## 7   1982  75.760  Canada
    ## 8   1987  76.860  Canada
    ## 9   1992  77.950  Canada
    ## 10  1997  78.610  Canada
    ## 11  2002  79.770  Canada
    ## 12  2007  80.653  Canada

``` r
gapminder[grep('Canada', gapminder$country),]
```

    ## # A tibble: 12 × 6
    ##    country continent  year lifeExp      pop gdpPercap
    ##     <fctr>    <fctr> <int>   <dbl>    <int>     <dbl>
    ## 1   Canada  Americas  1952  68.750 14785584  11367.16
    ## 2   Canada  Americas  1957  69.960 17010154  12489.95
    ## 3   Canada  Americas  1962  71.300 18985849  13462.49
    ## 4   Canada  Americas  1967  72.130 20819767  16076.59
    ## 5   Canada  Americas  1972  72.880 22284500  18970.57
    ## 6   Canada  Americas  1977  74.210 23796400  22090.88
    ## 7   Canada  Americas  1982  75.760 25201900  22898.79
    ## 8   Canada  Americas  1987  76.860 26549700  26626.52
    ## 9   Canada  Americas  1992  77.950 28523502  26342.88
    ## 10  Canada  Americas  1997  78.610 30305843  28954.93
    ## 11  Canada  Americas  2002  79.770 31902268  33328.97
    ## 12  Canada  Americas  2007  80.653 33390141  36319.24

Let's use pipeline

``` r
# function(arg1, arg2, arg3,....)

# same as arg1 %>% function(arg2, arg3, ....)

gapminder %>% 
  filter(country == 'Canada')
```

    ## # A tibble: 12 × 6
    ##    country continent  year lifeExp      pop gdpPercap
    ##     <fctr>    <fctr> <int>   <dbl>    <int>     <dbl>
    ## 1   Canada  Americas  1952  68.750 14785584  11367.16
    ## 2   Canada  Americas  1957  69.960 17010154  12489.95
    ## 3   Canada  Americas  1962  71.300 18985849  13462.49
    ## 4   Canada  Americas  1967  72.130 20819767  16076.59
    ## 5   Canada  Americas  1972  72.880 22284500  18970.57
    ## 6   Canada  Americas  1977  74.210 23796400  22090.88
    ## 7   Canada  Americas  1982  75.760 25201900  22898.79
    ## 8   Canada  Americas  1987  76.860 26549700  26626.52
    ## 9   Canada  Americas  1992  77.950 28523502  26342.88
    ## 10  Canada  Americas  1997  78.610 30305843  28954.93
    ## 11  Canada  Americas  2002  79.770 31902268  33328.97
    ## 12  Canada  Americas  2007  80.653 33390141  36319.24

``` r
gapminder %>% 
  filter(country == 'Canada') %>% 
  select(year, lifeExp)
```

    ## # A tibble: 12 × 2
    ##     year lifeExp
    ##    <int>   <dbl>
    ## 1   1952  68.750
    ## 2   1957  69.960
    ## 3   1962  71.300
    ## 4   1967  72.130
    ## 5   1972  72.880
    ## 6   1977  74.210
    ## 7   1982  75.760
    ## 8   1987  76.860
    ## 9   1992  77.950
    ## 10  1997  78.610
    ## 11  2002  79.770
    ## 12  2007  80.653

``` r
gapminder %>% 
  select(year, lifeExp, country) %>% 
  filter(country == "Canada") %>% 
  select(-country)
```

    ## # A tibble: 12 × 2
    ##     year lifeExp
    ##    <int>   <dbl>
    ## 1   1952  68.750
    ## 2   1957  69.960
    ## 3   1962  71.300
    ## 4   1967  72.130
    ## 5   1972  72.880
    ## 6   1977  74.210
    ## 7   1982  75.760
    ## 8   1987  76.860
    ## 9   1992  77.950
    ## 10  1997  78.610
    ## 11  2002  79.770
    ## 12  2007  80.653

``` r
gapminder %>% 
  select(starts_with('co'))
```

    ## # A tibble: 1,704 × 2
    ##        country continent
    ##         <fctr>    <fctr>
    ## 1  Afghanistan      Asia
    ## 2  Afghanistan      Asia
    ## 3  Afghanistan      Asia
    ## 4  Afghanistan      Asia
    ## 5  Afghanistan      Asia
    ## 6  Afghanistan      Asia
    ## 7  Afghanistan      Asia
    ## 8  Afghanistan      Asia
    ## 9  Afghanistan      Asia
    ## 10 Afghanistan      Asia
    ## # ... with 1,694 more rows
