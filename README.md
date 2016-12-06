
<!-- README.md is generated from README.Rmd. Please edit that file -->
-   Note: This package is still under construction. More functions will be added later.\*

`mytools` is a collection of my own functions.
----------------------------------------------

### Installation

``` r
devtools::install_github("YijunXie/mytools")
library(mytools)
```

### Quick demo

-   Calculate coefficient of tail independence

``` r
chi.bar(rnorm(1000),q = 0.95, n = 1, conf = 0.95)
#> [1] -0.3786211 -0.1326467  0.1133277
```

-   Find tail index

``` r
lrtest(c(rep(0,93),rep(1,7)),0.05)  
#> [[1]]
#> [1] 0.7530152
#> 
#> [[2]]
#> [1] 0.3855233
```

-   Result of piecewise linear scoring function

``` r
pwls(0.09,0.05,0.95)
#> [1] 0.002
```
