HW6\_Data\_wrangling\_conclusion
================
Yijun Xie
November 14, 2016

``` r
# load files and packages
library(gapminder)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(forcats)
library(gridExtra)
```

Writing Functions
=================

I checked several projects I am working on right now, and found some duplicated chunks of codes that I could write functions to make the script more tidy. So I decided to use this homework as a chance to write these codes as functions.

### 1. Likelihood ratio test for binomial distribution

``` r
# This function is used to conduct the likelihood ratio test.
# Given a vector of 0s and 1s indicate failure and success, 
# you can test whether the probability of success equals 
# the hypothesised one.
# Test staitstic = 2[y*log(y/p) + (n-y)log((n-y)/(n - n*p))]
# See http://www.biostat.umn.edu/~dipankar/bmtry711.11/lecture_02.pdf
# Input: A vector = dat; hypothesised probability = p.
# Output: test statistic = z; p-value
lrtest = function(dat,p){
  # number of success
  y = sum(dat)
  # number of trials
  n = length(dat)
  # sample proportion
  phat = y/n
  # calculate test statistics
  z = 2*(y*log(phat/p) + (n-y)*log((1-phat)/(1-p)))
  # calculate p-value
  p_value = 1 - pchisq(z,df = 1)
  return(list(z,p_value))
}
```

### 2. Unit Frechet Distribution Transformation

Sometimes I need to transform the marginal distribution to a unit Frechet distribution to analyze the tail behavior, and later transform it back to the orginal distribution. These two functions can help to do transform the data.

``` r
# Require package "Hsmic".
# This function transform the data to a unit Frechet distribution.
# Input: vector of data = dat
# Output: transformed data = res
to.unitfrechet = function(dat){
  # create an empty vector to store transferred data
  ind = rep(0,length(dat))
  # find empirical cdf of the original data
  cdfs = Ecdf(dat,pl = F)
  # remove the first one (F(`) = 0)
  tempx = cdfs$x[-1]
  # match the quantile of old data to the quantile of unit Frechet
  for(i in 1:length(dat)){
    ind[i] = which(dat == tempx[i])
  }
  fx = cdfs$y[ind]
  # find the corresponding value in unit Frechet
  res = -1 / log(fx)
  return(res)
}

# transfer the processed unit Frechet data back to the original data
# Input: vector of unit Frechet data = ufdat; old data = odat
# Output: new data = x
from.unitfrechet = function(ufdat,odat){
  # find the quantile of each unit Frechet data
  cd = exp(-1 / ufdat)
  # match quantile
  x = quantile(odat,cd)
  return(x)
}
```

### 3. Piece-wise Linear Scoring function

This is a sepcial penality function, similar to the mean score error or mean absolute error, and is used to regularize the quantile. It takes the form *f*(*x*)=*I*(*x* &lt; *t*)(*α*|*x* − *t*|) + *I*(*x* &gt; *t*)((1 − *α*)|*x* − *t*|), where *α* is the quantile level, *x* is predicted value, and *t* the true value.

``` r
# Piecewise linear scroing function
# Input: predicted value = x; true value = t; quantile = q;
pwls = function(x,t,q){
  if(x <= t){
    # I(x<t)(\alpha|x-t|)
    s = q*abs(x-t)
  } else {
    # I(x>t)((1-\alpha)|x-t|)
    s = (1-q)*abs(x-t)
  }
  return(s)
}
```

Work With A List
================

Trump was elected last week, and basicly he is the center of most of the conversations between my friends and me.

``` r
# load packages
library(purrr)
suppressMessages(library(dplyr))
library(tibble)
```

First, I load the data

``` r
load(url("http://varianceexplained.org/files/trump_tweets_df.rda"))
glimpse(trump_tweets_df)
```

    ## Observations: 1,512
    ## Variables: 16
    ## $ text          <chr> "My economic policy speech will be carried live ...
    ## $ favorited     <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,...
    ## $ favoriteCount <dbl> 9214, 6981, 15724, 19837, 34051, 29831, 19223, 1...
    ## $ replyToSN     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ created       <dttm> 2016-08-08 15:20:44, 2016-08-08 13:28:20, 2016-...
    ## $ truncated     <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,...
    ## $ replyToSID    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ id            <chr> "762669882571980801", "762641595439190016", "762...
    ## $ replyToUID    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ statusSource  <chr> "<a href=\"http://twitter.com/download/android\"...
    ## $ screenName    <chr> "realDonaldTrump", "realDonaldTrump", "realDonal...
    ## $ retweetCount  <dbl> 3107, 2390, 6691, 6402, 11717, 9892, 5784, 7930,...
    ## $ isRetweet     <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,...
    ## $ retweeted     <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,...
    ## $ longitude     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ latitude      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...

The content of his tweets is stored in the `text`. So I will extract this column.

``` r
tweets <- trump_tweets_df$text
tweets %>% head() %>% strtrim(140)
```

    ## [1] "My economic policy speech will be carried live at 12:15 P.M. Enjoy!"                                                                       
    ## [2] "Join me in Fayetteville, North Carolina tomorrow evening at 6pm. Tickets now available at: https://t.co/Z80d4MYIg8"                        
    ## [3] "#ICYMI: \"Will Media Apologize to Trump?\" https://t.co/ia7rKBmioA"                                                                        
    ## [4] "Michael Morell, the lightweight former Acting Director of C.I.A., and a man who has made serious bad calls, is a total Clinton flunky!"    
    ## [5] "The media is going crazy. They totally distort so many things on purpose. Crimea, nuclear, \"the baby\" and so much more. Very dishonest!" 
    ## [6] "I see where Mayor Stephanie Rawlings-Blake of Baltimore is pushing Crooked hard. Look at the job she has done in Baltimore. She is a joke!"

As a Chinese, I am interested in his opinion about China/Asia.

``` r
regex <- "China|Chinese|Asia|Asian|Pacific"
matches <- gregexpr(regex, tweets)
str(matches[1])
```

    ## List of 1
    ##  $ : atomic [1:1] -1
    ##   ..- attr(*, "match.length")= int -1

Next, I would like to extract the `match.length` of each tweet to see how many tweets is related to China/Asia.

``` r
ml = match_length <- map(matches, attr, which = "match.length")
cn = c()
for(i in 1:length(ml)){
  if(ml[[i]] > 1){
    cn = c(cn,i)
  }
}
length(cn)
```

    ## [1] 5

``` r
cn_tweet = tweets[cn]
cn_tweet
```

    ## [1] "Tim Kaine has been praising the Trans Pacific Partnership and has been pushing hard to get it approved. Job killer!"                         
    ## [2] "The @USCHAMBER must fight harder for the American worker. China, and many others, are taking advantage of U.S. with our terrible trade pacts"
    ## [3] "Hillary Clinton surged the trade deficit with China 40% as\nSecretary of State, costing Americans millions of jobs."                         
    ## [4] "Crooked Hillary has zero imagination and even less stamina. ISIS, China, Russia and all would love for her to be president. 4 more years!"   
    ## [5] "If Crooked Hillary Clinton can't close the deal on Crazy Bernie, how is she going to take on China, Russia, ISIS and all of the others?"

Clearly there are five tweets related to China/Asia. Totally enjoyable to read these LOL.

For the purpose of finishing this homework, I will add some unrelated tweets into this list.

``` r
cn_tweet = tweets[c(1,2,3,cn)]
matches <- gregexpr(regex,cn_tweet)
match_length <- map(matches, attr, which = "match.length")
str(matches)
```

    ## List of 8
    ##  $ : atomic [1:1] -1
    ##   ..- attr(*, "match.length")= int -1
    ##   ..- attr(*, "useBytes")= logi TRUE
    ##  $ : atomic [1:1] -1
    ##   ..- attr(*, "match.length")= int -1
    ##   ..- attr(*, "useBytes")= logi TRUE
    ##  $ : atomic [1:1] -1
    ##   ..- attr(*, "match.length")= int -1
    ##   ..- attr(*, "useBytes")= logi TRUE
    ##  $ : atomic [1:1] 39
    ##   ..- attr(*, "match.length")= int 7
    ##   ..- attr(*, "useBytes")= logi TRUE
    ##  $ : atomic [1:1] 59
    ##   ..- attr(*, "match.length")= int 5
    ##   ..- attr(*, "useBytes")= logi TRUE
    ##  $ : atomic [1:1] 47
    ##   ..- attr(*, "match.length")= int 5
    ##   ..- attr(*, "useBytes")= logi TRUE
    ##  $ : atomic [1:1] 67
    ##   ..- attr(*, "match.length")= int 5
    ##   ..- attr(*, "useBytes")= logi TRUE
    ##  $ : atomic [1:1] 94
    ##   ..- attr(*, "match.length")= int 5
    ##   ..- attr(*, "useBytes")= logi TRUE

Next, let's see how often did Trump talk about China/Asia in these tweets.

``` r
f <- function(x) sum(x > 0)
map(matches, f)
```

    ## [[1]]
    ## [1] 0
    ## 
    ## [[2]]
    ## [1] 0
    ## 
    ## [[3]]
    ## [1] 0
    ## 
    ## [[4]]
    ## [1] 1
    ## 
    ## [[5]]
    ## [1] 1
    ## 
    ## [[6]]
    ## [1] 1
    ## 
    ## [[7]]
    ## [1] 1
    ## 
    ## [[8]]
    ## [1] 1

Alternatively, I can also use `map_int`

``` r
map_int(matches, ~ sum(.x > 0))
```

    ## [1] 0 0 0 1 1 1 1 1

The following table shows the difference between naive length and the length of tweets involving China/Asia.

``` r
tibble(
  naive_length = lengths(matches),
  n_words = map_int(matches, ~ sum(.x > 0))
)
```

    ## # A tibble: 8 × 2
    ##   naive_length n_words
    ##          <int>   <int>
    ## 1            1       0
    ## 2            1       0
    ## 3            1       0
    ## 4            1       1
    ## 5            1       1
    ## 6            1       1
    ## 7            1       1
    ## 8            1       1

Next, let's see where dose the words related to China/Asia occur in his tweets.

``` r
(match_first <- map(matches, as.vector))
```

    ## [[1]]
    ## [1] -1
    ## 
    ## [[2]]
    ## [1] -1
    ## 
    ## [[3]]
    ## [1] -1
    ## 
    ## [[4]]
    ## [1] 39
    ## 
    ## [[5]]
    ## [1] 59
    ## 
    ## [[6]]
    ## [1] 47
    ## 
    ## [[7]]
    ## [1] 67
    ## 
    ## [[8]]
    ## [1] 94

Now let's focus on tweet \#1 and \#5, which contain 0, 1 and 1 word respectively. For tweet \#5:

``` r
(tweet <- cn_tweet[5])
```

    ## [1] "The @USCHAMBER must fight harder for the American worker. China, and many others, are taking advantage of U.S. with our terrible trade pacts"

``` r
(t_first <- match_first[[5]])
```

    ## [1] 59

``` r
(t_length <- match_length[[5]])
```

    ## [1] 5

``` r
(t_last <- t_first + t_length - 1)
```

    ## [1] 63

``` r
substring(tweet, t_first, t_last)
```

    ## [1] "China"

For tweet \#4:

``` r
(tweet <- cn_tweet[4])
```

    ## [1] "Tim Kaine has been praising the Trans Pacific Partnership and has been pushing hard to get it approved. Job killer!"

``` r
(t_first <- match_first[[4]])
```

    ## [1] 39

``` r
(t_length <- match_length[[4]])
```

    ## [1] 7

``` r
(t_last <- t_first + t_length - 1)
```

    ## [1] 45

``` r
substring(tweet, t_first, t_last)
```

    ## [1] "Pacific"

For tweet \#1:

``` r
(tweet <- cn_tweet[1])
```

    ## [1] "My economic policy speech will be carried live at 12:15 P.M. Enjoy!"

``` r
(t_first <- match_first[[1]])
```

    ## [1] -1

``` r
(t_length <- match_length[[1]])
```

    ## [1] -1

``` r
(t_last <- t_first + t_length - 1)
```

    ## [1] -3

``` r
substring(tweet, t_first, t_last)
```

    ## [1] ""

It looks great. Next, let's store where Trump's words by using `purrr::map2()`.

``` r
(match_last <- map2(match_first, match_length, ~ .x + .y - 1))
```

    ## [[1]]
    ## [1] -3
    ## 
    ## [[2]]
    ## [1] -3
    ## 
    ## [[3]]
    ## [1] -3
    ## 
    ## [[4]]
    ## [1] 45
    ## 
    ## [[5]]
    ## [1] 63
    ## 
    ## [[6]]
    ## [1] 51
    ## 
    ## [[7]]
    ## [1] 71
    ## 
    ## [[8]]
    ## [1] 98

Knowing where these words end, we can extract the exact word he said using `pmap()`

``` r
pmap(list(text = cn_tweet, first = match_first, last = match_last), substring)
```

    ## [[1]]
    ## [1] ""
    ## 
    ## [[2]]
    ## [1] ""
    ## 
    ## [[3]]
    ## [1] ""
    ## 
    ## [[4]]
    ## [1] "Pacific"
    ## 
    ## [[5]]
    ## [1] "China"
    ## 
    ## [[6]]
    ## [1] "China"
    ## 
    ## [[7]]
    ## [1] "China"
    ## 
    ## [[8]]
    ## [1] "China"

Thus, in the five tweets he mentioned China/Asia, he used "China" four times, and "Pacific" once. To make the code more neat and safer, we can creat a data frame to store the text as well as the starting and endind point.

``` r
mdf <- tibble(
  text = cn_tweet,
  first = match_first,
  last = match_last
)
pmap(mdf, substring)
```

    ## [[1]]
    ## [1] ""
    ## 
    ## [[2]]
    ## [1] ""
    ## 
    ## [[3]]
    ## [1] ""
    ## 
    ## [[4]]
    ## [1] "Pacific"
    ## 
    ## [[5]]
    ## [1] "China"
    ## 
    ## [[6]]
    ## [1] "China"
    ## 
    ## [[7]]
    ## [1] "China"
    ## 
    ## [[8]]
    ## [1] "China"

And do everything at once:

``` r
tibble(text = cn_tweet,
       first = gregexpr(regex, cn_tweet)) %>% 
  mutate(match_length = map(first, ~ attr(.x, which = "match.length")),
         last = map2(first, match_length, ~ .x + .y - 1)) %>%
  select(-match_length) %>% 
  pmap(substring)
```

    ## [[1]]
    ## [1] ""
    ## 
    ## [[2]]
    ## [1] ""
    ## 
    ## [[3]]
    ## [1] ""
    ## 
    ## [[4]]
    ## [1] "Pacific"
    ## 
    ## [[5]]
    ## [1] "China"
    ## 
    ## [[6]]
    ## [1] "China"
    ## 
    ## [[7]]
    ## [1] "China"
    ## 
    ## [[8]]
    ## [1] "China"

Reflection
----------

For the first part of this homework, I turned some of my previous codes into functions. This is something I plan to do for a while. I think this course is a good opportunity for me to start working on my personal package with the collection of useful function.

For the second part, it is kind of fun to work on Trump's tweets. I would definitely not have the time to read all his tweets, but with the help of `purrr` package I can easily find what I need.
