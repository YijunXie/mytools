setwd("/home/xyj/Documents/STAT 545/xie_yijun/homework_07")
library(tidyverse)
load(url("http://varianceexplained.org/files/trump_tweets_df.rda"))
write_csv(trump_tweets_df,"tweet.csv")