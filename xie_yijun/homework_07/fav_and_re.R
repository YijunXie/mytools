setwd("/home/xyj/Documents/STAT 545/xie_yijun/homework_07")
library(tidyverse)
dat = read.csv("tweet.csv")
dat = dat %>% 
	select(favoriteCount,retweetCount)
dat = cbind(ct = c(1:nrow(dat)),dat)
write_csv(dat,"fav_and_re.csv")
