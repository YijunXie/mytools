setwd("/home/xyj/Documents/STAT 545/xie_yijun/homework_07")
library(ggplot2)
library(tidyverse)
fav_and_re = read_csv("fav_and_re.csv")
leng = read_csv("tweet_length.csv")

p1 = ggplot(fav_and_re,aes(ct))+
	geom_point(aes(y = favoriteCount, colour = "favourites")) +
	geom_point(aes(y = retweetCount, colour = "retweets"))
ggsave(p1,file="fav_and_re.png")

p2 = qplot(lengths, data=leng, geom="histogram")
ggsave(p2,file="leng_count.png")
