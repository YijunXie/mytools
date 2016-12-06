library(tidyverse)
dat = read_csv("tweet.csv")
tweet_content = dat[1]
lengths = nchar(tweet_content[[1]])
lengths = cbind(ct = c(1:length(lengths)),lengths)
write_csv(as.data.frame(lengths,ncol = 1),"tweet_length.csv")
