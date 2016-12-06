.PHONY: all clean
.DELETE_ON_ERROR:
.SECONDARY:

all: pdf
	
clean:
	rm -f tweet.csv fav_and_re.csv tweet_length.csv 
	rm -f fav_and_re.png leng_count.png

# create the pdf file	
pdf: report
	pandoc -o homework_07.pdf homework_07.md

# generate .Rmd and .md file
report: homework_07.Rmd plot_tweet download_dat
	Rscript -e 'rmarkdown::render("$<")'

# create plots
plot_tweet: plot_tweet.R fav_and_re count_length
	Rscript $<

# count lengths of each tweet
count_length: count.R download_dat
	Rscript $<

# extract number of favourites and retweets
fav_and_re: fav_and_re.R download_dat
	Rscript $<

# download data
download_dat: download_data.R
	Rscript $<

