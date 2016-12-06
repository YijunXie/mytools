#' This function is used to conduct the likelihood ratio test.
#'
#' Given a vector of 0s and 1s indicate failure and success,
#' you can test whether the probability of success equals
#' the hypothesised one.
#' Test staitstic = 2[y*log(y/p) + (n-y)log((n-y)/(n - n*p))]
#' See http://www.biostat.umn.edu/~dipankar/bmtry711.11/lecture_02.pdf
#'
#' @import stats
#' @param dat list of values being tested
#' @param p hypothesis probability
#'
#' @return list
#' @export
#' @examples
#'#' lrtest(rnorm(100),0.01)

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
