#' function for calculating chibar
#'
#' Find the tail index chibar
#'
#' @import matrixStats
#' @param dat numerical matrix
#' @param q quantile level
#' @param n discarded number
#' @param conf confidence level
#'
#' @return vector
#' @export
#' @examples
#'#' chi.bar(rnorm(1000),q = 0.9,n = 1)

chi.bar = function(dat, q = 0.95, n = 1, conf = 0.95){
	# remove the smallest and largest value
	c1 = dat[-c((length(dat)-n+1):length(dat))]
	c2 = dat[-c(1:n)]
	# pairwise data
	temp_dat = cbind(c1,c2)
	# lower value of each pair
	z = rowMins(temp_dat)
	# find the thred
	u = quantile(dat,q)
	# proportion of pairs larger than the thred
	nu = length(which(z > u))/length(dat)
	# find chibar
	chibar = 2*log(1-q) / log(nu) -1
	# find ci of chibar
	chibarvar = (((4 * log(1 - q)^2)/(log(nu)^4 * nu^2)) *
							 	nu * (1 - nu))/length(dat)
	chibarlb = chibar - qnorm((1+ conf)/2)*sqrt(chibarvar)
	chibarub = chibar + qnorm((1+ conf)/2)*sqrt(chibarvar)
	return(c(chibarlb, chibar, chibarub))
}
