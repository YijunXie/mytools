#'Piecewise linear scroing function
#' @import stats
#' @param x tested value
#' @param t true value
#' @param q quantile level
#'
#' @return numerical
#' @export
#' @examples
#'#' pwls(0.1,0.05,0.95)
#'#' pwls(0.2,0.2,0.9)

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
