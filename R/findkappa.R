#' function for find coefficient of tail of GARCH process
#' @import stats
#' @import graphics
#' @param l1 lambda_1, coefficient of ARCH term
#' @param l2 lambda_2, coefficient of GARCH term
#' @param precision precision level of result
#' @param inov innovation distribution of GARCH process
#' @param df degree of freedom if the innovation is Student's t
#'
#' @return numerical value of kappa
#' @export
#' @examples
#'#' find.kappa(0.1,0.85,precision = 0.001,inov = "norm"),
#'#' find.kappa(0.1,0.85,precision = 0.001,inov = "std",df = 5)

find.kappa = function(l1,l2,precision = 0.01,inov = "norm",df=1){
	g = seq(0,10,precision) # searching grid
	result = rep(0,length(g))
	flag = length(g)
	for(i in 1:length(g)){
		kappa = g[i]
		# expectation for each possible kappa
		result[i] = gn2(l1,l2,kappa,inov,df)
		if(result[i] > 0.5) {
			flag = i
			break
		}
	}
	#return(result)
	title = paste('lambda1=',round(l1,3),'lambda2=',round(l2,3))
	#   if(i == length(g)){
	#     plot(result[1:(5/precision)], main = c(title))
	#   } else{
	plot(result[1:flag], main = c(title),type = 'l')
	#}
	abline(h = 0)
	# find the kappa with smallest absolute value
	ind = which.min(abs(result[2:flag]))+1
	return(g[ind])
}
