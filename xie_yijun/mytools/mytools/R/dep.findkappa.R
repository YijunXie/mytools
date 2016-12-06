#' Dependent file for findkappa
#'
fn = function(l1,l2,kappa,inov = "norm",df=1){
	# normal distribution
	if(inov == "norm"){
		intf = function(x){
			2*((l1*x^2+l2)^kappa)*dnorm(x)
		}
	}

	# t distribution
	if(inov == "std"){
		intf = function(x){
			2*((l1*x^2+l2)^kappa)*dt(x,df = df)
		}
	}
	return(intf)
}

# expectation of fn -1
gn2 = function(l1,l2,kappa,inov = "norm",df=1){
	integrate(fn(l1,l2,kappa,inov,df),0,Inf)$value - 1
}

