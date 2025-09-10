fanchart_percentiles <- function(
	s,
	percentiles = c(0.1,0.25,0.5,0.75,0.9)
){

	# Error handler
	if(base::is.list(s) == FALSE) base::stop("argument \"s\" must be the list outputted by function sim().")

	simulations <- s$scenarios$stochastic_scenarios

	variable_names <- base::colnames(simulations[[1]])

	list_simulations <- base::list()
	for(i in variable_names){
		for(the_list in 1:base::length(simulations)){
			temp_dt <- simulations[[the_list]][,i]
			if(the_list == 1){
				list_simulations[[i]] <- temp_dt
			} else{
				list_simulations[[i]] <- xts::merge.xts(list_simulations[[i]],temp_dt,check.names=FALSE)
			}
		} 
		base::colnames(list_simulations[[i]]) <- base::seq_along(simulations)
	}

	# Sorts percentiles for correct display of results
	percentiles <- base::sort(percentiles)

	the_percentiles <- base::list()
	for(list_names in base::names(list_simulations)){
		simulated <- list_simulations[[list_names]]
		f <- function(x) stats::quantile(x = x,probs = percentiles,na.rm = TRUE)
		if (base::length(percentiles)==1){
			values <- base::apply(X=simulated,MARGIN=1,FUN=f)
		} else {
			values <- base::t(base::apply(X=simulated,MARGIN=1,FUN=f))
		}

		dt <- xts::xts(x = values, order.by = zoo::as.Date(base::rownames(values)))
		the_percentiles[[list_names]] <- dt
	}



	base::return(base::list(
		the_percentiles = the_percentiles
	))
}
