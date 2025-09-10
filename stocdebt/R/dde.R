dde <- function(
	realized_data,
	stock_flow_adjustments = NULL,
	d,
	i,
	ti,
	tic = NULL,
	r,
	r_tic = NULL,
	g,
	p
){

	# Error handlers
	# ______________________________________________________________________
	# ______________________________________________________________________

	if (!base::is.null(tic) & base::is.null(r_tic) | base::is.null(tic) & !base::is.null(r_tic)) base::stop("arguments \"tic\" and \"r_tic\" must both be informed or both be omitted, but one cannot be omitted while the other is omitted.")

	if (!base::is.null(tic) & !base::is.null(r_tic)){
		if (!base::all(base::sapply(X=base::c(d,i,ti,tic,r,r_tic,g,p),FUN=base::is.character))) base::stop("all variable names (d, i, ti, tic, r, r_tic, g, p) must be of class \"character\".")
	} else {
		if (!base::all(base::sapply(X=base::c(d,i,ti,r,g,p),FUN=base::is.character))) base::stop("all variable names (d, i, ti, r, g, p) must be of class \"character\".")
	}

	# Selects only variables of interest from the realized data
	# ______________________________________________________________________
	# ______________________________________________________________________

	if (!base::is.null(tic)){
		realized_data <- realized_data[,base::c(d,i,ti,tic,r,r_tic,g,p)]
	} else {
		realized_data <- realized_data[,base::c(d,i,ti,r,g,p)]
	}



	# Computes the new paths
	# ______________________________________________________________________
	# ______________________________________________________________________

	# Creates names for the variables to be simulated
	new_d <- base::paste(d,"(simulated, fixed init. cond.)")
	new_r <- base::paste(r,"(simulated, fixed init. cond.)")
	new_r_tic <- base::paste(r_tic,"(simulated, fixed init. cond.)")

	# Creates columns for the SIMULATED debt and the SIMULATED real interest rate, which will be filled later
	n <- base::colnames(realized_data)
	if (!base::is.null(tic)){
		combined_paths <- xts::merge.xts(NA,NA,NA,realized_data,check.names=FALSE)
		base::colnames(combined_paths) <- base::c(new_d,new_r,new_r_tic,n)
	} else {
		combined_paths <- xts::merge.xts(NA,NA,realized_data,check.names=FALSE)
		base::colnames(combined_paths) <- base::c(new_d,new_r,n)
	}

	# Converts the xts object "combined_paths" to a data.frame, which is better suited for the following computations
	y <- base::as.data.frame(combined_paths)

	# Simulates the scenario
	y[1,new_d] <- y[1,d] # Sets initial condition for debt
	if (!base::is.null(tic)){
		for (period in 1:nrow(y)){

			# Dynamics of the real interest rate w.r.t. GDP DEFLATOR
			y[period,new_r] <- 100 * {(1+y[period,i]/100) / (1+y[period,ti]/100) - 1}

			# Dynamics of the real interest rate w.r.t. CONSUMER INFLATION
			y[period,new_r_tic] <- 100 * {(1+y[period,i]/100) / (1+y[period,tic]/100) - 1}

			# Skips first iteration before computing debt, because debt requires lagged debt (which is not yet defined in the first iteration)
			if (period == 1) next

			# Debt dynamics using real interest rate w.r.t. CONSUMER INFLATION
			y[period,new_d] <- 100 * ( (1 + y[period,tic]/100) / (1 + y[period,ti]/100) * (1 + y[period,new_r_tic]/100) / (1 + y[period,g]/100) * (y[period-1,new_d]/100) - (y[period,p]/100) )

		}
	} else {
		for (period in 1:nrow(y)){

			# Dynamics of the real interest rate w.r.t. GDP DEFLATOR
			y[period,new_r] <- 100 * {(1+y[period,i]/100) / (1+y[period,ti]/100) - 1}

			# Skips first iteration before computing debt, because debt requires lagged debt (which is not yet defined in the first iteration)
			if (period == 1) next

			# Debt dynamics using real interest rate w.r.t. GDP DEFLATOR
			y[period,new_d] <- 100 * {(1 + y[period,new_r]/100)/(1 + y[period,g]/100) * y[period-1,new_d]/100 - y[period,p]/100}

		}
	}

	y <- xts::as.xts(y)
	zoo::index(y) <- zoo::index(combined_paths)
	y -> paths



	# Computes an additional, artificial path for debt, for the purpose of approximating stock-flow adjustments
	# The difference with respect to the computation above is that, every year, debt will be calculated using the REALIZED debt one period before, instead of the SIMULATED debt
	# That is, we have here a MOVING initial conditional for the difference equation, instead of a FIXED initial condition
	# ______________________________________________________________________
	# ______________________________________________________________________

	# Creates a name for the variable to be simulated
	new_d2 <- base::paste(d,"(simulated, moving init. cond.)")

	# Creates columns for the SIMULATED debt, which will be filled later
	n <- base::colnames(paths)
	combined_paths <- xts::merge.xts(NA,paths,check.names=FALSE)
	base::colnames(combined_paths) <- base::c(new_d2,n)

	# Converts the xts object "combined_paths" to a data.frame, which is better suited for the following computations
	y <- base::as.data.frame(combined_paths)

	# Simulates the scenario
	y[1,new_d2] <- y[1,d] # Sets initial condition for debt

	if (!base::is.null(tic)){
		for (period in 2:nrow(y)){
			# Debt dynamics using real interest rate w.r.t. CONSUMER INFLATION
			y[period,new_d2] <- 100 * ( (1 + y[period,tic]/100) / (1 + y[period,ti]/100) * (1 + y[period,new_r_tic]/100) / (1 + y[period,g]/100) * (y[period-1,d]/100) - (y[period,p]/100) )
		}
	} else {
		for (period in 2:nrow(y)){
			# Debt dynamics using real interest rate w.r.t. GDP DEFLATOR
			y[period,new_d2] <- 100 * {(1 + y[period,new_r]/100)/(1 + y[period,g]/100) * y[period-1,d]/100 - y[period,p]/100}
		}
	}
	y <- xts::as.xts(y)
	zoo::index(y) <- zoo::index(combined_paths)
	y -> paths



	# Computes the difference between realized debt and simulated debt (FIXED initial condition)
	# ______________________________________________________________________
	# ______________________________________________________________________

	temp <- paths[,d] - paths[,new_d]
	base::colnames(temp) <- base::paste(d,"(real - simulated, fixed init. cond.)") 
	paths <- xts::merge.xts(paths,temp,check.names=FALSE)

	# Computes the difference between realized debt and simulated debt (MOVING initial condition)
	# ______________________________________________________________________
	# ______________________________________________________________________

	temp <- paths[,d] - paths[,new_d2]
	base::colnames(temp) <- base::paste(d,"(real - simulated, moving init. cond.)") 
	paths <- xts::merge.xts(paths,temp,check.names=FALSE)



	# Adds stock-flow adjustment series for comparisons
	if (!base::is.null(stock_flow_adjustments)){
		paths <- xts::merge.xts(paths,stock_flow_adjustments,check.names=FALSE)
	}

	base::return(base::list(
		paths = paths
	))
}
