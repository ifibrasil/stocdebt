shocks_generator <- function(

	case = "normal",
	parameters = NULL,
	n_stochastic_scenarios,

	realized_data = NULL,
	subintervals = NULL,

	shocks_first_date,
	shocks_last_date,
	shocks_frequency,
	correlated_shocks = TRUE,

	i,
	ti,
	tic = NULL,
	g,
	p,

	shocks_seed = 1

){




	# PART 1/2: preparation
	# ______________________________________________________________________________________________
	# ______________________________________________________________________________________________

	# Error handlers
	if (!shocks_frequency %in% base::c("year","quarter","month")) base::stop("argument \"shocks_frequency\" must be one of \"year\", \"quarter\" or \"month\".")
	if (!base::class(shocks_first_date) %in% base::c("character","Date")) base::stop("argument \"shocks_first_date\" must be of class \"character\" or \"Date\".")
	if (!base::class(shocks_last_date) %in% base::c("character","Date")) base::stop("argument \"shocks_last_date\" must be of class \"character\" or \"Date\".")

	if (case == "normal"){
		if (base::is.null(parameters)) base::stop("argument \"parameters\" must be informed when case = \"normal\".")
	}

	if (case == "ec"){
		if (base::is.null(realized_data)) base::stop("argument \"realized_data\" must be informed when case = \"ec\".")

		if (!base::is.null(subintervals) & !base::class(subintervals) %in% base::c("character","Date")) base::stop("argument \"subintervals\" must be of class \"character\" or \"Date\".")

		if (shocks_frequency == "quarter"){
			the_months_1 <- base::unique(base::format(zoo::index(realized_data),"%m"))
			the_months_2 <- base::unique(base::format(zoo::as.Date(c(shocks_first_date,shocks_last_date)),"%m"))

			all_dates <- base::unlist(base::strsplit(x=subintervals,split="/"))
			the_months_3 <- base::unique(base::format(zoo::as.Date(all_dates),"%m"))

			if (!all(the_months_2 %in% the_months_1)) base::stop("quarters cannot be indicated by different months in arguments \"realized_data\", \"shocks_first_date\" and \"shocks_last_date\".\n  For example, if the second quarter is represented by June in \"realized_data\", it cannot be represented by April neither May in \"shocks_first_date\".")

			if (!all(the_months_3 %in% the_months_1)) base::stop("quarters cannot be indicated by different months in arguments \"realized_data\" and \"subintervals\".\n  For example, if the second quarter is represented by June in \"realized_data\", it cannot be represented by April neither May in \"subintervals\".")
		}
	}

	# Ensures that dates informed as arguments are of class 'Date'
	shocks_first_date <- zoo::as.Date(shocks_first_date)
	shocks_last_date <- zoo::as.Date(shocks_last_date)

	# Initializes list which will store all shocks and will be returned by the function
	shocks <- base::list()

	# Calculates the size of shock vectors
	size <- base::length(base::seq.Date(
		from = shocks_first_date,
		to = shocks_last_date,
		by = shocks_frequency
	))








	# PART 2/2: shock drawing
	# ______________________________________________________________________________________________
	# ______________________________________________________________________________________________

	# Case "normal" (draws from normal distributions)
	# _______________________________________________

	if (case == "normal"){

	  	if (shocks_frequency == "year") roll_window <- 1
	  	if (shocks_frequency == "quarter") roll_window <- 4
	  	if (shocks_frequency == "month") roll_window <- 12

		withr::with_seed(
			seed = shocks_seed,
			code = {

				for (k in base::seq_len(n_stochastic_scenarios)){

					# Randomization (creation of the shocks per se). Function zoo::rollsum() is used with the sole purpose of smoothing the time series generated.
					temp <- base::list()

					temp[[i]] <-
						zoo::rollsum(x=stats::rnorm(n = -1 + size + roll_window, mean = 0, sd = parameters$sd[[i]]), k = roll_window)

					temp[[ti]] <-
						zoo::rollsum(x=stats::rnorm(n = -1 + size + roll_window, mean = 0, sd = parameters$sd[[ti]]), k = roll_window)
					if (!base::is.null(tic)){
						temp[[tic]] <-
							zoo::rollsum(x=stats::rnorm(n = -1 + size + roll_window, mean = 0, sd = parameters$sd[[tic]]), k = roll_window)
					}
					temp[[g]] <-
						zoo::rollsum(x=stats::rnorm(n = -1 + size + roll_window, mean = 0, sd = parameters$sd[[g]]), k = roll_window)

					temp[[p]] <-
						zoo::rollsum(x=stats::rnorm(n = -1 + size + roll_window, mean = 0, sd = parameters$sd[[p]]), k = roll_window)

					# Gathers shocks into a single object, of class "xts"
					temp <- xts::xts(
						x = base::data.frame(temp),
						order.by = base::seq.Date(
							from = shocks_first_date,
							to = shocks_last_date,
							by = shocks_frequency
						)
					)
					if (!base::is.null(tic)){
						base::colnames(temp) = base::c(i,ti,tic,g,p) # Applies standard names
					} else {
						base::colnames(temp) = base::c(i,ti,g,p) # Applies standard names
					}

					# Stores the shocks generated in this loop iteration into the final list
					shocks[[k]] <- temp

				}
			}
		)

		historical_shocks <- NULL
		var_cov_historical_shocks <- NULL
		correlation_tests <- NULL
	}

	# Case "ec" (European Commission)
	# _______________________________________________

	if (case == "ec"){

		# Selects only variables of interest from realized data
		if (!base::is.null(tic)){
			realized_data <- realized_data[,base::c(i,ti,tic,g,p)]
		} else {
			realized_data <- realized_data[,base::c(i,ti,g,p)]
		}

		# Generates first differences of variables ("historical shocks")
		historical_shocks <- base::diff(x=realized_data, lag=1, differences=1)
		historical_shocks <- stats::na.omit(historical_shocks)

		if (!base::is.null(subintervals)){
			# Restricts historical shocks to subintervals
			list_of_subintervals <- base::list()
			for (k in base::seq_along(subintervals)){
				list_of_subintervals[[k]] <- historical_shocks[subintervals[[k]]]
			}
			historical_shocks <- base::Reduce(f=xts::rbind.xts,list_of_subintervals)
		}

		# Calculates the variance-covariance matrix of the historical shocks
		var_cov_historical_shocks <- stats::var(historical_shocks,na.rm=TRUE)

		# Creates matrices to store p-values of many significance tests of the covariances (correlations)
		pearson <- base::matrix(
			NA,
			ncol = base::ncol(historical_shocks),
			nrow = base::ncol(historical_shocks),
			dimnames = base::list(
				base::colnames(historical_shocks),
				base::colnames(historical_shocks)
			)
		)
		spearman <- pearson
		kendall <- pearson

		# Computes the p-values
		for (v in base::colnames(historical_shocks)){
			for (w in base::colnames(historical_shocks)){
				if (v == w) next
				pearson[v,w] <- stats::cor.test(
					base::as.numeric(historical_shocks[,v]),
					base::as.numeric(historical_shocks[,w]),
					method="pearson"
				)$p.value
				spearman[v,w] <- stats::cor.test(
					base::as.numeric(historical_shocks[,v]),
					base::as.numeric(historical_shocks[,w]),
					method="spearman",
					exact=FALSE
				)$p.value
				kendall[v,w] <- stats::cor.test(
					base::as.numeric(historical_shocks[,v]),
					base::as.numeric(historical_shocks[,w]),
					method="kendall",
					exact=FALSE
				)$p.value
		}}
		correlation_tests <- base::list(pearson = pearson, spearman = spearman, kendall = kendall)

		if (correlated_shocks == FALSE){
		       var_cov_historical_shocks <-
			       var_cov_historical_shocks *
			       base::diag(x=1,nrow=base::nrow(var_cov_historical_shocks))
		}

		withr::with_seed(
			seed = shocks_seed,
			code = {

				for (k in base::seq_len(n_stochastic_scenarios)){

					# Randomization (creation of the shocks per se)
					if (!base::is.null(tic)){
						temp <- MASS::mvrnorm(n=size, mu=base::c(0,0,0,0,0), Sigma=var_cov_historical_shocks)
						temp <- temp[,base::c(i,ti,tic,g,p)] # Ensures that variables (columns) are appropriately positioned
					} else {
						temp <- MASS::mvrnorm(n=size, mu=base::c(0,0,0,0),   Sigma=var_cov_historical_shocks)
						temp <- temp[,base::c(i,ti,g,p)] # Ensures that variables (columns) are appropriately positioned
					}

					# Converts shocks to an object of class "xts"
					temp <- xts::xts(
						x = base::data.frame(temp),
						order.by = base::seq.Date(
							from = shocks_first_date,
							to = shocks_last_date,
							by = shocks_frequency
						)
					)
					if (!base::is.null(tic)){
						base::colnames(temp) = base::c(i,ti,tic,g,p) # Applies standard names
					} else {
						base::colnames(temp) = base::c(i,ti,g,p) # Applies standard names
					}

					# Stores the shocks generated in this loop iteration into the final list
					shocks[[k]] <- temp

				}
			}
		)
	}




	base::return(base::list(
		historical_shocks = historical_shocks,
		shocks = shocks,
		var_cov_historical_shocks = var_cov_historical_shocks,
		correlation_tests = correlation_tests
	))
}
