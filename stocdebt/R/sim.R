sim <- function(

	first_date_to_simulate,
	last_date_to_simulate,

	realized_data,
	baseline_scenario = NULL,

	compute_deterministic_d_and_r = FALSE,
	baseline_as_median = TRUE,

	n_stochastic_scenarios,
	shocks,
	persistent_shocks = NULL,
	persistent_shocks_duration = NULL,
	accumulate_quarterly_shocks = FALSE,

	d,
	i,
	ti,
	tic = NULL,
	r,
	r_tic = NULL,
	g,
	p,

	no_shocks = NULL

){




	# PART 1/4: preparation
	# ______________________________________________________________________________________________
	# ______________________________________________________________________________________________

	# Validating variables informed by the user

	if (!base::is.null(tic) & !base::is.null(r_tic)){
		for (v in base::list(d,i,ti,tic,r,r_tic,g,p)){
			if (
			    (base::is.atomic(v) & !base::is.character(v)) |
			    (base::is.list(v) & !all(base::sapply(v,FUN=base::is.character)))
			) base::stop("all variable names (d, i, ti, tic, r, r_tic, g, p) must be of classes \"character\" or \"list\". In the latter case, it must be a list of \"character\" objects.")
		}
		for (v in base::list(i,ti,tic,g,p)){
			if (!base::is.null(baseline_scenario)){
				the_names <- base::c("realized","baseline","shocks")
			} else {
				the_names <- base::c("realized","shocks")
			}

			if (base::is.list(v) & !base::all(the_names %in% base::names(v))) base::stop("when informing a list in arguments \"i\", \"ti\", \"tic\", \"g\", and \"p\", the list must contain elements named \"realized\", \"baseline\" (if \"baseline_scenario\" has been provided) and \"shocks\".")
		}
		for (v in base::list(d,r,r_tic)){
			if (!base::is.null(baseline_scenario)){
				the_names <- base::c("realized","baseline")
			} else {
				the_names <- "realized"
			}

			if (base::is.list(v) & !base::all(the_names %in% base::names(v))) base::stop("when informing a list in arguments \"d\", \"r\" and \"r_tic\", the list must contain elements named \"realized\" and \"baseline\" (if \"baseline_scenario\" has been provided).")
		}
	} else {
		for (v in base::list(d,i,ti,r,g,p)){
			if (
			    (base::is.atomic(v) & !base::is.character(v)) |
			    (base::is.list(v) & !all(base::sapply(v,FUN=base::is.character)))
			) base::stop("all variable names (d, i, ti, r, g, p) must be of classes \"character\" or \"list\". In the latter case, it must be a list of \"character\" objects.")
		}
		for (v in base::list(i,ti,g,p)){
			if (!base::is.null(baseline_scenario)){
				the_names <- base::c("realized","baseline","shocks")
			} else {
				the_names <- base::c("realized","shocks")
			}

			if (base::is.list(v) & !base::all(the_names %in% base::names(v))) base::stop("when informing a list for arguments \"i\", \"ti\", \"g\" and \"p\", the list must contain elements named \"realized\", \"baseline\" (if \"baseline_scenario\" has been provided) and \"shocks\".")
		}
		for (v in base::list(d,r)){
			if (!base::is.null(baseline_scenario)){
				the_names <- base::c("realized","baseline")
			} else {
				the_names <- "realized"
			}

			if (base::is.list(v) & !base::all(the_names %in% base::names(v))) base::stop("when informing a list for arguments \"d\" and \"r\", the list must contain elements named \"realized\" and \"baseline\" (if \"baseline_scenario\" has been provided).")
		}
	}

	# Extracting variable names in an organized way

	d_real <- base::ifelse(base::is.atomic(d),d,d$realized)
	i_real <- base::ifelse(base::is.atomic(i),i,i$realized)
	ti_real <- base::ifelse(base::is.atomic(ti),ti,ti$realized)
	r_real <- base::ifelse(base::is.atomic(r),r,r$realized)
	g_real <- base::ifelse(base::is.atomic(g),g,g$realized)
	p_real <- base::ifelse(base::is.atomic(p),p,p$realized)

	i_sho <- base::ifelse(base::is.atomic(i),i,i$shocks)
	ti_sho <- base::ifelse(base::is.atomic(ti),ti,ti$shocks)
	g_sho <- base::ifelse(base::is.atomic(g),g,g$shocks)
	p_sho <- base::ifelse(base::is.atomic(p),p,p$shocks)

	if (!base::is.null(tic)){
		tic_real <- base::ifelse(base::is.atomic(tic),tic,tic$realized)
		tic_sho <- base::ifelse(base::is.atomic(tic),tic,tic$shocks)
		if (!base::is.null(baseline_scenario)){
			tic_base <- base::ifelse(base::is.atomic(tic),tic,tic$baseline)
		}
	}


	if (!base::is.null(r_tic)){
		r_tic_real <- base::ifelse(base::is.atomic(r_tic),r_tic,r_tic$realized)
		if (!base::is.null(baseline_scenario)){
			r_tic_base <- base::ifelse(base::is.atomic(r_tic),r_tic,r_tic$baseline)
		}
	}

	if (!base::is.null(baseline_scenario)){
		d_base <- base::ifelse(base::is.atomic(d),d,d$baseline)
		i_base <- base::ifelse(base::is.atomic(i),i,i$baseline)
		ti_base <- base::ifelse(base::is.atomic(ti),ti,ti$baseline)
		r_base <- base::ifelse(base::is.atomic(r),r,r$baseline)
		g_base <- base::ifelse(base::is.atomic(g),g,g$baseline)
		p_base <- base::ifelse(base::is.atomic(p),p,p$baseline)
	}

	# Enforcing argument "realized_data", if in yearly frequency, to be positioned in December (to support the case when argument "shocks" is of quarterly frequency, since these two objects will be appended later in this function)
	if (xts::periodicity(realized_data)$label == "year"){
		zoo::index(realized_data) <- zoo::as.Date(base::format(zoo::index(realized_data),"%Y-12-%d"))
	}

	# Identifying the date lying one step ahead of the end of the realized series (useful for a validation procedure below)
	if (accumulate_quarterly_shocks){ # If the user REQUESTED shocks to be accumulated over the quarters up to a yearly total, so as to apply yearly shocks on an yearly baseline scenario

		temp <- base::seq.Date(
			from = base::as.Date(zoo::index(xts::last(realized_data))),
			by = xts::periodicity(realized_data)$label, # Periodicity here must be that of "realized_data", not "shocks"
			length = 2
		)
		temp[-1] -> first_period_ahead_of_realized_data

	} else { # If the user did NOT REQUEST shocks to be accumulated over quarters up to a yearly total

		temp <- base::seq.Date(
			from = base::as.Date(zoo::index(xts::last(realized_data))),
			by = xts::periodicity(shocks[[1]])$label, # Periodicity here must be that of "shocks", not "realized_data", to support the case when these objects have different periodicities (e.g. quarterly and yearly, respectively)
			length = 2
		)
		temp[-1] -> first_period_ahead_of_realized_data
	}

	# Further validations of the arguments set by the user
  	# ------------------------------------------

	if (accumulate_quarterly_shocks & xts::periodicity(shocks[[1]])$label != "quarter") base::stop("argument \"accumulate_quarterly_shocks\" cannot be TRUE when argument \"shocks\" is not of quarterly frequency.")

	if (!base::class(first_date_to_simulate) %in% base::c("character","Date")) base::stop("argument \"first_date_to_simulate\" must be of class \"character\" or \"Date\".")

	if (!base::class(last_date_to_simulate) %in% base::c("character","Date")) base::stop("argument \"last_date_to_simulate\" must be of class \"character\" or \"Date\".")

	if (!base::is.null(tic) & base::is.null(r_tic) | base::is.null(tic) & !base::is.null(r_tic)) base::stop("arguments \"tic\" and \"r_tic\" must both be informed or both be omitted, but one cannot be omitted while the other is omitted.")

	if (base::any(base::is.na(utils::tail(realized_data,1)))) base::stop("all variables in argument \"realized_data\" must be different from NA in the last period.")

	if (first_period_ahead_of_realized_data != first_date_to_simulate){
		base::stop("argument \"first_date_to_simulate\" must be exactly one period ahead of the last period in argument \"realized_data\".")
	}

	if (base::length(shocks) != n_stochastic_scenarios) base::stop("the number of shock scenarios cannot differ from the number of stochastic scenarios (\"n_stochastic_scenarios\").")

	if (!base::is.null(baseline_scenario)){

		if (!base::is.null(tic) & !base::is.null(r_tic)){
			if(!base::all(base::c(i_base,ti_base,tic_base,g_base,p_base) %in% base::colnames(baseline_scenario))) base::stop("argument \"baseline_scenario\" does not contain all variables (i,ti,tic,g,p).")
		} else {
			if(!base::all(base::c(i_base,ti_base,g_base,p_base) %in% base::colnames(baseline_scenario))) base::stop("argument \"baseline_scenario\" does not contain all variables (i,ti,g,p).")
		}

		if (accumulate_quarterly_shocks == FALSE &
		    baseline_as_median == TRUE &
		    xts::periodicity(baseline_scenario)$label != xts::periodicity(shocks[[1]])$label){
			base::stop("arguments \"baseline_scenario\" and \"shocks\" cannot have different frequencies, unless \"accumulate_quarterly_shocks = TRUE\", OR \"baseline_as_median = FALSE\".")
		}

		if (xts::periodicity(baseline_scenario)$label == "quarter"){
			the_months_1 <- base::unique(base::format(zoo::index(baseline_scenario),"%m"))
			the_months_2 <- base::unique(base::format(zoo::index(shocks[[1]]),"%m"))

			if (base::length(the_months_1) < base::length(the_months_2)){
				if (!all(the_months_1 %in% the_months_2)) base::stop("quarters cannot be indicated by different months in arguments \"baseline_scenario\" and \"shocks\".\n  For example, if the second quarter is represented by June in \"baseline_scenario\", it cannot be represented by April neither May in \"shocks\".")
			} else {
				if (!all(the_months_2 %in% the_months_1)) base::stop("quarters cannot be indicated by different months in arguments \"baseline_scenario\" and \"shocks\".\n  For example, if the second quarter is represented by June in \"baseline_scenario\", it cannot be represented by April neither May in \"shocks\".")
			}
		}
	}

	the_months_1 <- base::unique(base::format(zoo::as.Date(c(first_date_to_simulate,last_date_to_simulate)),"%m"))
	the_months_2 <- base::unique(base::format(zoo::index(shocks[[1]]),"%m"))

	if (!all(the_months_1 %in% the_months_2)) base::stop("quarters cannot be indicated by different months in arguments \"first_date_to_simulate\", \"last_date_to_simulate\" and \"shocks\".\n  For example, if the second quarter is represented by June in \"first_date_to_simulate\", it cannot be represented by April neither May in \"shocks\".")

	temp <- base::seq.Date(
		from = zoo::as.Date(first_date_to_simulate),
		to = zoo::as.Date(last_date_to_simulate),
		by = xts::periodicity(shocks[[1]])$label
	)

	if (!all(temp %in% zoo::index(shocks[[1]]))) base::stop("the time interval between \"first_date_to_simulate\" and \"last_date_to_simulate\" must be within the time interval of \"shocks\".")

	if (!base::is.null(no_shocks)){

		if (!base::is.null(tic)){
			if (!all(no_shocks %in% base::c(i_sho,ti_sho,tic_sho,g_sho,p_sho))) base::stop("argument \"no_shocks\" must contain only variables provided in arguments \"i\", \"ti\", \"tic\", \"g\" and \"p\" (under the name \"shocks\" if these are lists).")
		} else {
			if (!all(no_shocks %in% c(i_sho,ti_sho,g_sho,p_sho))) base::stop("argument \"no_shocks\" must contain only variables provided in arguments \"i\", \"ti\", \"g\" and \"p\" (under the name \"shocks\" if these are lists).")
		}
		if (any(base::c(d_base,d_real,r_base,r_real) %in% no_shocks)) base::stop("argument \"no_shocks\" should not contain variables provided in arguments \"d\" and \"r\".")

	}

	if (!base::is.null(persistent_shocks)){
		if (base::length(persistent_shocks) == 1){
			if (persistent_shocks != "all"){
				if (!base::all(persistent_shocks %in% base::colnames(shocks[[1]]))) base::stop("argument \"persistent_shocks\" must be equal to \"all\" or to variable names in argument \"shocks\".")
			}
		} else {
			if (!base::all(persistent_shocks %in% base::colnames(shocks[[1]]))) base::stop("argument \"persistent_shocks\" must be equal to \"all\" or to variable names in argument \"shocks\".")
		}
	} else {
		if (!base::is.null(persistent_shocks_duration)) base::warning("argument \"persistent_shocks_duration\" is ignored when argument \"persistent_shocks\" is NULL.")
	}

	


	# Ensures that dates informed as arguments are of class 'Date'
	# ------------------------------------------

	first_date_to_simulate <- zoo::as.Date(first_date_to_simulate)
	last_date_to_simulate <- zoo::as.Date(last_date_to_simulate)

	# Initializes the list which will store all scenarios to be generated
  	# ------------------------------------------

	scenarios <- base::list()

	# Storing dates, which will be part of this function output (they will be useful for other functions of the "stocdebt" package)
  	# ------------------------------------------

	dates <- base::list()
	dates[["first_date_to_simulate"]] <- first_date_to_simulate
	dates[["last_date_to_simulate"]] <- last_date_to_simulate
	dates[["last_realized_date"]] <- zoo::index(xts::last(realized_data))





	# PART 2/4: creation of the BASELINE SCENARIO for debt (in case it has been required by
	# the user, that is, in case argument "baseline_scenario" has not been omitted by the user)
	# ______________________________________________________________________________________________
	# ______________________________________________________________________________________________

	if (!base::is.null(baseline_scenario)){

		# Restricts the baseline scenario to the time interval required by the user
		baseline_scenario <- baseline_scenario[base::paste(
			first_date_to_simulate,
			"/",
			last_date_to_simulate,
			sep=""
		)]

		# Selects only variables of interest from the realized data and the baseline scenario
		if (!base::is.null(tic)){
			realized_data <- realized_data[,base::c(d_real,i_real,ti_real,tic_real,r_real,r_tic_real,g_real,p_real)]
		} else {
			realized_data <- realized_data[,base::c(d_real,i_real,ti_real,r_real,g_real,p_real)]
		}

		if (compute_deterministic_d_and_r == FALSE){

			# Keeps debt and real interest rate in the baseline scenario
			if (!base::is.null(tic)){
				baseline_scenario <- baseline_scenario[,base::c(d_base,i_base,ti_base,tic_base,r_base,r_tic_base,g_base,p_base)]
			} else {
				baseline_scenario <- baseline_scenario[,base::c(d_base,i_base,ti_base,r_base,g_base,p_base)]
			}

		} else {

			# Removes debt and real interest rate from the baseline scenario
			if (!base::is.null(tic)){
				baseline_scenario <- baseline_scenario[,base::c(i_base,ti_base,tic_base,g_base,p_base)]
			} else {
				baseline_scenario <- baseline_scenario[,base::c(i_base,ti_base,g_base,p_base)]
			}

			# Creates columns of debt and real interest rate in the baseline scenario, which will be filled later
			if (!base::is.null(tic)){
				baseline_scenario <- xts::merge.xts(NA,NA,NA,baseline_scenario,check.names=FALSE)
				base::colnames(baseline_scenario) <- base::c(d_base,r_base,r_tic_base,i_base,ti_base,tic_base,g_base,p_base)
			} else {
				baseline_scenario <- xts::merge.xts(NA,NA,baseline_scenario,check.names=FALSE)
				base::colnames(baseline_scenario) <- base::c(d_base,r_base,i_base,ti_base,g_base,p_base)
			}

		}

		# Combines the realized data with the baseline scenario, ensuring that variables (columns) are ordered identically in both objects (realized_data and baseline_scenario)
		# The column names of the realized data will be applied to the resulting columns (this is an arbitrary choice; other names could have been chosen as well, without any effect on the final outcome of the simulation)
		if (!base::is.null(tic)){
			x <- xts::rbind.xts(
				realized_data[,    base::c(d_real,i_real,ti_real,tic_real,r_real,r_tic_real,g_real,p_real)],
				baseline_scenario[,base::c(d_base,i_base,ti_base,tic_base,r_base,r_tic_base,g_base,p_base)]
			)
			base::colnames(x) <- base::c(d_real,i_real,ti_real,tic_real,r_real,r_tic_real,g_real,p_real)
		} else {
			x <- xts::rbind.xts(
				realized_data[,    base::c(d_real,i_real,ti_real,r_real,g_real,p_real)],
				baseline_scenario[,base::c(d_base,i_base,ti_base,r_base,g_base,p_base)]
			)
			base::colnames(x) <- base::c(d_real,i_real,ti_real,r_real,g_real,p_real)
		}

		if (compute_deterministic_d_and_r){

			# Debt dynamics (according to debt dynamics equations) and real interest rate dynamics
			line_of_first_date_to_simulate <- base::nrow(realized_data) + 1
			line_of_last_date_to_simulate <- base::nrow(x)

			y <- base::as.data.frame(x)

			if (!base::is.null(tic)){
				for (period in line_of_first_date_to_simulate:line_of_last_date_to_simulate){
					y[period,r_real] <- 100 * ( (1+y[period,i_real]/100) / (1+y[period,ti_real]/100) - 1 ) # Dynamics of real interest rate w.r.t. GDP deflator
					y[period,r_tic_real] <- 100 * ( (1+y[period,i_real]/100) / (1+y[period,tic_real]/100) - 1 ) # Dynamics of real interest rate w.r.t. consumer inflation
					y[period,d_real] <- 100 * ( (1 + y[period,tic_real]/100) / (1 + y[period,ti_real]/100) * (1 + y[period,r_tic_real]/100) / (1 + y[period,g_real]/100) * (y[period-1,d_real]/100) - (y[period,p_real]/100) ) # Debt dynamics using real interest rate w.r.t. consumer inflation
				}
			} else {
				for (period in line_of_first_date_to_simulate:line_of_last_date_to_simulate){
					y[period,r_real] <- 100 * ( (1+y[period,i_real]/100) / (1+y[period,ti_real]/100) - 1 ) # Dynamics of real interest rate w.r.t. GDP deflator
					y[period,d_real] <- 100 * ( (1 + y[period,r_real]/100) / (1 + y[period,g_real]/100) * (y[period-1,d_real]/100) - (y[period,p_real]/100) ) # Debt dynamics using real interest rate w.r.t. GDP deflator
				}
			}

			y <- xts::as.xts(y)
			zoo::index(y) <- zoo::index(x)
			x <- y
		}

		# Adds the object to the final list of scenarios
		x -> scenarios[["baseline_scenario"]]

	}




	# PART 3/4: creation of the STOCHASTIC scenarios for (i) debt determinants and (ii) debt itself
	# ______________________________________________________________________________________________
	# ______________________________________________________________________________________________

	# Initializes the list which will store the stochastic scenarios generated in this part of the code
	stochastic_scenarios <- base::list()

	# Simulates the scenarios
	for (k in base::seq_len(n_stochastic_scenarios)){

		# If the user required shocks to be accumulated over quarters, so as to render yearly shocks, accumulates them
		if (accumulate_quarterly_shocks){

			# Shocks
			accum_i <- xts::apply.yearly(x = shocks[[k]][,i_sho],FUN=sum)
			accum_ti <- xts::apply.yearly(x = shocks[[k]][,ti_sho],FUN=sum)
			if (!base::is.null(tic)){
				accum_tic <- xts::apply.yearly(x = shocks[[k]][,tic_sho],FUN=sum)
			}
			accum_g <- xts::apply.yearly(x = shocks[[k]][,g_sho],FUN=sum)
			accum_p <- xts::apply.yearly(x = shocks[[k]][,p_sho],FUN=sum)

			if (!base::is.null(tic)){
				shocks[[k]] <- base::Reduce(f=xts::merge.xts,base::list(accum_i,accum_ti,accum_tic,accum_g,accum_p))
				base::colnames(shocks[[k]]) <- base::c(i_sho,ti_sho,tic_sho,g_sho,p_sho)
			} else {
				shocks[[k]] <- base::Reduce(f=xts::merge.xts,base::list(accum_i,accum_ti,accum_g,accum_p))
				base::colnames(shocks[[k]]) <- base::c(i_sho,ti_sho,g_sho,p_sho)
			}
		}

		# If the user requested PERMANENT shocks, accumulates shocks over time
		if (!base::is.null(persistent_shocks)){
			if (base::length(persistent_shocks) == 1){
				if (persistent_shocks == "all"){
					if (!base::is.null(persistent_shocks_duration)){
						x <- base::cumsum(x=shocks[[k]])
						y <- xts::lag.xts(x,k=persistent_shocks_duration,na.pad=TRUE)
						y[is.na(y)] <- 0
						shocks[[k]] <- x-y # Accumulates only over the rolling window made of the number of periods informed in argument "persistent_shocks_duration"
					} else {
						shocks[[k]] <- base::cumsum(x=shocks[[k]]) # Accumulates indefinitely
					}
				} else {
					if (!base::is.null(persistent_shocks_duration)){
						x <- base::cumsum(x=shocks[[k]][,persistent_shocks])
						y <- xts::lag.xts(x,k=persistent_shocks_duration,na.pad=TRUE)
						y[is.na(y)] <- 0
						shocks[[k]][,persistent_shocks] <- x-y # Accumulates only over the rolling window made of the number of periods informed in argument "persistent_shocks_duration"
					} else {
						shocks[[k]][,persistent_shocks] <- base::cumsum(x=shocks[[k]][,persistent_shocks]) # Accumulates indefinitely
					}
				}
			} else {
				if (!base::is.null(persistent_shocks_duration)){
					x <- base::cumsum(x=shocks[[k]][,persistent_shocks])
					y <- xts::lag.xts(x,k=persistent_shocks_duration,na.pad=TRUE)
					y[is.na(y)] <- 0
					shocks[[k]][,persistent_shocks] <- x-y # Accumulates only over the rolling window made of the number of periods informed in argument "persistent_shocks_duration"
				} else {
					shocks[[k]][,persistent_shocks] <- base::cumsum(x=shocks[[k]][,persistent_shocks]) # Accumulates indefinitely
				}
			}
		}

		# Creates new time series for the DETERMINANTS of debt, by
		# - applying shocks on the respective variables in the BASELINE scenario (if argument baseline_scenario is not NULL); or 
		# - departing from the last observed period (if baseline_scenario = NULL)
		if (!base::is.null(baseline_scenario)){
			# Extracts only future values, from the object containing the baseline scenario
			x <- scenarios[["baseline_scenario"]][base::paste(first_date_to_simulate,"/",last_date_to_simulate,sep="")]
			# Note: despite the column names in x (below) end with the suffix "real" (e.g. i_real), x actually contains baseline values (beyond realized values), which have been stacked together earlier in this code (if argument baseline_scenario is not NULL)
			i_  <- x[,i_real] + shocks[[k]][,i_sho] * base::ifelse(i_sho %in% no_shocks, 0, 1)
			ti_ <- x[,ti_real] + shocks[[k]][,ti_sho] * base::ifelse(ti_sho %in% no_shocks, 0, 1)
			if (!base::is.null(tic)){
				tic_ <- x[,tic_real] + shocks[[k]][,tic_sho] * base::ifelse(tic_sho %in% no_shocks, 0, 1)
			}
			g_  <- x[,g_real] + shocks[[k]][,g_sho] * base::ifelse(g_sho %in% no_shocks, 0, 1)
			p_  <- x[,p_real] + shocks[[k]][,p_sho] * base::ifelse(p_sho %in% no_shocks, 0, 1)
		} else {
			# Applies "shocks on shocks" (it is just an arbitrary strategy to address the absence of a baseline scenario)
			shocks[[k]][,i_sho]  <- base::cumsum(x=shocks[[k]][,i_sho])
			shocks[[k]][,ti_sho] <- base::cumsum(x=shocks[[k]][,ti_sho])
			if (!base::is.null(tic)){
				shocks[[k]][,tic_sho] <- base::cumsum(x=shocks[[k]][,tic_sho])
			}
			shocks[[k]][,g_sho]  <- base::cumsum(x=shocks[[k]][,g_sho])
			shocks[[k]][,p_sho]  <- base::cumsum(x=shocks[[k]][,p_sho])

			i_  <- base::as.numeric(xts::last(realized_data[,i_real])) + shocks[[k]][,i_sho] * base::ifelse(i_sho %in% no_shocks, 0, 1)
			ti_ <- base::as.numeric(xts::last(realized_data[,ti_real])) + shocks[[k]][,ti_sho] * base::ifelse(ti_sho %in% no_shocks, 0, 1)
			if (!base::is.null(tic)){
				tic_ <- base::as.numeric(xts::last(realized_data[,tic_real])) + shocks[[k]][,tic_sho] * base::ifelse(tic_sho %in% no_shocks, 0, 1)
			}
			g_  <- base::as.numeric(xts::last(realized_data[,g_real])) + shocks[[k]][,g_sho] * base::ifelse(g_sho %in% no_shocks, 0, 1)
			p_  <- base::as.numeric(xts::last(realized_data[,p_real])) + shocks[[k]][,p_sho] * base::ifelse(p_sho %in% no_shocks, 0, 1)
		}

		if (!base::is.null(tic)){
			new_stochastic_scenario <- base::Reduce(f=xts::merge.xts,base::list(i_,ti_,tic_,g_,p_))
			base::colnames(new_stochastic_scenario) <- base::c(i_real,ti_real,tic_real,g_real,p_real)
		} else {
			new_stochastic_scenario <- base::Reduce(f=xts::merge.xts,base::list(i_,ti_,g_,p_))
			base::colnames(new_stochastic_scenario) <- base::c(i_real,ti_real,g_real,p_real)
		}

		# Creates columns for debt and real interest rate (in the NEW scenario), which will be filled later
		if (!base::is.null(tic)){
			new_stochastic_scenario <- xts::merge.xts(NA,NA,NA,new_stochastic_scenario,check.names=FALSE)
			base::colnames(new_stochastic_scenario) <- base::c(d_real,r_real,r_tic_real,i_real,ti_real,tic_real,g_real,p_real)
		} else {
			new_stochastic_scenario <- xts::merge.xts(NA,NA,new_stochastic_scenario,check.names=FALSE)
			base::colnames(new_stochastic_scenario) <- base::c(d_real,r_real,i_real,ti_real,g_real,p_real)
		}

		# Combines realized data with the NEW scenario, ensuring that variables (columns) are identically ordered in both objects (realized_data and new_stochastic_scenario)
		realized_data <- realized_data[,base::colnames(new_stochastic_scenario)]
		x <- xts::rbind.xts(realized_data,new_stochastic_scenario)

		# Debt dynamics (according to the debt dynamics equation)
		line_of_first_date_to_simulate <- base::nrow(realized_data) + 1
		line_of_last_date_to_simulate <- base::nrow(x)

		y <- base::as.data.frame(x)

		if (!base::is.null(tic)){
			for (period in line_of_first_date_to_simulate:line_of_last_date_to_simulate){
				y[period,r_real] <- 100 * {(1+y[period,i_real]/100) / (1+y[period,ti_real]/100) - 1} # Dynamics of the real interest rate w.r.t. GDP deflator
				y[period,r_tic_real] <- 100 * {(1+y[period,i_real]/100) / (1+y[period,tic_real]/100) - 1} # Dynamics of the real interest rate w.r.t. consumer inflation
				y[period,d_real] <- 100 * ( (1 + y[period,tic_real]/100) / (1 + y[period,ti_real]/100) * (1 + y[period,r_tic_real]/100) / (1 + y[period,g_real]/100) * (y[period-1,d_real]/100) - (y[period,p_real]/100) ) # Debt dynamics using real interest rate w.r.t. consumer inflation
			}
		} else {
			for (period in line_of_first_date_to_simulate:line_of_last_date_to_simulate){
				y[period,r_real] <- 100 * {(1+y[period,i_real]/100) / (1+y[period,ti_real]/100) - 1} # Dynamics of the real interest rate w.r.t. GDP deflator
				y[period,d_real] <- 100 * {(1 + y[period,r_real]/100)/(1 + y[period,g_real]/100) * y[period-1,d_real]/100 - y[period,p_real]/100} # Debt dynamics using real interest rate w.r.t. GDP deflator
			}
		}
		y <- xts::as.xts(y)
		zoo::index(y) <- zoo::index(x)
		y -> stochastic_scenarios[[k]]

	}

	# Adds the stochastic scenarios to the final list of scenarios
	scenarios[["stochastic_scenarios"]] <- stochastic_scenarios




	# PART 4/4: in case the user has requested, shifts the stochastic scenarios for debt, so that
	#           their median coincides with the baseline scenario, at every period. The same is done
	#           for the determinants of debt (GDP growth, interest rates, etc.).
	# ______________________________________________________________________________________________
	# ______________________________________________________________________________________________

	if (!base::is.null(baseline_scenario)){
		if (baseline_as_median){

			# Extracts only future values, from the object containing the baseline scenario
			x <- scenarios[["baseline_scenario"]][base::paste(first_date_to_simulate,"/",last_date_to_simulate,sep="")]
			periods <- zoo::index(x)

			# Note: despite the names of the variables in "the_vars" (below) and "x" (above) will be called using the suffix "real" (e.g. i_real), the final versions of the baseline and the stochastic scenarios treated here actually contain baseline and stochastic values (beyond realized values), which have been stacked together earlier in this code
			the_vars <- NULL
			if (!base::is.null(tic)){
				the_vars <- base::c(d_real,i_real,ti_real,tic_real,r_real,r_tic_real,g_real,p_real)
			} else {
				the_vars <- base::c(d_real,i_real,ti_real,r_real,g_real,p_real)
			}
			for (k in periods){
				for (v in the_vars){

					k <- zoo::as.Date(k)

					# Shifts the distribution median, so that it coincides with the values in the baseline scenario, at this period of the time series
					temp <- base::unlist(base::lapply(X=scenarios$stochastic_scenarios,FUN=function(x) x[k,v]))

					original_median <- stats::median(temp)
					target_median <- x[k,v]
					shift_factor <- target_median - original_median

					function_for_lapply <- function(x){
						# Shifts the value at period k for variable v
						x[k,v] <- x[k,v] + base::as.numeric(shift_factor)
						# Returns the entire scenario, but now with a shifted value at period k for variable v
						x
					}

					scenarios$stochastic_scenarios <- base::lapply(X=scenarios$stochastic_scenarios,FUN=function_for_lapply)
				}
			}
		}
	}



	base::return(base::list(
		scenarios = scenarios,
		dates = dates
	))

}
