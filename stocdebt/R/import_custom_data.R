import_custom_data <- function(

	realized_data = NULL,
	baseline_scenario = NULL,
	shocks = NULL

){

	# Preparation
	# _______________________________________________________________________
	# _______________________________________________________________________

	# Validation of arguments
	if (!base::is.null(realized_data)){
		if (base::is.null(realized_data$sheet)) base::stop("list \"realized_data\" must contain an element \"sheet\".")
		if (base::length(realized_data$sheet) > 1) base::stop("only one sheet must be informed in list \"realized_data\".")
		if (!realized_data$frequency %in% base::c("year","quarter","month")) base::stop("element \"frequency\" of argument \"realized_data\" must be one of \"year\", \"quarter\" or \"month\".")
		if (!base::class(realized_data$start) %in% base::c("character","Date")) base::stop("element \"start\" of argument \"realized_data\" must be of class \"character\" or \"Date\".")
		if (!base::class(realized_data$end) %in% base::c("character","Date")) base::stop("element \"end\" of argument \"realized_data\" must be of class \"character\" or \"Date\".")
	}
	if (!base::is.null(baseline_scenario)){
		if (base::is.null(baseline_scenario$sheet)) base::stop("list \"baseline_scenario\" must contain an element \"sheet\".")
		if (base::length(baseline_scenario$sheet) > 1) base::stop("only one sheet must be informed in list \"baseline_scenario\".")
		if (!baseline_scenario$frequency %in% base::c("year","quarter","month")) base::stop("element \"frequency\" of argument \"baseline_scenario\" must be one of \"year\", \"quarter\" or \"month\".")
		if (!base::class(baseline_scenario$start) %in% base::c("character","Date")) base::stop("element \"start\" of argument \"baseline_scenario\" must be of class \"character\" or \"Date\".")
		if (!base::class(baseline_scenario$end) %in% base::c("character","Date")) base::stop("element \"end\" of argument \"baseline_scenario\" must be of class \"character\" or \"Date\".")
	}
	if (!base::is.null(shocks)){
		if (shocks$structure == "one per sheet" & base::is.null(shocks$sheets)) base::stop("list \"shocks\" must contain an element \"sheets\".")
		if (shocks$structure == "all in one sheet" & base::length(shocks$sheet) > 1) base::stop("only one sheet must be informed in list \"shocks\".")
		if (!shocks$frequency %in% base::c("year","quarter","month")) base::stop("element \"frequency\" of argument \"shocks\" must be one of \"year\", \"quarter\" or \"month\".")
		if (!base::class(shocks$start) %in% base::c("character","Date")) base::stop("element \"start\" of argument \"shocks\" must be of class \"character\" or \"Date\".")
		if (!base::class(shocks$end) %in% base::c("character","Date")) base::stop("element \"end\" of argument \"shocks\" must be of class \"character\" or \"Date\".")
	}

	# Ensures that dates informed as arguments are of class 'Date'

	if (!base::is.null(realized_data)){
		realized_data$start <- zoo::as.Date(realized_data$start)
		realized_data$end <- zoo::as.Date(realized_data$end)
	}

	if (!base::is.null(baseline_scenario)){
		baseline_scenario$start <- zoo::as.Date(baseline_scenario$start)
		baseline_scenario$end <- zoo::as.Date(baseline_scenario$end)
	}

	# Realized data
	# _______________________________________________________________________
	# _______________________________________________________________________

	if (!base::is.null(realized_data)){

		if (realized_data$file_type == "xlsx"){

			# Imports the table from the xlsx file
			temp <- openxlsx::read.xlsx(xlsxFile=realized_data$file_path, sheet=realized_data$sheet, detectDates=TRUE, sep.names=" ")

			# Error handlers
			the_index <- base::seq.Date(
				from = realized_data$start,
				to = realized_data$end,
				by = realized_data$frequency
			)

			if (realized_data$frequency == "year"){
				if (xts::first(the_index) != realized_data$start |
				    xts::last(the_index)  != realized_data$end) base::stop("start and end periods informed in list \"realized_data\" must be positioned on the same month-day pattern. For example, both on June 1, or both on December 1. This is only required for yearly data.")
			}

			if (base::length(the_index) != base::nrow(temp)) base::stop("the number of periods imported as realized data cannot be different from the number of periods implied by arguments \"start\", \"end\" and \"frequency\".")

			# Conversion to an "xts" object
			temp <- xts::xts(
				x = temp,
				order.by = the_index
			)
			temp <- temp[,-1] # Removes first column (with periods, not necessary anymore)

			# Ensures that data are of class "numeric" (instead of "character")
			temp <- base::apply(X=temp,MARGIN=2,FUN=base::as.numeric)
			temp <- xts::xts(x = temp, order.by = the_index)

			temp -> realized_data_output
		}

	} else {
		realized_data_output <- NULL
	}

	# Baseline scenario
	# _______________________________________________________________________
	# _______________________________________________________________________

	if (!base::is.null(baseline_scenario)){

		if (baseline_scenario$file_type == "xlsx"){

			# Imports the table from the xlsx file
			temp <- openxlsx::read.xlsx(xlsxFile=baseline_scenario$file_path, sheet=baseline_scenario$sheet, detectDates=TRUE, sep.names=" ")

			# Error handlers
			the_index <- base::seq.Date(
				from = baseline_scenario$start,
				to = baseline_scenario$end,
				by = baseline_scenario$frequency
			)

			if (baseline_scenario$frequency == "year"){
				if (xts::first(the_index) != baseline_scenario$start |
				    xts::last(the_index)  != baseline_scenario$end) base::stop("start and end periods informed in list \"baseline_scenario\" must be positioned on the same month-day pattern. For example, both on June 1, or both on December 1. This is only required for yearly data.")
			}

			if (base::length(the_index) != base::nrow(temp)) base::stop("the number of periods imported as baseline scenario cannot be different from the number of periods implied by arguments \"start\", \"end\" and \"frequency\".")

			# Conversion to an "xts" object
			temp <- xts::xts(
				x = temp,
				order.by = the_index
			)
			temp <- temp[,-1] # Removes first column (with periods, not necessary anymore)

			# Ensures that data are of class "numeric" (instead of "character")
			temp <- base::apply(X=temp,MARGIN=2,FUN=base::as.numeric)
			temp <- xts::xts(x = temp, order.by = the_index)

			temp -> baseline_scenario_output
		}

	} else {
		baseline_scenario_output <- NULL
	}

	# Shocks
	# _______________________________________________________________________
	# _______________________________________________________________________

	if (!base::is.null(shocks)){

		if (shocks$file_type == "xlsx"){
			
			if (shocks$structure == "all in one sheet"){

				# Imports the table from the xlsx file, and converts it to an "xts" object
				temp <- openxlsx::read.xlsx(xlsxFile=shocks$file_path, sheet=shocks$sheet, detectDates=TRUE, sep.names=" ")

				# Error handlers
				the_index <- base::seq.Date(
					from = shocks$start,
					to = shocks$end,
					by = shocks$frequency
				)

				if (shocks$frequency == "year"){
					if (xts::first(the_index) != shocks$start |
					    xts::last(the_index)  != shocks$end) base::stop("start and end periods informed in list \"shocks\" must be positioned on the same month-day pattern. For example, both on June 1, or both on December 1. This is only required for yearly data.")
				}

				if (base::length(the_index) != base::nrow(temp)) base::stop("the number of periods imported as shocks cannot be different from the number of periods implied by arguments \"start\", \"end\" and \"frequency\".")

				# Gathers columns with the same name into separate groups with all the shocks for the same variable (name)
				all_shocks_same_name <- base::list()

				unique_names <- base::unique(base::colnames(temp))
				unique_names <- unique_names[-1]
				for (n in unique_names){
					all_shocks_same_name[[n]] <- temp[,base::colnames(temp) %in% n]
				}

				# Creates a new list, wherein each element is a scenario of shocks
				number_of_scenarios <- (base::ncol(temp)-1)/base::length(unique_names)
				temp <- base::list()
				for (k in base::seq_len(number_of_scenarios)){
					temp1 <- base::lapply(X=all_shocks_same_name,FUN=function(x) x[,k])
					temp1 <- base::data.frame(temp1,check.names=FALSE)
					temp[[k]] <- xts::xts(
						x = temp1,
						order.by = the_index
					)

					# Ensures that data are of class "numeric" (instead of "character")
					temp[[k]] <- base::apply(X=temp[[k]],MARGIN=2,FUN=base::as.numeric)
					temp[[k]] <- xts::xts(x = temp[[k]], order.by = the_index)

				}

				shocks_output <- temp

			}

			if (shocks$structure == "one per sheet"){

				# Gets the names of the relevant sheets to be read from the xlsx file (in case the user informed numbers, instead of names, in argument "sheets")
				the_file <- openxlsx::loadWorkbook(file = shocks$file_path)
				if (base::is.integer(shocks$sheets)){
					shocks$sheets <- base::names(the_file)[shocks$sheets]
				}

				# Imports the table from the xlsx file, and converts it to an "xts" object
				shock_scenarios <- base::list()
				for (k in base::seq_along(shocks$sheets)){

					temp <- openxlsx::read.xlsx(xlsxFile=shocks$file_path, sheet=shocks$sheets[k], detectDates=TRUE, sep.names=" ")

					# Error handlers
					the_index <- base::seq.Date(
						from = shocks$start,
						to = shocks$end,
						by = shocks$frequency
					)

					if (shocks$frequency == "year"){
						if (xts::first(the_index) != shocks$start |
						    xts::last(the_index)  != shocks$end) base::stop("start and end periods informed in list \"shocks\" must be positioned on the same month-day pattern. For example, both on June 1, or both on December 1. This is only required for yearly data.")
					}

					if (base::length(the_index) != base::nrow(temp)) base::stop("the number of periods imported as shocks cannot be different from the number of periods implied by arguments \"start\", \"end\" and \"frequency\".")

					# Conversion to an "xts" object
					temp <- xts::xts(
						x = temp,
						order.by = the_index
					)
					temp <- temp[,-1] # Removes first column (with periods, not necessary anymore)

					# Ensures that data are of class "numeric" (instead of "character")
					temp <- base::apply(X=temp,MARGIN=2,FUN=base::as.numeric)
					temp <- xts::xts(x = temp, order.by = the_index)

					temp -> shock_scenarios[[k]]

				}

				shocks_output <- shock_scenarios

			}

		}

	} else {
		shocks_output <- NULL
	}






	base::return(base::list(
		realized_data = realized_data_output,
		baseline_scenario = baseline_scenario_output,
		shocks = shocks_output
	))

}
