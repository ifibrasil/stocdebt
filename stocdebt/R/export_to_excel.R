export_to_excel <- function(

	object,
	type,
	percentiles=c(0.1,0.25,0.5,0.75,0.9),
	filename,
	draw_charts = NULL,
	percentile_labels = FALSE,
	d = NULL,
	stock_flow_adjustments = NULL,
	date_format = "dd/mm/yyyy",
	chart_with_title = TRUE,
	x_axis_label = NULL,
	y_axis = NULL,
	color = "#005d89",
	seed_if_limited_export=1,
	max_to_export = 10,
	max_lags = 10,
	bartlett_level = 0.05,
	box_pierce_lag = 5,
	ljung_box_lag = 5

){

	# Initializes xlsx file
	wb <- openxlsx::createWorkbook()

	if (type == "simulation"){

		# Error handler
		if (!base::is.null(draw_charts)){
			var_names <- base::colnames(object$scenarios$stochastic_scenarios[[1]])
			if (base::length(draw_charts) == 1){
				if (draw_charts != "all"){
					if (!base::all(draw_charts %in% var_names)) base::stop("argument \"draw_charts\" must be equal to \"all\" or to variable names in the realized data used in function sim().")
				}
			} else {
				if (!base::all(draw_charts %in% var_names)) base::stop("argument \"draw_charts\" must be equal to \"all\" or to variable names in the realized data used in function sim().")
			}
		}

		# Preparation
		# -------------------------------------------------------------------------------------------

		# Checks if the number of simulated scenarios to be written into the xlsx
		# file is smaller than a fixed upper bound (this is to avoid an excessively
		# large file, and allow for the creation of charts within it).
		# If there are too many scenarios, selects a few of them (randomly).
		if (base::length(object$scenarios$stochastic_scenarios) > max_to_export){

			withr::with_seed(
				seed = seed_if_limited_export,
				code = {
					which_scenarios_to_export <- base::sample(
						x=base::seq_along(object$scenarios$stochastic_scenarios),
						size=max_to_export,
						replace=FALSE
					)
				}
			)
			stochastic_scenarios_to_export <- object$scenarios$stochastic_scenarios[which_scenarios_to_export]
			base::names(stochastic_scenarios_to_export) <- which_scenarios_to_export

		} else {

			which_scenarios_to_export <- base::seq_along(object$scenarios$stochastic_scenarios)
			stochastic_scenarios_to_export <- object$scenarios$stochastic_scenarios
			base::names(stochastic_scenarios_to_export) <- which_scenarios_to_export

		}

		# Worksheet for the baseline scenario
		# -------------------------------------------------------------------------------------------
		
		sheet_name <- "baseline"

		# Creates worksheet and writes a description
		# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

		openxlsx::addWorksheet(wb,sheetName = sheet_name, gridLines = FALSE)
		openxlsx::writeData(wb, sheet = sheet_name,x = "Realized data (past) and BASELINE scenario (future)")

		# Writes the first column, containing periods
		# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

		temp <- object$scenarios$baseline_scenario

		temp <- base::data.frame(Period = zoo::index(temp))
		openxlsx::writeData(wb,sheet = sheet_name,x = temp,startRow = 3,startCol = 1,colNames = TRUE,borders = "surrounding")

		# Writes the series from the baseline scenario
		# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

		openxlsx::writeData(
			wb,
			sheet = sheet_name,
			x = object$scenarios$baseline_scenario,
			withFilter = FALSE,
			startRow = 3,
			startCol = 2,
			keepNA = FALSE,
			colNames = TRUE,
			borders = "surrounding"
		)

		# Worksheet for the stochastic scenarios
		# -------------------------------------------------------------------------------------------

		sheet_name <- "stochastic"

		# Creates worksheet and writes a description
		# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

		openxlsx::addWorksheet(wb,sheetName = sheet_name, gridLines = FALSE)
		openxlsx::writeData(wb, sheet = sheet_name,x = "Realized data (past) and STOCHASTIC scenarios (future)")

		# Writes the first column, containing periods
		# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

		temp <- stochastic_scenarios_to_export[[1]] # Uses periods from the first stochastic scenario (could be any stochastic scenario)

		temp <- base::data.frame(Period = zoo::index(temp))
		openxlsx::writeData(wb,sheet = sheet_name,x = temp,startRow = 3,startCol = 1,colNames = TRUE,borders = "surrounding")

		# Writes the simulated series
		# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

		# Initializes column counter for the loop below
		the_column <- 2

		for (k in base::seq_along(stochastic_scenarios_to_export)){
			openxlsx::writeData(
				wb,
				sheet = sheet_name,
				x = stochastic_scenarios_to_export[[k]],
				withFilter = FALSE,
				startRow = 3,
				startCol = the_column,
				keepNA = FALSE,
				colNames = TRUE,
				borders = "surrounding"
			)
			the_column <- the_column + base::ncol(stochastic_scenarios_to_export[[k]])
		}

		# Worksheets for each variable:
		# one sheet per variable, containing the BASELINE AND STOCHASTIC SCENARIOS of the same variable
		# -------------------------------------------------------------------------------------------

		var_names <- base::colnames(object$scenarios$baseline_scenario)
		periods <- base::data.frame(Period = zoo::index(object$scenarios$baseline_scenario))

		# Generates suitable names for sheets
		edited_names <- base::tolower(base::substr(var_names,start=1,stop=17))
		# Ensures that there will not be repeated sheet names (this would cause an error while executing the code)
		temp <- base::duplicated(edited_names)
		if (base::any(temp)){
			temp[temp] <- 1:base::sum(temp) + 1
			edited_names[temp>0] <- base::paste0(edited_names[temp>0],"_",temp[temp>0])
		}

		for (n in base::seq_along(var_names)){

			sheet_name <- edited_names[n]

			# Creates worksheet and writes a description
			# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

			openxlsx::addWorksheet(wb,sheetName = sheet_name, gridLines = FALSE)
			openxlsx::writeData(wb, sheet = sheet_name,x = "Combined baseline and stochastic scenarios")
			openxlsx::writeData(wb, sheet = sheet_name,x = var_names[n],startRow = 2)

			# Writes the first column, containing periods
			# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

			openxlsx::writeData(wb,sheet = sheet_name,x = periods,startRow = 3,startCol = 1,colNames = TRUE,borders = "surrounding")

			# Writes the series from the baseline scenario
			# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

			temp <- base::data.frame(base::as.numeric(object$scenarios$baseline_scenario[,var_names[n]]))
			base::names(temp) <- "Baseline scenario"
			openxlsx::writeData(
				wb,
				sheet = sheet_name,
				x = temp,
				withFilter = FALSE,
				startRow = 3,
				startCol = 2,
				keepNA = FALSE,
				colNames = TRUE,
				borders = "surrounding"
			)

			# Writes the simulated series
			# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

			# Initializes column counter for the loop below
			the_column <- 3

			for (k in base::seq_along(stochastic_scenarios_to_export)){
				temp <- base::data.frame(base::as.numeric(stochastic_scenarios_to_export[[k]][,var_names[n]]))
				base::names(temp) <- base::paste("Stochastic scenario",which_scenarios_to_export[k])
				openxlsx::writeData(
					wb,
					sheet = sheet_name,
					x = temp,
					withFilter = FALSE,
					startRow = 3,
					startCol = the_column,
					keepNA = FALSE,
					colNames = TRUE,
					borders = "surrounding"
				)
				the_column <- the_column + 1
			}

		}

		# Creates 2 worksheets for each variable:
		# - one sheet, containing the QUANTILES
		# - another sheet, containing the DIFFERENCES between quantiles, needed to create a fan chart for this variable using the "stacked area" chart type
		# -------------------------------------------------------------------------------------------

		fc <- fanchart_percentiles(s=object,percentiles=percentiles)$the_percentiles

		var_names <- base::colnames(object$scenarios$baseline_scenario)
		periods <- base::data.frame(Period = zoo::index(object$scenarios$baseline_scenario))

		for (n in base::seq_along(var_names)){

			# Group 1 of 2: QUANTILES ===========================
			# ===================================================

			sheet_name <- base::paste("quant_",edited_names[n],sep="")

			# Creates worksheet and writes a description
			# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

			openxlsx::addWorksheet(wb,sheetName = sheet_name, gridLines = FALSE)
			openxlsx::writeData(wb, sheet = sheet_name,x = base::paste("QUANTILES of all simulated ",base::length(object$scenarios$stochastic_scenarios)," stochastic scenarios",sep=""))
			openxlsx::writeData(wb, sheet = sheet_name,x = var_names[n],startRow = 2)

			# Writes the first column, containing periods
			# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

			openxlsx::writeData(wb,sheet = sheet_name,x = periods,startRow = 3,startCol = 1,colNames = TRUE,borders = "surrounding")

			# Writes the series of quantiles
			# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

			temp <- fc[[var_names[n]]]
			# Adds the baseline scenario to the table
			temp <- xts::merge.xts(object$scenarios$baseline_scenario[,var_names[n]],temp,check.names=FALSE)
			base::colnames(temp) <- base::c("Baseline scenario",base::colnames(fc[[var_names[n]]]))

			openxlsx::writeData(
				wb,
				sheet = sheet_name,
				x = temp,
				withFilter = FALSE,
				startRow = 3,
				startCol = 2,
				keepNA = FALSE,
				colNames = TRUE,
				borders = "surrounding"
			)

			# Group 2 of 2: DIFFERENCES BETWEEN QUANTILES =======
			# ===================================================

			sheet_name <- base::paste("diff_",edited_names[n],sep="")

			# Creates worksheet and writes a description
			# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

			openxlsx::addWorksheet(wb,sheetName = sheet_name, gridLines = FALSE)
			openxlsx::writeData(wb, sheet = sheet_name,x = "DIFFERENCES between quantiles of the stochastic scenarios")
			openxlsx::writeData(wb, sheet = sheet_name,x = var_names[n],startRow = 2)

			# Writes the first column, containing periods
			# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

			openxlsx::writeData(wb,sheet = sheet_name,x = periods,startRow = 3,startCol = 1,colNames = TRUE,borders = "surrounding")

			# Calculates differences between quantiles
			# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

			# Sorts columns in ascending order of the percentiles
			temp <- fc[[var_names[n]]][,base::sort(base::colnames(fc[[var_names[n]]]))]

			# Imputes NA value for quantiles at past dates (realized time series)
			temp2 <- temp[base::paste("/",object$dates$last_realized_date,sep="")]
			the_line <- base::nrow(temp2)-1
			NA -> temp[base::seq_len(the_line),]

			# Replaces each quantile with its difference with respect to the quantile immediately below it
			temp2 <- temp
			for (i in base::seq_len(base::ncol(temp))){
				if (i==1) next
				temp[,i] <- temp2[,i] - temp2[,i-1]
			}

			# Adds the baseline scenario to the table
			temp <- xts::merge.xts(object$scenarios$baseline_scenario[,var_names[n]],temp,check.names=FALSE)
			base::colnames(temp) <- base::c("Baseline scenario",base::colnames(fc[[var_names[n]]]))

			# Writes the series of differences between quantiles
			# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

			openxlsx::writeData(
				wb,
				sheet = sheet_name,
				x = temp,
				withFilter = FALSE,
				startRow = 3,
				startCol = 2,
				keepNA = FALSE,
				colNames = TRUE,
				borders = "surrounding"
			)
		}
		
		# Saves (writes) the workbook
		openxlsx::saveWorkbook(wb,file = filename,overwrite = TRUE)

		# Draws charts in the xlsx file, if requested by the user
		if (!base::is.null(draw_charts)){

			if (!reticulate::py_available(initialize = TRUE)) base::stop("you must have Python installed on your computer to use option \"draw_charts\".")
			for (m in c("pandas","numpy","xlsxwriter","openpyxl")){
			  if (!reticulate::py_module_available(m)) base::stop("you must have Python modules \"pandas\", \"numpy\", \"xlsxwriter\" and \"openpyxl\" installed on your computer.")
			}

			# Selects the appropriate variable names
			if (base::length(draw_charts) == 1){
				if (draw_charts != "all"){
					edited_names <- edited_names[var_names %in% draw_charts]
				}
			} else {
				edited_names <- edited_names[var_names %in% draw_charts]
			}

			# Draws charts using Python
			adds_fancharts <- NULL # This command has the sole purpose of solving an issue raised by R CMD check
			reticulate::source_python(base::system.file("python/adds_fancharts.py",package="stocdebt"))
			# Converts R list to Python dictionary
			y_axis <- reticulate::dict(y_axis)
			adds_fancharts(
				excel_input = filename,
				excel_output = filename,
				vars_fanchart = edited_names,
				percentile_labels = percentile_labels,
				the_date_format = date_format,
				chart_with_title = chart_with_title,
				x_axis_label = x_axis_label,
				y_axis = y_axis,
				color = color
			)
		}
	}






	if (type == "debt_dynamics_equation"){

		# Worksheet for the data
		# -------------------------------------------------------------------------------------------

		sheet_name <- "data"

		# Creates worksheet and writes a description
		# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

		openxlsx::addWorksheet(wb,sheetName = sheet_name, gridLines = FALSE)
		openxlsx::writeData(wb, sheet = sheet_name,x = "Realized data and simulated paths")

		# Writes the first column, containing periods
		# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

		temp <- object$paths

		temp <- base::data.frame(Period = zoo::index(temp))
		openxlsx::writeData(wb,sheet = sheet_name,x = temp,startRow = 3,startCol = 1,colNames = TRUE,borders = "surrounding")

		# Writes the series
		# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

		openxlsx::writeData(
			wb,
			sheet = sheet_name,
			x = object$paths,
			withFilter = FALSE,
			startRow = 3,
			startCol = 2,
			keepNA = FALSE,
			colNames = TRUE,
			borders = "surrounding"
		)

		# Saves (writes) the workbook
		openxlsx::saveWorkbook(wb,file = filename,overwrite = TRUE)

		# Draws charts in the xlsx file, if requested by the user
		if (draw_charts == TRUE){

			if (!reticulate::py_available(initialize = TRUE)) base::stop("you must have Python installed on your computer to use option \"draw_charts\".")
			for (m in c("pandas","numpy","xlsxwriter","openpyxl")){
			  if (!reticulate::py_module_available(m)) base::stop("you must have Python modules \"pandas\", \"numpy\", \"xlsxwriter\" and \"openpyxl\" installed on your computer.")
			}
			if (base::is.null(d)) base::stop("argument \"d\" cannot be NULL when \"draw_charts\" is TRUE and \"type\" is \"debt_dynamics_equation\".")

			# Draws charts using Python
			adds_dde_charts <- NULL # This command has the sole purpose of solving an issue raised by R CMD check
			reticulate::source_python(base::system.file("python/adds_dde_charts.py",package="stocdebt"))
			# Converts R list to Python dictionary
			y_axis <- reticulate::dict(y_axis)
			adds_dde_charts(
				excel_input = filename,
				excel_output = filename,
				d = d,
				stock_flow_adjustments = stock_flow_adjustments,
				the_date_format = date_format,
				chart_with_title = chart_with_title,
				x_axis_label = x_axis_label
			)
		}

	}






	if (type == "correlograms"){

		# Worksheet for the data
		# -------------------------------------------------------------------------------------------

		# Creates worksheet and writes a description
		# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

		openxlsx::addWorksheet(wb,sheetName = "autocorr", gridLines = FALSE)
		openxlsx::addWorksheet(wb,sheetName = "partial autocorr", gridLines = FALSE)

		openxlsx::writeData(wb, sheet = "autocorr",x = "Autocorrelations of realized data")
		openxlsx::writeData(wb, sheet = "partial autocorr",x = "PARTIAL autocorrelations of realized data")

		# Computes correlations and critical values
		# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

		# Initializes lists
		corr <- base::list()
		par_corr <- base::list()
		bartlett <- base::list()
		box_pierce <- base::list()
		ljung_box <- base::list()

		for (v in base::names(object)){

			# Partial autocorrelation -------------------------------
			temp1 <- stats::acf(x=object[,v],plot=FALSE,type="partial",lag.max=max_lags)

			temp2 <- xts::xts(x=temp1$acf[,,1],order.by=zoo::as.Date(base::seq_along(temp1$acf[,,1]))) # Dates are used here temporarily, as an auxiliary tool to ensure exact alignment of rows in the final matrix to be exported
			par_corr[[v]] <- temp2

			# Autocorrelation and tests -----------------------------
			temp1 <- stats::acf(x=object[,v],plot=FALSE,type="correlation",lag.max=max_lags)

			temp2 <- xts::xts(x=temp1$acf[,,1],order.by=zoo::as.Date(base::seq_along(temp1$acf[,,1])-1)) # Dates are used here temporarily, as an auxiliary tool to ensure exact alignment of rows in the final matrix to be exported
			corr[[v]] <- temp2

			temp3 <- stats::qnorm(p=(1 + (1-bartlett_level))/2)/base::sqrt(temp1$n.used) # Bartlett's critical value. Null hypothesis: process is white noise (no autocorrelation).
			temp3 <- base::round(temp3,digits=5)
			bartlett[[v]] <- base::paste("[-",temp3," , +",temp3,"]",sep="")

			temp4 <- stats::Box.test(x=temp1$acf[,,1],lag=box_pierce_lag,type = "Box-Pierce")
			box_pierce[[v]] <- base::round(temp4$p.value,digits=5) # Box-Pierce p-value. Null hypothesis: process variables are independent over time.

			temp5 <- stats::Box.test(x=temp1$acf[,,1],lag=ljung_box_lag,type = "Ljung-Box")
			ljung_box[[v]] <- base::round(temp5$p.value,digits=5) # Ljung-Box p-value. Null hypothesis: process variables are independent over time (i.e. same as Box-Pierce).
		}

    		corr <- base::Reduce(f=xts::merge.xts,corr)
		corr <- base::as.data.frame(corr)
		base::rownames(corr) <- NULL
		corr <- base::cbind(base::seq(from=0,length.out=base::nrow(corr)),corr)
		base::names(corr) <- base::c("Lag",base::names(object))

    		par_corr <- base::Reduce(f=xts::merge.xts,par_corr)
		par_corr <- base::as.data.frame(par_corr)
		base::rownames(par_corr) <- NULL
		par_corr <- base::cbind(base::seq(from=1,length.out=base::nrow(par_corr)),par_corr)
		base::names(par_corr) <- base::c("Lag",base::names(object))

		n <- base::names(bartlett)
		bartlett <- base::as.data.frame(bartlett)
		bartlett <- base::cbind("Bartlett CI:",bartlett)
		base::names(bartlett) <- base::c("",n)

		n <- base::names(box_pierce)
		box_pierce <- base::as.data.frame(box_pierce)
		box_pierce <- base::cbind("Box-Pierce p-value:",box_pierce)
		base::names(box_pierce) <- base::c("",n)

		n <- base::names(ljung_box)
		ljung_box <- base::as.data.frame(ljung_box)
		ljung_box <- base::cbind("Ljung-Box p-value:",ljung_box)
		base::names(ljung_box) <- base::c("",n)

		# Writes the critical values and p-values of tests
		# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

		# Bartlett
		openxlsx::writeData(
			wb,
			sheet = "autocorr",
			x = bartlett,
			withFilter = FALSE,
			startRow = 3,
			startCol = 1,
			keepNA = FALSE,
			colNames = FALSE
		)

		# Box-Pierce
		openxlsx::writeData(
			wb,
			sheet = "autocorr",
			x = box_pierce,
			withFilter = FALSE,
			startRow = 4,
			startCol = 1,
			keepNA = FALSE,
			colNames = FALSE
		)

		# Ljung-Box
		openxlsx::writeData(
			wb,
			sheet = "autocorr",
			x = ljung_box,
			withFilter = FALSE,
			startRow = 5,
			startCol = 1,
			keepNA = FALSE,
			colNames = FALSE
		)

		# Writes autocorrelations and partial autocorrelations
		# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

		openxlsx::writeData(
			wb,
			sheet = "autocorr",
			x = corr,
			withFilter = FALSE,
			startRow = 7,
			startCol = 1,
			keepNA = FALSE,
			colNames = TRUE,
			borders = "surrounding"
		)

		openxlsx::writeData(
			wb,
			sheet = "partial autocorr",
			x = par_corr,
			withFilter = FALSE,
			startRow = 3,
			startCol = 1,
			keepNA = FALSE,
			colNames = TRUE,
			borders = "surrounding"
		)

		# Edits columns widths for better visualization of the results in the xlsx file
		# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

		openxlsx::setColWidths(
			wb,
			sheet = "autocorr",
			cols = base::c(1,2,3,4,5),
			widths = 17
		)

		# Saves (writes) the workbook
		openxlsx::saveWorkbook(wb,file = filename,overwrite = TRUE)
	}






	if (type == "event_prob"){

		# Preparation
		# -------------------------------------------------------------------------------------------

		# Selects and arranges XTS OBJECTS
		# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

		selector <- base::unlist(base::lapply(X=object$p, FUN=xts::is.xts))
		the_names <- base::names(object$p)[selector]
		the_xts_objects <- base::Reduce(
			f = xts::merge.xts,
			x = object$p[selector]
		)
		base::colnames(the_xts_objects) <- the_names

		# Selects and arranges NON XTS OBJECTS
		# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

		selector <- !base::unlist(base::lapply(X=object$p, FUN=xts::is.xts))
		the_names <- base::names(object$p)[selector]
		the_non_xts_objects <- base::Reduce(
			f = base::c,
			x = object$p[selector]
		)
		the_non_xts_objects <- base::data.frame(the_names, the_non_xts_objects)
		base::colnames(the_non_xts_objects) <- base::c("","Estimate")

		# Worksheet for events which have ONE VALUE PER PERIOD (xts objects)
		# -------------------------------------------------------------------------------------------

		# Creates worksheet and writes a description
		# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

		openxlsx::addWorksheet(wb,sheetName = "probs_per_period", gridLines = FALSE)
		openxlsx::writeData(wb, sheet = "probs_per_period",x = "Estimated probabilities of events",startRow = 1)
		openxlsx::writeData(wb, sheet = "probs_per_period",x = "Probabilities that are meaningful to compute FOR EACH PERIOD",startRow = 2)

		# Writes the first column, containing periods
		# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

		temp <- base::data.frame(Period = zoo::index(the_xts_objects))
		openxlsx::writeData(wb, sheet = "probs_per_period", x = temp, startRow = 4, startCol = 1, colNames = TRUE)

		# Writes the xts object into the worksheet
		# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

		openxlsx::writeData(wb, sheet = "probs_per_period",x = the_xts_objects,startRow = 4,startCol = 2,keepNA = FALSE)

		# Edits cells for better visualization of the results in the xlsx file
		# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

		openxlsx::setColWidths(wb, sheet = "probs_per_period", cols = base::c(2,3), widths = 30)
		openxlsx::setRowHeights(wb, sheet = "probs_per_period", rows = 4, height = 70)
		wrap_style <- openxlsx::createStyle(wrapText = TRUE)
		openxlsx::addStyle(wb, "probs_per_period", style = wrap_style, rows = 4, cols = base::c(2,3))

		# Worksheet for events which have ONE OVERALL VALUE for the entire time span (non-xts objects)
		# -------------------------------------------------------------------------------------------

		# Creates worksheet and writes a description
		# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

		openxlsx::addWorksheet(wb,sheetName = "overall_probs", gridLines = FALSE)
		openxlsx::writeData(wb, sheet = "overall_probs",x = "Estimated probabilities of events",startRow = 1)
		openxlsx::writeData(wb, sheet = "overall_probs",x = "Probabilities that are meaningful to compute as an OVERALL PROBABILITY for the entire time span (not for each period)",startRow = 2)

		# Writes the table into the worksheet
		# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

		openxlsx::writeData(wb, sheet = "overall_probs",x = the_non_xts_objects,startRow = 4,keepNA = FALSE)

		# Edits cells for better visualization of the results in the xlsx file
		# ~  ~  ~  ~  ~  ~  ~  ~ ~  ~  ~  ~  ~  ~  ~

		openxlsx::setColWidths(wb, sheet = "overall_probs", cols = 1, widths = 80)



		# Saves (writes) the workbook
		openxlsx::saveWorkbook(wb,file = filename,overwrite = TRUE)
	}

}
