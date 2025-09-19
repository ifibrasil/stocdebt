charts <- function(
	object,
	type="simulation",
	fan_chart_percentiles=NULL,
	seed_if_limited_export=1,
	fan_color = "#005d89",
	max_to_plot = 100,
	y_axis = list(),
	add_horizontal_line = list()
){

	# Initializes lists which will store final tables and charts, to be returned by the function
	list_of_tables <- base::list()
	list_of_charts <- base::list()




	# Charts for simulated paths for debt and its determinants
	# ____________________________________________________________________________________
	# ____________________________________________________________________________________
	# ____________________________________________________________________________________
	# ____________________________________________________________________________________

	if (type == "simulation"){

		# Error handler
		if (base::length(fan_chart_percentiles) < 2) base::stop("you must inform at least two percentiles.")

		# Simulated paths of each variable
		# _________________________________________________________________________________
		# _________________________________________________________________________________

		# Initializes two lists, which will contain, for each variable, tables and charts of simulated paths
		paths_tabs <- base::list()
		paths_charts <- base::list(dygraphs = base::list(), ggplot = base::list())

		# Checks if the number of shock scenarios to be plotted is smaller than a fixed upper bound
		# (this is to avoid a chart with an excessive quantity of time series, which would make the code slow to run)
		# If there are too many shock scenarios, selects a few of them (randomly)
		if (base::length(object$scenarios$stochastic_scenarios) > max_to_plot){

			withr::with_seed(
				seed = seed_if_limited_export,
				code = {
					which_scenarios_to_export <- base::sample(
						x=base::seq_along(object$scenarios$stochastic_scenarios),
						size=max_to_plot,
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

		# For each variable, combines all scenarios (baseline and stochastic) into a single object of class "xts"
		for (v in base::colnames(object$scenarios$stochastic_scenarios[[1]])){

			if (!base::is.null(object$scenarios$cenario_base)){
				# Adds the baseline scenario to the paths of variable "v"
				paths_tabs[[v]] <- object$scenarios$cenario_base[,v]
			}

			# Adds the stochastic scenarios to the paths of variable "v"
			for (k in base::seq_along(stochastic_scenarios_to_export)){

				temp <- base::lapply(stochastic_scenarios_to_export,function(x) x[,v])
				temp <- xts::xts(
					x = base::as.data.frame(temp),
					order.by = zoo::index(object$scenarios$stochastic_scenarios[[1]])
				)
				base::colnames(temp) <- which_scenarios_to_export
			}

			if (!base::is.null(object$scenarios$cenario_base)){
				# Combines the baseline scenario of variable "v" with its stochastic scenarios
				paths_tabs[[v]] <- xts::merge.xts(paths_tabs[[v]],temp,check.names=FALSE)
				base::colnames(paths_tabs[[v]]) <- base::c("Baseline scenario",base::colnames(temp))
			} else {
				# Stores the stochastic scenarios appropriately into the list
				paths_tabs[[v]] <- temp
			}

			# Creates a "dygraph" chart for the paths of each variable
				paths_charts[["dygraphs"]][[v]] <- 
					dygraphs::dygraph(paths_tabs[[v]], xlab = "Period",main = v) %>%
					dygraphs::dyHighlight( 
						highlightSeriesBackgroundAlpha = 0.2,
						highlightCircleSize = 0) %>%
					dygraphs::dyLegend(show = "never")

				# Sets limits of vertical axis, if requested by the user
				if (base::length(y_axis) > 0){
					paths_charts[["dygraphs"]][[v]] <- 
						paths_charts[["dygraphs"]][[v]] |>
						dygraphs::dyAxis("y", valueRange = base::c(y_axis[[v]][1], y_axis[[v]][2]))
				}

				# Adds horizontal line, if requested by the user
				if (base::length(add_horizontal_line) > 0){
					paths_charts[["dygraphs"]][[v]] <- 
						paths_charts[["dygraphs"]][[v]] |>
						dygraphs::dyLimit(
							limit = add_horizontal_line[[v]],
							label = base::as.character(add_horizontal_line[[v]]),
							strokePattern = "dashed"
						)
				}

			# Creates a "ggplot" chart for the paths of each variable

				# Converts the object of class "xts" into another, of class "tibble", of "long" format, as required by function ggplot()
				temp <- base::data.frame(  zoo::index(paths_tabs[[v]])  ,  paths_tabs[[v]]  )
				base::names(temp) <- base::c("Period",base::colnames(paths_tabs[[v]]))
				temp <- temp %>%
					tidyr::pivot_longer(cols = base::names(temp)[-1], names_to = "Scenario", values_to = v)

				# Creates the ggplot chart
				paths_charts[["ggplot"]][[v]] <-
					stats::na.omit(temp) %>%
					ggplot2::ggplot(ggplot2::aes(x = .data$Period, y = !!rlang::sym(v))) +
					ggplot2::geom_line(ggplot2::aes(color = .data$Scenario), linewidth = 0.8) +
					ggplot2::geom_line(data=dplyr::filter(temp,.data$Scenario == "Baseline scenario"),color="black",linewidth=2) +
					ggplot2::theme(legend.position = "none") +
					ggplot2::labs(title = v, y = NULL, x = NULL)

				# Sets limits of vertical axis, if requested by the user
				if (base::length(y_axis) > 0){
					paths_charts[["ggplot"]][[v]] <-
						paths_charts[["ggplot"]][[v]] +
						ggplot2::coord_cartesian(ylim = base::c(y_axis[[v]][1], y_axis[[v]][2]))
				}

				# Adds horizontal line, if requested by the user
				if (base::length(add_horizontal_line) > 0){
					paths_charts[["ggplot"]][[v]] <-
						paths_charts[["ggplot"]][[v]] +
						ggplot2::geom_hline(
							yintercept = add_horizontal_line[[v]],
							color = "black",
							linetype = "dashed"
						)
				}
		}

		list_of_tables[["paths"]] <- paths_tabs
		list_of_charts[["paths"]] <- paths_charts

		if (!base::is.null(fan_chart_percentiles)){

			# Ensures that the percentiles are correctly ordered (for better display of results)
			fan_chart_percentiles <- base::sort(fan_chart_percentiles)

			# Computes the percentiles
			fc <- fanchart_percentiles(s=object,percentiles=fan_chart_percentiles)$the_percentiles

			# Initializes two lists to store, for each variable, a table and a chart of its fan chart
			fancharts_tabs <- base::list(paths = base::list(), ribbons = base::list())
			fancharts_grafs <- base::list(dygraphs = base::list(), ggplot = base::list())

			for (v in base::names(fc)){

				# Stores the percentiles table
				if (!base::is.null(object$scenarios$cenario_base)){
					fancharts_tabs[["paths"]][[v]] <- xts::merge.xts(object$scenarios$cenario_base[,v],fc[[v]],check.names=FALSE)
					base::colnames(fancharts_tabs[["paths"]][[v]]) <- base::c("Baseline scenario",base::colnames(fc[[v]]))
				} else {
					fancharts_tabs[["paths"]][[v]] <- fc[[v]]
				}

				# Creates another version of the percentiles table
				# Converts each percentile to its difference with respect to the percentile just below it
				# The obtained differences will be used to make charts of "stacked areas" format

					# Orders the columns in ascending order of percentiles
					temp <- fc[[v]][,base::sort(base::colnames(fc[[v]]))]

					# Inputs NA values into past values of the percentiles (realized time series)
					temp3 <- temp[base::paste(
						"/",
						object$dates$last_realized_date,
						sep=""
					)]
					the_line <- base::nrow(temp3)-1
					NA -> temp[base::seq_len(the_line),]

					# Creates a list where each element will be the time series of the percentile,
					# accompanied by the upper and lower limits of its ribbon. Then, rearranges
					# the object to the "long" format, appropriate for function ggplot().

					ribbons <- base::list()
					for (i in base::seq_len(base::ncol(temp))){
						if(i==1){
							temp2 <- xts::merge.xts(temp[,i],temp[,i],check.names=FALSE)
							temp2 <- xts::merge.xts(temp[,i],temp2,check.names=FALSE)
						} else {
							temp2 <- xts::merge.xts(temp[,i],temp[,i-1],check.names=FALSE)
							temp2 <- xts::merge.xts(temp2,temp[,i],check.names=FALSE)
						}
						base::colnames(temp2) <- base::c(base::colnames(temp)[i],"ymin","ymax")
						temp2 <- base::data.frame(Period=zoo::index(temp2),temp2,check.names=FALSE)
						temp2 <- temp2 %>% tidyr::pivot_longer(cols = base::colnames(temp)[i],names_to = "Percentile",values_to = v)
						temp2 -> ribbons[[base::colnames(temp)[i]]]
					}

					# Stacks the list of ribbons, creating a single object
					temp <- dplyr::bind_rows(ribbons)

					if (!base::is.null(object$scenarios$cenario_base)){
						# Adds the baseline scenario to the table
						temp2 <- object$scenarios$cenario_base[,v]
						temp2 <- base::data.frame(zoo::index(temp2),"Baseline scenario",temp2,check.names=FALSE)
						base::names(temp2) <- base::c("Period","Percentile",v)
						base::rownames(temp2) <- NULL
						temp <- dplyr::bind_rows(temp2,temp)
					} else {
						# Adds the history to the table
						temp2 <- object$scenarios$stochastic_scenarios[[1]][,v]
						temp2 <- temp2[base::paste("/",object$dates$last_realized_date,sep="")]
						temp2 <- base::data.frame(zoo::index(temp2),"Baseline scenario",temp2,check.names=FALSE)
						base::names(temp2) <- base::c("Period","Percentile",v)
						base::rownames(temp2) <- NULL
						temp <- dplyr::bind_rows(temp2,temp)
					}

					# Stores into a list to be returned by the function
					temp -> fancharts_tabs[["ribbons"]][[v]]

				# Creates a "ggplot" fan chart for each varible

					# Converts one of the variables to class "factor", for appropriate use by function ggplot()
					table_for_plotting <- temp %>% dplyr::mutate(Percentile = base::factor(.data$Percentile,levels=base::c("Baseline scenario",base::rev(base::colnames(fc[[v]])))))

					# Creates vector of transparent colors, to be used to color the fan chart
					if (base::length(fan_chart_percentiles) %% 2 == 0){ # If the quantity of percentiles is EVEN

						temp <- base::length(fan_chart_percentiles) / 2
						temp <- base::seq(from=0,to=1,length.out=temp+2)
						# Removes first and last elements
						temp <- utils::head(temp,base::length(temp)-1)
						temp <- utils::tail(temp,base::length(temp)-1)
						temp <- scales::alpha(
							colour = fan_color,
							alpha = temp
						)
						the_colors <- base::c(temp,base::rev(temp))
						base::names(the_colors) <- base::sort(base::colnames(fc[[v]]))

					} else { # If the quantity of percentiles is ODD

						temp <- ( base::length(fan_chart_percentiles) - 1 ) / 2
						temp <- base::seq(from=0,to=1,length.out=temp+2)
						# Removes first and last elements
						temp <- utils::head(temp,base::length(temp)-1)
						temp <- utils::tail(temp,base::length(temp)-1)
						temp <- scales::alpha(
							colour = fan_color,
							alpha = temp
						)
						the_colors <- base::c("#FFFFFF00",temp,base::rev(temp))
						base::names(the_colors) <- base::sort(base::colnames(fc[[v]]))

					}

					# Creates an auxiliary table, which will help in label positioning in the chart
					temp4 <- base::data.frame(
						x = base::max(table_for_plotting$Period),
						y = base::as.numeric(utils::tail(fc[[v]],1)),
						z = base::colnames(fc[[v]]),
						row.names = NULL
					)

					# Specifies formatting of the dates to be placed in the horizontal axis
					if (xts::periodicity(fc[[v]])$label == "year") {the_date_labels <- "%Y"} # Yearly
					if (xts::periodicity(fc[[v]])$label == "quarter") {the_date_labels <- "%b/%y"} # Quarterly
					if (xts::periodicity(fc[[v]])$label == "month"){the_date_labels <- "%b/%y"} # Monthly

					# Creates the "ggplot" chart
					fancharts_grafs[["ggplot"]][[v]] <-
						table_for_plotting %>%
						ggplot2::ggplot(ggplot2::aes(x = .data$Period, y = !!rlang::sym(v))) +
						ggplot2::geom_line(data=dplyr::filter(table_for_plotting,.data$Percentile == "Baseline scenario"),color=fan_color,linewidth=1.5) +
						ggplot2::geom_ribbon(data=dplyr::filter(table_for_plotting,.data$Percentile != "Baseline scenario"),mapping=ggplot2::aes(ymin=.data$ymin,ymax=.data$ymax,fill=.data$Percentile)) +
						ggplot2::scale_fill_manual(values=the_colors) +
						ggplot2::labs(title = v, y = NULL, x = NULL) +
						ggplot2::theme(
							panel.background=ggplot2::element_blank(),
							panel.grid.major.y=ggplot2::element_line(color=scales::alpha("black",0.1)),
							panel.grid.minor=ggplot2::element_line(color=scales::alpha("black",0.1)),
							legend.title=ggplot2::element_blank(),
							legend.position="none"
						) +
						ggplot2::scale_x_date(date_labels=the_date_labels,breaks=table_for_plotting$Period)
					# If the stochastic scenarios of variable "v" have positive variance (i.e. its shocks have not been supressed when running sim()), then adds labels for percentiles on the "ggplot" chart
					is_variance_positive <- !base::all(
						stats::na.omit(table_for_plotting$ymin) == stats::na.omit(table_for_plotting$ymax)
					)
					if (is_variance_positive == TRUE){
						fancharts_grafs[["ggplot"]][[v]] <- 
							fancharts_grafs[["ggplot"]][[v]] +
							ggplot2::geom_text(data=temp4,ggplot2::aes(x=.data$x,y=.data$y,label=.data$z),nudge_x=80)
					}
					# Adds a line representing the middle percentile, in case the quantity of percentiles is odd
					if (base::length(fan_chart_percentiles) %% 2 == 1){

						# Selects the middle percentile
						temp <- base::ceiling(base::length(fan_chart_percentiles)/2)
						temp <- base::sort(base::colnames(fc[[v]]))[temp]

						fancharts_grafs[["ggplot"]][[v]] <-
							fancharts_grafs[["ggplot"]][[v]] +
							ggplot2::geom_line(data=stats::na.omit(table_for_plotting) %>% dplyr::filter(.data$Percentile == temp),color=fan_color)

					}
					# Sets limits of vertical axis, if requested by the user
					if (base::length(y_axis) > 0){
						fancharts_grafs[["ggplot"]][[v]] <-
							fancharts_grafs[["ggplot"]][[v]] +
							ggplot2::coord_cartesian(ylim = base::c(y_axis[[v]][1], y_axis[[v]][2]))
					}
					# Adds horizontal line, if requested by the user
					if (base::length(add_horizontal_line) > 0){
						fancharts_grafs[["ggplot"]][[v]] <-
							fancharts_grafs[["ggplot"]][[v]] +
							ggplot2::geom_hline(
								yintercept = add_horizontal_line[[v]],
								color = "black",
								linetype = "dashed"
							)
					}
			}

			list_of_tables[["fancharts (paths)"]] <- fancharts_tabs[["paths"]]
			list_of_tables[["fancharts (ribbons)"]] <- fancharts_tabs[["ribbons"]]
			list_of_charts[["fancharts"]] <- fancharts_grafs

		}


	}




	# Charts for shocks
	# ____________________________________________________________________________________
	# ____________________________________________________________________________________
	# ____________________________________________________________________________________
	# ____________________________________________________________________________________

	if (type == "shocks"){

		# Initializes two lists, which will store, for each variable, tables and charts of the respective shocks
		shocks_tabs <- base::list()
		shocks_grafs <- base::list(dygraphs = base::list(), ggplot = base::list())

		# Checks if the number of shock scenarios to be plotted is smaller than a fixed upper bound
		# (this is to avoid a chart with an excessive quantity of time series, which would make the code slow to run)
		# If there are too many shock scenarios, selects a few of them (randomly)
		if (base::length(object$shocks) > max_to_plot){

			withr::with_seed(
				seed = seed_if_limited_export,
				code = {
					which_scenarios_to_export <- base::sample(
						x=base::seq_along(object$shocks),
						size=max_to_plot,
						replace=FALSE
					)
				}
			)
			stochastic_scenarios_to_export <- object$shocks[which_scenarios_to_export]
			base::names(stochastic_scenarios_to_export) <- which_scenarios_to_export

		} else {

			which_scenarios_to_export <- base::seq_along(object$shocks)
			stochastic_scenarios_to_export <- object$shocks 
			base::names(stochastic_scenarios_to_export) <- which_scenarios_to_export

		}

		# For each variable, combines shocks from all stochastic scenarios into a single object of class "xts"
		for (v in base::colnames(stochastic_scenarios_to_export[[1]])){

			# Adds shocks of variable "v"
			for (k in base::seq_along(stochastic_scenarios_to_export)){
				temp <- base::lapply(stochastic_scenarios_to_export,function(x) x[,v])
				temp <- xts::xts(
					x = base::as.data.frame(temp),
					order.by = zoo::index(stochastic_scenarios_to_export[[1]])
				)
				base::colnames(temp) <- base::seq_along(stochastic_scenarios_to_export)
			}

			# Combines the history of shocks with the randomized shocks
			temp <- xts::merge.xts(object$historical_shocks[,v],temp,check.names=FALSE)

			# Stores the table containing the shock scenarios of variable "v"
			shocks_tabs[[v]] <- temp

			# Creates a "dygraph" chart for the shocks of each variable
				shocks_grafs[["dygraphs"]][[v]] <- 
					dygraphs::dygraph(shocks_tabs[[v]], xlab = "Period",main = base::paste(v,"(first difference)")) %>%
					dygraphs::dyHighlight( 
						highlightSeriesBackgroundAlpha = 0.2,
						highlightCircleSize = 0) %>%
					dygraphs::dyLegend(show = "never")

				# Sets limits of vertical axis, if requested by the user
				if (base::length(y_axis) > 0){
					shocks_grafs[["dygraphs"]][[v]] <- 
						shocks_grafs[["dygraphs"]][[v]] |>
						dygraphs::dyAxis("y", valueRange = base::c(y_axis[[v]][1], y_axis[[v]][2]))
				}

				# Adds horizontal line, if requested by the user
				if (base::length(add_horizontal_line) > 0){
					shocks_grafs[["dygraphs"]][[v]] <- 
						shocks_grafs[["dygraphs"]][[v]] |>
						dygraphs::dyLimit(
							limit = add_horizontal_line[[v]],
							label = base::as.character(add_horizontal_line[[v]]),
							strokePattern = "dashed"
						)
				}

			# Creates a "ggplot" chart for the shocks of each variable

				# Converts the object of class "xts" into another, of class "tibble", of "long" format, as required by function ggplot()
				temp <- base::data.frame(  zoo::index(shocks_tabs[[v]])  ,  shocks_tabs[[v]]  )
				base::names(temp) <- base::c("Period",base::colnames(shocks_tabs[[v]]))
				temp <- temp %>%
					tidyr::pivot_longer(cols = base::names(temp)[-1], names_to = "Scenario", values_to = v)

				# Creates the "ggplot" chart
				shocks_grafs[["ggplot"]][[v]] <-
					stats::na.omit(temp) %>%
					ggplot2::ggplot(ggplot2::aes(x = .data$Period, y = !!rlang::sym(v))) +
					ggplot2::geom_line(ggplot2::aes(color = .data$Scenario), linewidth = 1) +
					ggplot2::theme(legend.position = "none") +
					ggplot2::labs(title = base::paste(v,"(first difference)"), y = NULL, x = NULL)

				# Sets limits of vertical axis, if requested by the user
				if (base::length(y_axis) > 0){
					shocks_grafs[["ggplot"]][[v]] <-
						shocks_grafs[["ggplot"]][[v]] +
						ggplot2::coord_cartesian(ylim = base::c(y_axis[[v]][1], y_axis[[v]][2]))
				}

				# Adds horizontal line, if requested by the user
				if (base::length(add_horizontal_line) > 0){
					shocks_grafs[["ggplot"]][[v]] <-
						shocks_grafs[["ggplot"]][[v]] +
						ggplot2::geom_hline(
							yintercept = add_horizontal_line[[v]],
							color = "black",
							linetype = "dashed"
						)
				}
		}

		 list_of_tables[["shocks"]] <- shocks_tabs
		 list_of_charts[["shocks"]] <- shocks_grafs
	}




	base::return(base::list(
		list_of_tables = list_of_tables,
		list_of_charts = list_of_charts
	))

}
