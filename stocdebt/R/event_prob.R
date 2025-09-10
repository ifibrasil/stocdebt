event_prob <- function(
	object,
	id=NULL,
	d,
	threshold=100,
	interval=NULL
){

	# Error handlers
	if (!base::is.null(interval) & !base::is.list(interval)) stop("argument \"interval\" must be a list, with elements \"start\" and \"end\".")

	if (!all(zoo::as.Date(c(interval$start,interval$end)) %in% zoo::index(object$scenarios$stochastic_scenarios[[1]]))) base::stop("Start and end periods in argument \"interval\" must be within the time indexes of simulated scenarios. In other words, periods ",interval$start," and ",interval$end," should be within the vector zoo::index(object$scenarios$stochastic_scenarios[[1]]).")

	# Preparation
	if (base::is.null(interval)){
		interval <- base::list(
			start=object$dates$first_date_to_simulate,
			end=NULL
		)
	} else if (base::is.null(interval$start)) {
		interval$start <- object$dates$first_date_to_simulate
	}

	# Ensures that dates within object 'interval' are of class 'Date'
	interval$start <- zoo::as.Date(interval$start)
	interval$end <- zoo::as.Date(interval$end)

	# Initializes the list which will store results
	p <- base::list()

	# Sets "id" to zero if it is NULL, to avoid failure of logical operators below
	id <- ifelse(test=base::is.null(id),yes=0,no=id)

	# Probability that future debt will be above the threshold, at EACH future period in the interval
	# ______________________________________________________________
	# ______________________________________________________________

	if (id==0 | id==1){

		function_for_lapply <- function(x){
			temp1 <- x[,d][base::paste(
				interval$start,
				"/",
				interval$end,
				sep=""
			)]
			temp2 <- xts::xts(
				x = base::as.numeric(temp1) > threshold,
				order.by = zoo::index(temp1)
			)
			temp2
		}
		temp <- base::lapply(object$scenarios$stochastic_scenarios,FUN=function_for_lapply)
		temp <- base::as.data.frame(temp)
		temp <- base::apply(X=temp,MARGIN=1,FUN=mean)
		temp <- xts::xts(
			x = temp,
			order.by = zoo::as.Date(base::names(temp))
		)
		base::names(temp) <- "Estimated probability"
		temp -> p[["Probability that future debt will be above the threshold, at EACH future period in the interval"]]

	}

	# Probability that future debt will be above the threshold, at EVERY period in the interval
	# ______________________________________________________________
	# ______________________________________________________________

	if (id==0 | id==2){

		function_for_lapply <- function(x){
			temp1 <- x[,d][base::paste(
				interval$start,
				"/",
				interval$end,
				sep=""
			)]
			temp2 <- base::all(base::as.numeric(temp1) > threshold)
			temp2
		}
		temp <- base::lapply(object$scenarios$stochastic_scenarios,FUN=function_for_lapply)
		temp <- base::mean(base::as.integer(temp))
		temp -> p[["Probability that future debt will be above the threshold, at EVERY period in the interval"]]

	}

	# Probability that future debt will be above the threshold, AT LEAST AT ONE period in the interval
	# ______________________________________________________________
	# ______________________________________________________________

	if (id==0 | id==3){

		function_for_lapply <- function(x){
			temp1 <- x[,d][base::paste(
				interval$start,
				"/",
				interval$end,
				sep=""
			)]
			temp2 <- base::all(base::as.numeric(temp1) <= threshold)
			temp2
		}
		temp <- base::lapply(object$scenarios$stochastic_scenarios,FUN=function_for_lapply)
		temp <- 1 - base::mean(base::as.integer(temp))
		temp -> p[["Probability that future debt will be above the threshold, AT LEAST AT ONE period in the interval"]]

	}

	# Probability that future debt will cross the threshold from below for the FIRST TIME, at EACH future period in the interval
	# ______________________________________________________________
	# ______________________________________________________________

	if (id==0 | id==4){

		function_for_lapply <- function(x){
			temp1 <- x[,d][base::paste(
				interval$start,
				"/",
				interval$end,
				sep=""
			)]
			temp2 <- xts::xts(
				x = base::as.numeric(temp1) > threshold,
				order.by = zoo::index(temp1)
			)
			temp3 <- base::grep(pattern=TRUE,x=temp2)[1]
			temp4 <- base::rep(x=FALSE,times=base::length(temp2))

			if (!is.na(temp3)){ temp4[temp3] <- TRUE }

			temp4
		}

		temp <- base::lapply(object$scenarios$stochastic_scenarios,FUN=function_for_lapply)
		temp <- base::as.data.frame(temp)
		temp <- base::apply(X=temp,MARGIN=1,FUN=mean)
		temp <- xts::xts(
			x = temp,
			order.by = base::seq.Date(
				from = interval$start,
				to = interval$end,
				by = xts::periodicity(object$scenarios$stochastic_scenarios[[1]])$label
			)
		)
		base::names(temp) <- "Estimated probability"
		temp -> p[["Probability that future debt will cross the threshold from below for the FIRST TIME, at EACH future period in the interval"]]

	}




	base::return(base::list(
		p = p
	))
}
