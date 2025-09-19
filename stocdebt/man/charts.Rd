\name{charts}
\alias{charts}
\title{Charts of simulated objects}
\description{
Creates charts of simulated objects, using packages \code{dygraphs} and \code{ggplot2}.
}
\usage{
charts(
  object,
  type = "simulation",
  fan_chart_percentiles = NULL,
  seed_if_limited_export = 1,
  fan_color = "#005d89",
  max_to_plot = 100,
  y_axis = list(),
  add_horizontal_line = list()
)
}
\arguments{
\item{object}{An object of class \code{list}. It can be the output of function \code{\link{sim}} or of function \code{\link{shocks_generator}}. In the first case, set \code{type="simulation"}. In the second case, set \code{type="shocks"}.}

\item{type}{\code{character(1)}. Set \code{"simulation"} (default) for charts of stochastic scenarios (outputted by \code{\link{sim}}. Set \code{"shocks"} for charts of generated shocks (outputted by \code{\link{shocks_generator}}).}

\item{fan_chart_percentiles}{\code{numeric(n)}. Numeric vector with one or more percentiles, in decimal form. For example, for the 10\% and 90\% percentiles, write \code{c(0.1,0.9)}. Useful only if \code{type = "simulation"}.}

\item{seed_if_limited_export}{\code{integer(1)}. In case there are too many time series (of shocks or stochastic scenarios) to plot, a sample of them will be drawn based on this seed. Default seed is 1.}

\item{fan_color}{\code{character(1)}. RGB code of the color of the fan chart. Default is dark blue (\code{"#005d89"}).}

\item{max_to_plot}{\code{integer(1)}. Maximum number of stochastic scenarios to plot in \emph{paths} charts (\emph{fan} charts are not affected). The more scenarios plotted, the longer it will take to load the charts. Default is 100.}

\item{y_axis}{\code{list(n)}. List with maxima and minima for the y-axis in the charts. For example, \code{list("debt" = c(0,100), "real gdp growth" = c(-5,5))} will set the range of the y-axis to be [0,100] in the chart for debt, and [-5,5] in the chart for real GDP growth. Variable names must be those used when generating argument \code{object} by means of functions \code{\link{sim}} (if \code{type="simulation"}) or \code{\link{shocks_generator}} (if \code{type ="shocks"}).}

\item{add_horizontal_line}{\code{list(n)}. List with the y-axis value of a horizontal line. For example, \code{list("primary balance" = 0, "real gdp growth" = 1)}. Variable names must be those used when generating argument \code{object} by means of functions \code{\link{sim}} (if \code{type="simulation"}) or \code{\link{shocks_generator}} (if \code{type ="shocks"}). Default is not to add a line.}
}
\details{

The function uses package
\itemize{
\item{\code{dygraphs} to create \bold{interactive} JavaScript charts, to be loaded in web browsers, for example.}
\item{\code{ggplot2} to create \bold{non-interactive} charts that can be added to PDF documents, for example.}
}

Charts of the \code{dygraphs} family have been implemented only for paths (not for fan charts).

Charts of shocks (\code{type = "shocks"}) are useful for diagnostic purposes.

Specifically when \code{type = "simulation"}, if there are too many stochastic scenarios to plot in \emph{paths} charts, only a subset of them (\code{max_to_plot}) are indeed plotted. This limitation has been set to prevent R from taking too long to generate the chart. However, \emph{fan} charts are not affected by \code{max_to_plot}, because their percentiles are computed from all stochastic scenarios, without exception. The total number of stochastic scenarios used used to compute the percentiles is that informed in argument \code{n_stochastic_scenarios} of function \code{\link{sim}}.

}
\value{

A \code{list} containing two sublists: a list of \bold{tables} (\code{list_of_tables}) and a list of \bold{charts} (\code{list_of_charts}). Their contents depend on argument \code{type}, as explained below, and form a sequence of embedded sublists.



\enumerate{

	\item{

		If \code{type = "shocks"}:
		\itemize{
			\item{\code{list_of_tables}:
				\itemize{
					\item{sublist \code{shocks}: first differences of realized time series, merged with their shocks.}
				}
			}
			\item{\code{list_of_charts}:
				\itemize{
					\item{\code{shocks}:}
					\itemize{
					    \item{\code{dygraphs}: charts drawn using the \code{\link[dygraphs]{dygraph}} function.}
					    \item{\code{ggplot}: charts drawn using the \code{\link[ggplot2]{ggplot}} function.}
					}
				}
			}
			\item{To retrieve values of \bold{specific variables} (real GDP growth, primary balance, etc.) within these tables and charts, use the names attributed to them in argument \code{realized_data}, in functions \code{\link{shocks_generator}}.}
		}
	}

	\item{
	
		If \code{type = "simulation"}:
		\itemize{
			\item{\code{list_of_tables}:
				\itemize{
					\item{\code{paths}: realized time series, stacked on (at most \code{max_to_plot}) simulated paths.}
					\item{\code{fancharts (paths)}: realized time series, stacked on the quantile paths.}
					\item{\code{fancharts (ribbons)}: realized and baseline time series, merged with the upper and lower limits of all ribbons making up the fanchart.}
				}
			}
			\item{\code{list_of_charts}:
				\itemize{
					\item{\code{paths}: charts of (at most \code{max_to_plot}) simulated paths.}
					\itemize{
					    \item{\code{dygraphs}: charts drawn using the \code{\link[dygraphs]{dygraph}} function.}
					    \item{\code{ggplot}: charts drawn using the \code{\link[ggplot2]{ggplot}} function.}
					}
					\item{\code{fancharts}: fancharts for each variable.}
					\itemize{
					    \item{\code{dygraphs}: charts drawn using the \code{\link[dygraphs]{dygraph}} function.}
					    \item{\code{ggplot}: charts drawn using the \code{\link[ggplot2]{ggplot}} function.}
					}
				}
			}
			\item{To retrieve values of \bold{specific variables} (real GDP growth, primary balance, etc.) within these tables and charts, use the names attributed to them in argument \code{realized_data}, in functions \code{\link{sim}}.}
		}
	}

}
}

\examples{
## Step 1) generate yearly shocks using function "shocks_generator"
shocks <- shocks_generator(
  case = "ec",
  n_stochastic_scenarios = 100,
  realized_data = sample_series$realized_yearly_data,
  subintervals = "2009-12-01/2019-12-01",
  shocks_first_date = "2024-12-01",
  shocks_last_date = "2028-12-01",
  shocks_frequency = "year",
  i = "nominal interest rate",
  ti = "gdp deflator",
  g = "real gdp growth",
  p = "primary balance"
)

## Step 2) simulate the stochastic scenarios
s <- sim(
  first_date_to_simulate = "2024-12-01",
  last_date_to_simulate = "2028-12-01",
  realized_data = sample_series$realized_yearly_data,
  baseline_scenario = sample_series$baseline_yearly_scenario,
  baseline_as_median = TRUE,
  n_stochastic_scenarios = 100,
  shocks = shocks$shocks,
  d = "debt",
  i = "nominal interest rate",
  ti = "gdp deflator",
  r = "real interest rate (w.r.t. gdp deflator)",
  g = "real gdp growth",
  p = "primary balance"
)

## Step 3) create charts of shocks
x <- charts(
  object = shocks,
  type = "shocks"
)

## Step 3.1) visualize charts of shocks
\dontrun{
x$list_of_charts$shocks$ggplot[["real gdp growth"]]
x$list_of_charts$shocks$ggplot[["primary balance"]]
}

## Step 4) create charts of stochastic scenarios
x <- charts(
  object = s,
  type = "simulation",
  fan_chart_percentiles = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
)

\dontrun{
## Step 4.1) visualize some PATHS of stochastic scenarios
x$list_of_charts$paths$ggplot[["debt"]]
x$list_of_charts$paths$ggplot[["primary balance"]]

## Step 4.2) visualize FAN CHARTS of stochastic scenarios
x$list_of_charts$fancharts$ggplot[["debt"]]
x$list_of_charts$fancharts$ggplot[["primary balance"]]
}
}
