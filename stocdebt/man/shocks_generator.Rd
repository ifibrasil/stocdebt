\name{shocks_generator}
\alias{shocks_generator}
\title{Shocks generator}
\description{
Generates random shocks according to specific methodologies.
}
\usage{
shocks_generator(
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
)
}
\arguments{
\item{case}{\code{character(1)}. Method used to generate shocks. Currently, only methods \code{"normal"} (default) and \code{"ec"} are implemented. See section \dQuote{Details} for further information.}

\item{parameters}{\code{list(1)}. A list of additional parameters to be informed when \code{case = "normal"}. Currently, only standard deviations for the normal shocks have to be informed. Thus, \code{parameters} should be a list containing an element named \dQuote{sd}, which in turn is a list as well. For clarity, see the examples. The standard deviation for each variable must be named as in arguments \code{i}, \code{ti}, \code{tic} (when provided), \code{g} and \code{p}.}

\item{n_stochastic_scenarios}{\code{integer(1)}. Number of stochastic scenarios (time series) of shocks to generate for each debt determinant. For example, if equal to 10, there will be 10 series of shocks for nominal interest rates, 10 series of shocks for real GDP growth, and so on.}

\item{realized_data}{\code{xts}. Realized data (past observations). Default is NULL, but must be informed when \code{case = "ec"}. Variable units in the realized time series must be as follows:
\itemize{
\item{nominal interest rate (argument \code{i}): percent per annum (p.a.). For example, if the rate is 7\% p.a., inform it as 7.}
\item{inflation rate (\code{ti}): percent change of the \bold{GDP deflator (not of a consumer price index)}. For example, if the change is 5\%, inform it as 5.}
\item{inflation rate (\code{tic}): percent change of a \bold{consumer price index (not of the GDP deflator))}. For example, if the change is 4\%, inform it as 4. This variable is not necessary to run simulations successfully, and thus can be omitted. Its purpose is to enable the user to later generate a fan chart for the consumer price inflation (by means of function \code{\link{sim}}), but has absolutely no effect on paths and fan charts of other variables, including debt (except perhaps for numeric rounding differences).}
\item{real GDP growth (\code{g}): percent change. For example, if the change is 3\%, inform it as 3.}
\item{primary balance (\code{p}): percent of GDP. For example, if the balance is 1\% of GDP, inform it as 1.}
}
}

\item{subintervals}{\code{character(n)}. A character vector with subintervals of interest, written in the syntax of package \code{xts}, and in the format \dQuote{YYYY-MM-DD}. The function allows more than one subinterval, which can be nonadjacent. Examples with a \bold{single} subinterval:
\itemize{
\item{\code{"2011-03-01/"}: selects all observations from March 1, 2011, onwards.}
\item{\code{"2011-03-01/2014-12-01"}: selects all observations between March 1, 2011, and December 1, 2014.}
\item{\code{"/2014-12-01"}: selects all observations up to December 1, 2014.}
}
Examples with \bold{two nonadjacent} subintervals:
\itemize{
\item{\code{c("2011-03-01/2014-12-01","2017-03-01/2019-03-01")}: selects all observations between March 1, 2011, and December 1, 2014, and all observations between March 1, 2017, and March 1, 2019.}
\item{\code{c("/2014-12-01","2017-03-01/")}: selects all observations up to December 1, 2014, and all observations from March 1, 2017, onwards.}
}
}


\item{shocks_first_date}{\code{character(1)} or \code{Date(1)} in format \dQuote{YYYY-MM-DD}. First date for which random variates should be drawn. See the example.}
\item{shocks_last_date}{\code{character(1)} or \code{Date(1)} in format \dQuote{YYYY-MM-DD}. Last date for which random variates should be drawn. See the example.}
\item{shocks_frequency}{\code{character(1)}. Frequency of the time series of shocks to be generated. Set "year" for yearly, "quarter" for quarterly and "month" for monthly frequencies. See the example.}
\item{correlated_shocks}{\code{logical(1)}. When \code{case = "ec"}, should the shocks be correlated? Default is TRUE. This argument is useful for robustness checks, and is innocuous when \code{case} is not \code{ec}.}

\item{i}{\code{character(1)}. Name of nominal interest variable in argument \code{realized_data}.}
\item{ti}{\code{character(1)}. Name of inflation rate variable (inflation of the \bold{GDP deflator}) in argument \code{realized_data}.}
\item{tic}{\code{character(1)}. Name of inflation rate variable (inflation of a \bold{consumer price index}) in argument \code{realized_data}. This variable is not necessary to run simulations successfully, and thus can be omitted. Its purpose is to enable the user to later generate a fan chart for the consumer price inflation (by means of function \code{\link{sim}}), but has absolutely no effect on paths and fan charts of other variables, including debt (except perhaps for numeric rounding differences).}
\item{g}{\code{character(1)}. Name of real GDP growth variable in argument \code{realized_data}.}
\item{p}{\code{character(1)}. Name of primary balance variable in argument \code{realized_data}.}

\item{shocks_seed}{\code{integer(1)}. Seed for randomization. Default is 1.}
}
\details{

Case \code{"normal"} draws random observations from normal probability distributions, with mean zero and standard deviations set by the user, for the debt determinants (nominal interest, inflation rate, real GDP growth and primary balance). The normal distributions associated to the determinants are independent from each other.

Case \code{"ec"} implements the randomization as in European Commission (2013), which amounts to drawing random observations from a multivariate normal probability distribution. This distribution has mean zero and variance-covariance matrix equal to the observed variance-covariance of the \dQuote{historical shocks} (first differences of debt determinants). When \code{case = "ec"}, argument \code{subintervals} causes the shocks to be computed in two steps:
\enumerate{
\item compute first differences of \code{realized_data}.
\item select periods informed in \code{subintervals}.
}
Thus, when \code{case = "ec"}, dates in argument \code{subintervals} are those of the \bold{first differences} of debt determinants, not of their \bold{levels}. So, for example, if you are dealing with quarterly data and have chosen two nonadjacent \code{subintervals = c("2011-03-01/2014-12-01","2017-03-01/2019-03-01")}, then shocks will include the difference between values at period \code{"2017-03-01"} and one period behind, \code{"2016-12-01"}, even though the latter is not explicitly in \code{subintervals}.

Argument \code{shocks_seed} is set to 1 by default. It makes the randomization lead to the same results across consecutive calls of function \code{shocks_generator}, thus enabling the user to reproduce the analysis. To obtain different results, the user must change the value of \code{shocks_seed} across calls.

If argument \code{tic} is not NULL, the function will also generate time series of shocks for consumer inflation. These shocks will later enable the user to draw fan charts of
\itemize{
	\item{consumer inflation.}
	\item{real interest rates with respect to consumer inflation.}
	\item{debt, by means of a version of the debt dynamics equation which is useful for consistency checks (see the documentation of \code{\link{sim}} for further details).}
}
}
\value{

A \code{list} containing objects:
\itemize{
\item{\code{shocks}: a \code{list} with \code{n_stochastic_scenarios} objects of class \code{xts}. Each \code{xts} contains time series of shocks for all determinants of public debt: nominal interest rate, inflation rate, real GDP growth and primary balance.}
\item{\code{historical_shocks}: an output which depends on the value of argument \code{case} set by the user. If \code{case = "normal"}, then \code{historical_shocks = NULL}. If \code{case = "ec"}, then \code{historical_shocks} is an \code{xts} object with the historical shocks (first differences, in times series terminology) calculated from the data in argument \code{realized_data}.}
\item{\code{var_cov_historical_shocks}: a matrix returned if \code{case = "ec"}. It is the observed variance-covariance matrix of the first differences of the debt determinants (nominal interest rate, inflation rate, real GDP growth and primary balance). If \code{case} is not \dQuote{ec}, then \code{var_cov_historical_shocks = NULL}.}
\item{\code{correlation_tests}: a \code{list} with p-values of Pearson, Spearman and Kendall significance tests for correlations historical shocks.}
}

}
\references{
EUROPEAN COMMISSION, Directorate-General for Economic and Financial Affairs, Berti, K. (2013). Stochastic public debt projections using the historical variance-covariance matrix approach for EU countries. Economic papers, n. 480, p. 1-25, 2013, \url{https://data.europa.eu/doi/10.2765/4211}.
}
\examples{
## generate 100 scenarios of yearly random shocks,
## drawn from independent normal probability distributions
shocks <- shocks_generator(
  case = "normal",
  parameters = list(sd = list(
    "nominal interest rate" = 0.1,
    "gdp deflator" = 0.1,
    "consumer price index" = 0.1,
    "real gdp growth" = 0.1,
    "primary balance" = 0.1
  )),
  n_stochastic_scenarios = 100,
  shocks_first_date = "2024-12-01",
  shocks_last_date = "2028-12-01",
  shocks_frequency = "year",
  i = "nominal interest rate",
  ti = "gdp deflator",
  tic = "consumer price index",
  g = "real gdp growth",
  p = "primary balance"
)

## generate 100 scenarios of yearly random shocks,
## according to the European Commission method
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
  tic = "consumer price index",
  g = "real gdp growth",
  p = "primary balance"
)
}
