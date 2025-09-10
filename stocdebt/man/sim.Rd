\name{sim}
\alias{sim}
\title{Simulation of stochastic scenarios}
\description{
Simulates stochastic scenarios for public debt and its determinants. This function has been designed to deal with data in quarterly and yearly frequencies. 
}
\usage{
sim(
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
)
}
\arguments{

\item{first_date_to_simulate}{\code{character(1)} or \code{Date(1)} in format \dQuote{YYYY-MM-DD}. First date for which stochastic scenarios must be simulated.}

\item{last_date_to_simulate}{\code{character(1)} or \code{Date(1)} in format \dQuote{YYYY-MM-DD}. Last date for which stochastic scenarios must be simulated.}

\item{realized_data}{\code{xts}. Realized data (past observations). Variable units must be as follows:
\itemize{
\item{debt (\code{d}): percent of GDP. For example, if debt is 60\% of GDP, inform it as 60.}
\item{nominal interest rate (\code{i}): percent per annum (p.a.). For example, if the rate is 7\% p.a., inform it as 7.}
\item{inflation rate (\code{ti}): percent change of the \bold{GDP deflator} (it should not be the change of a consumer price index or any other price index). For example, if the change is 5\%, inform it as 5.}
\item{inflation rate (\code{tic}): percent change of a \bold{consumer price index}. For example, if the change is 4\%, inform it as 4. This argument is not necessary to run simulations, and thus can be omitted. Its purpose is to enable the user to later generate a fan chart for consumer price inflation, but has absolutely no effect on paths and fan charts of other variables, including debt (except perhaps for numeric rounding differences).}
\item{real interest rate (\code{r}): percent per annum (p.a.). For example, if the rate is 2\% p.a., inform it as 2. This must be the real interest rate computed with respect to the \bold{GDP deflator}, not with respect to any other price index.}
\item{real interest rate (\code{r_tic}): percent per annum (p.a.). For example, if the rate is 8\% p.a., inform it as 8. This must be the real interest rate computed with respect to a \bold{consumer price index} (\code{tic}). This argument is not necessary to run simulations, and thus can be omitted. Its purpose is to enable the user to later generate a fan chart for the real interest rate (computed with respect to the consumer price inflation \code{tic}), but has absolutely no effect on paths and fan charts of other variables, including debt.}
\item{real GDP growth (\code{g}): percent change. For example, if the change is 3\%, inform it as 3.}
\item{primary balance (\code{p}): percent of GDP. For example, if the balance is 1\% of GDP, inform it as 1.}
}
}

\item{baseline_scenario}{\code{xts}. Future values (i.e. not yet realized) for the variables, in the hypothetical baseline scenario. Note that variable \bold{units} must be the same used in argument \code{realized_data}.
\itemize{
\item{if \code{baseline_scenario = NULL} (default), the stochastic scenarios will be generated up to the last date in argument \code{shocks}.}
\item{if \code{baseline_scenario} is an \code{xts} object, the stochastic scenarios will be generated up to the last date in such \code{xts}.}
}
}

\item{compute_deterministic_d_and_r}{\code{logical(1)}. If \code{FALSE} (default), the function will take baseline values of real interest rates (\code{r}) and debt (\code{d}) from argument \code{baseline_scenario}. If \code{TRUE}, the function will overwrite those values, existent in \code{baseline_scenario}, with new values, computed as follows:
	\itemize{
		\item{for real interest rates, using the baseline paths of nominal interest rates (\code{i}) and inflation (\code{ti} or \code{tic}, if the latter is not NULL), informed in \code{baseline_scenario}.}
		\item{for debt, using (1) the debt dynamics equation, (2) the real interest rate computed in the previous item, and (3) the remaining debt determinants informed in \code{baseline_scenario}.}
	}
In case \code{baseline_scenario} is originally missing data specifically for these two variables (real interest rate and debt), setting \code{compute_deterministic_d_and_r = TRUE} ensures that such time series will be created by the function.

In case \code{baseline_scenario = NULL} (i.e. completely missing), the value set for \code{compute_deterministic_d_and_r}, be it \code{TRUE} or \code{FALSE}, is innocuous. The reason is that \code{compute_deterministic_d_and_r} operates on \code{baseline_scenario} and thus, if the latter is missing, there is no object to operate on.
}

\item{baseline_as_median}{\code{logical(1)}. If \code{TRUE} (default), shifts all stochastic scenarios to ensure that the baseline scenario is the median, at all simulated time periods, for all variables (debt determinants as well as debt itself). If \code{FALSE}, does not shift the stochastic scenarios.}

\item{n_stochastic_scenarios}{\code{integer(1)}. Number of stochastic scenarios to simulate per variable (debt determinants and debt itself.}

\item{shocks}{\code{list(n)}. List of shocks on the \bold{determinants} of public debt. Its length must be equal to \code{n_stochastic_scenarios}. Each element of the list must be an object of class \code{xts}, containing one time series of shocks for each debt determinant: \code{i}, \code{ti}, \code{g}, \code{p} and \code{tic} (this one optionally). The list can be generated by function \code{\link{shocks_generator}}.}

\item{persistent_shocks}{\code{character(n)}. If \code{persistent_shocks = NULL} (default), shocks are temporary, which means they last for a single period. If \code{persistent_shocks} is a vector of names (of debt determinants), shocks are persistent, i.e. accumulated over time, for the indicated determinants. The names must be within those in \code{shocks}. Finally, if \code{persistent_shocks = "all"}, shocks are persistent for all debt determinants.}

\item{persistent_shocks_duration}{\code{integer(1)}. When \code{persistent_shocks = NULL}, this argument is innocuous. When \code{persistent_shocks} is not NULL:
\itemize{
\item{if \code{persistent_shocks_duration = NULL} (default), shocks on all debt determinants are accumulated over time, indefinitely. That is, shocks are permanent (the effect of a shock lasts forever, as in a \dQuote{random walk} without drift).}
\item{if \code{persistent_shocks_duration} is an integer (for example, 2), then the effect of a shock lasts such number of periods (in the example, 2 periods).

Note that if \code{accumulate_quarterly_shocks = TRUE}, periods should be counted in years. Thus, in the example, shocks would last for 2 years, not 2 quarters.}
}
}

\item{accumulate_quarterly_shocks}{\code{logical(1)}. Should quarterly shocks be accumulated within each year, so as to render yearly shocks (as in European Commission, 2013)?}

\item{d}{\code{character(1)} or \code{list(2)}. Name of \bold{debt} variable. If the name is \emph{identical} across arguments \code{realized_data} and \code{baseline_scenario} (in case \code{base_scenario} is not NULL), then it suffices to provide the name as a single \code{character}. If the names are \emph{not identical}, provide them as a list with elements named \dQuote{realized} and \dQuote{baseline} (in case \code{base_scenario} is not NULL). See the example.}

\item{i}{\code{character(1)} or \code{list(3)}. Name of \bold{nominal interest rate} variable. If the name is \emph{identical} across arguments \code{realized_data}, \code{baseline_scenario} (in case \code{base_scenario} is not NULL) and \code{shocks}, then it suffices to provide the name as a single \code{character}. If the names are \emph{not identical}, provide them as a list with elements named \dQuote{realized}, \dQuote{baseline} (in case \code{base_scenario} is not NULL) and \dQuote{shocks}. See the example.}

\item{ti}{\code{character(1)} or \code{list(3)}. Name of \bold{inflation rate} variable (inflation of the \bold{GDP deflator}). If the name is \emph{identical} across arguments \code{realized_data}, \code{baseline_scenario} (in case \code{base_scenario} is not NULL) and \code{shocks}, then it suffices to provide the name as a single \code{character}. If the names are \emph{not identical}, provide them as a list with elements named \dQuote{realized}, \dQuote{baseline} (in case \code{base_scenario} is not NULL) and \dQuote{shocks}. See the example.}

\item{tic}{\code{character(1)} or \code{list(3)}. Name of \bold{inflation rate} variable (inflation of the \bold{consumer price index}). Defaults to NULL and, if informed, argument \code{r_tic} must also be informed. If the name is \emph{identical} across arguments \code{realized_data}, \code{baseline_scenario} (in case \code{base_scenario} is not NULL) and \code{shocks}, then it suffices to provide the name as a single \code{character}. If the names are \emph{not identical}, provide them as a list with elements named \dQuote{realized}, \dQuote{baseline} (in case \code{base_scenario} is not NULL) and \dQuote{shocks}. See the example.}

\item{r}{\code{character(1)} or \code{list(2)}. Name of \bold{real interest rate} variable (computed with respect to the \bold{GDP deflator}). If the name is \emph{identical} across arguments \code{realized_data} and \code{baseline_scenario} (in case \code{base_scenario} is not NULL), then it suffices to provide the name as a single \code{character}. If the names are \emph{not identical}, provide them as a list with elements named \dQuote{realized} and \dQuote{baseline} (in case \code{base_scenario} is not NULL). See the example.}

\item{r_tic}{\code{character(1)} or \code{list(2)}. Name of \bold{real interest rate} variable (computed with respect to a \bold{consumer price index}). Defaults to NULL and, if informed, argument \code{tic} must also be informed. If the name is \emph{identical} across arguments \code{realized_data} and \code{baseline_scenario} (in case \code{base_scenario} is not NULL), then it suffices to provide the name as a single \code{character}. If the names are \emph{not identical}, provide them as a list with elements named \dQuote{realized} and \dQuote{baseline} (in case \code{base_scenario} is not NULL). See the example.}

\item{g}{\code{character(1)} or \code{list(3)}. Name of \bold{real GDP growth} variable. If the name is \emph{identical} across arguments \code{realized_data}, \code{baseline_scenario} (in case \code{base_scenario} is not NULL) and \code{shocks}, then it suffices to provide the name as a single \code{character}. If the names are \emph{not identical}, provide them as a list with elements named \dQuote{realized}, \dQuote{baseline} (in case \code{base_scenario} is not NULL) and \dQuote{shocks}. See the example.}

\item{p}{\code{character(1)} or \code{list(3)}. Name of \bold{primary balance} variable. If the name is \emph{identical} across arguments \code{realized_data}, \code{baseline_scenario} (in case \code{base_scenario} is not NULL) and \code{shocks}, then it suffices to provide the name as a single \code{character}. If the names are \emph{not identical}, provide them as a list with elements named \dQuote{realized}, \dQuote{baseline} (in case \code{base_scenario} is not NULL) and \dQuote{shocks}. See the example.}

\item{no_shocks}{\code{character(n)}. Debt determinants whose shocks (contained in argument \code{shocks}) should be suppressed, that is, set to zero. Acceptable names are those informed in \code{i}, \code{ti}, \code{g} and \code{p}.}

}

\details{

\bold{Argument} \code{baseline_as_median} \bold{allows the user to move the fan charts} of all variables, so that the fans  median coincide with the baseline scenarios of each variable. This is the practice of the Office for Budget Responsibility, the United Kingdom independent fiscal institution (Steel, 2021).

\bold{Difference is allowed between frequencies} of past data (argument \code{realized_data}) and future data (arguments \code{baseline_scenario}, \code{shocks}, \code{first_date_to_simulate} and \code{last_date_to_simulate}). In other words, past data can be of yearly frequency, while future data is of quarterly frequency.

\bold{The formula for the real interest rate} is the usual one:
\deqn{r_t = \frac{(1+i_t)}{(1+\pi_t)}}
where \eqn{r_t} is real interest rate at period \eqn{t}; \eqn{i_t} is nominal interest rate at period \eqn{t}; and \eqn{\pi_t} is percent change in GDP deflator at period \eqn{t}.

\bold{The formula for debt} is the usual debt dynamics equation:
\deqn{d_t = \frac{(1+r_t)}{(1+g_t)}d_{t-1} - pb_t}
where \eqn{d_t} is debt in percent of GDP at period \eqn{t}; \eqn{g_t} is real GDP growth at period \eqn{t}; and \eqn{pb_t} is primary balance, in percent of GDP, at period \eqn{t}.

\bold{If arguments} \code{tic} \bold{and} \code{r_tic} \bold{are not NULL}, the \code{sim} function will
\itemize{
	\item{compute, besides \eqn{r_t}, the real interest rate with respect to the consumer price index (CPI), denoted by \eqn{r_t^{CPI}}. This rate is computed as:
		\deqn{r_t^{CPI} = \frac{(1+i_t)}{(1+\pi_t^{CPI})}}
	where \eqn{\pi_t^{CPI}} is the percent change in the CPI at period \eqn{t}.
	}

	\item{use, in place of the debt dynamics equation above, the modified (but algebraically equivalent) version:
		\deqn{d_t = \frac{(1+\pi_t^{CPI})}{(1+\pi_t)}\frac{(1+r_t^{CPI})}{(1+g_t)}d_{t-1} - pb_t}
	The purpose of this form of the equation is to afford consistency checks. That is, it is useful for validation of the computations carried out with the first form above. However, for everyday generation of fan charts, the choice between the equations is innocuous for the user.
	}
}
A practical advantage of using arguments \code{tic} and \code{r_tic} is the possibility to generate fan charts of consumer inflation and real interest rates with respect to the consumer inflation.

\bold{The baseline scenario plays a key role in the simulation}, because shocks are applied on it. However, it is not mandatory. In the absence of it, i.e. \code{baseline_scenario = NULL}:
	\itemize{
		\item{stochastic scenarios for debt \bold{determinants} (except real interest rates) will be generated simply by successive accumulation of shocks over time. In other words, shocks are applied on their own immediately past value, as in a \dQuote{random walk} without drift.}
		\item{stochastic scenarios for \bold{debt} and \bold{real interest rates} will be generated as usual, by means of the two equations shown above.}
	}

}
\value{
A \code{list} named \code{scenarios}, such that
\itemize{
	\item{if \code{baseline_scenario = NULL}, the list will contain a single object: \code{stochastic_scenarios} (class \code{list}).}

	\item{if \code{baseline_scenario} is not NULL, the list will contain two objects:
		\itemize{
			\item{an extended \code{baseline_scenario} object (class \code{xts}): combines \code{realized_data} (past) with \code{baseline_scenario} (future).}

			\item{\code{stochastic_scenarios} (class \code{list}): a set of \code{n_stochastic_scenarios} objects of class \code{xts}. Each of them combines realized data (past) with one simulated stochastic scenario (future), for each variable.}
			}
	}
}
}
\references{
EUROPEAN COMMISSION, Directorate-General for Economic and Financial Affairs, Berti, K. (2013). Stochastic public debt projections using the historical variance-covariance matrix approach for EU countries. Economic papers, n. 480, p. 1-25, 2013, \url{https://data.europa.eu/doi/10.2765/4211}.

DI GIOVANNI, J. and GARDNER, E. H. A Simple Stochastic Approach to Debt Sustainability Applied to Lebanon. IMF Working Paper No. 2008/097, April 1, 2008. Available at: \url{https://www.imf.org/en/Publications/WP/Issues/2016/12/31/A-Simple-Stochastic-Approach-to-Debt-Sustainability-Applied-to-Lebanon-21891}.

STEEL, Daniel. Evaluating forecast uncertainty with stochastic simulations. OBR Working Paper No. 17. December 2021. Available at: \url{https://obr.uk/docs/dlm_uploads/working_paper_no17_uncertainty.pdf}.
}
\examples{

## EXAMPLE A: WITHOUT consumer price index (i.e. without arguments "tic" and "r_tic")
###################################################################################

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

## Step 2) apply shocks from step 1 so as to simulate stochastic scenarios
## for debt determinants and debt itself
s <- sim(
  first_date_to_simulate = "2024-12-01",
  last_date_to_simulate = "2028-12-01",
  realized_data = sample_series$realized_yearly_data,
  baseline_scenario = sample_series$baseline_yearly_scenario,
  baseline_as_median = TRUE,
  n_stochastic_scenarios = 100,
  shocks = shocks$shocks,
  d = list(
    realized="debt",
    baseline="debt"
  ),
  i = list(
    realized="nominal interest rate",
    baseline="nominal interest rate",
    shocks="nominal interest rate"
  ),
  ti = list(
    realized="gdp deflator",
    baseline="gdp deflator",
    shocks="gdp deflator"
  ),
  r = list(
    realized="real interest rate (w.r.t. gdp deflator)",
    baseline="real interest rate (w.r.t. gdp deflator)"
  ),
  g = list(
    realized="real gdp growth",
    baseline="real gdp growth",
    shocks="real gdp growth"
  ),
  p = list(
    realized="primary balance",
    baseline="primary balance",
    shocks="primary balance"
  )
)

## NOTE (equivalence): since variable names are identical across "realized", "baseline" and
## "shocks", we can use the following EQUIVALENT command
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







## EXAMPLE B: WITH consumer price index (i.e. with arguments "tic" and "r_tic")
#############################################################################

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
  tic = "consumer price index", # <-- consumer price index here
  g = "real gdp growth",
  p = "primary balance"
)

## Step 2) apply shocks from step 1 so as to simulate stochastic scenarios
## for debt determinants and debt itself
s <- sim(
  first_date_to_simulate = "2024-12-01",
  last_date_to_simulate = "2028-12-01",
  realized_data = sample_series$realized_yearly_data,
  baseline_scenario = sample_series$baseline_yearly_scenario,
  baseline_as_median = TRUE,
  n_stochastic_scenarios = 100,
  shocks = shocks$shocks,
  d = list(
    realized="debt",
    baseline="debt"
  ),
  i = list(
    realized="nominal interest rate",
    baseline="nominal interest rate",
    shocks="nominal interest rate"
  ),
  ti = list(
    realized="gdp deflator",
    baseline="gdp deflator",
    shocks="gdp deflator"
  ),
  tic = list(
    realized="consumer price index", # <-- consumer price index here
    baseline="consumer price index", # <-- consumer price index here
    shocks="consumer price index"    # <-- consumer price index here
  ),
  r = list(
    realized="real interest rate (w.r.t. gdp deflator)",
    baseline="real interest rate (w.r.t. gdp deflator)"
  ),
  r_tic = list(
    realized="real interest rate (w.r.t. consumer price index)", # <-- real interest rate w.r.t. consumer price index here
    baseline="real interest rate (w.r.t. consumer price index)"  # <-- real interest rate w.r.t. consumer price index here
  ),
  g = list(
    realized="real gdp growth",
    baseline="real gdp growth",
    shocks="real gdp growth"
  ),
  p = list(
    realized="primary balance",
    baseline="primary balance",
    shocks="primary balance"
  )
)
}
