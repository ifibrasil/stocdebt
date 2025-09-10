\name{event_prob}
\alias{event_prob}
\title{Probabilities of events}
\description{
Computes probabilities of events related to public debt.
}
\usage{
event_prob(
  object,
  id = NULL,
  d,
  threshold = 100,
  interval = NULL
)
}

\arguments{

\item{object}{An object of class \code{list}. Currently only the output of function \code{\link{sim}} is accepted.}

\item{id}{\code{integer(1)}. The ID of the event whose probability should be computed (see IDs below). If \code{id = NULL} (default), then all events will have their probabilities computed.
	\itemize{
		\item{ID 1: \bold{future debt will be above} \code{threshold}\bold{, at \emph{each} period in the interval}. This probability answers questions like: \dQuote{what is the probability, for \emph{each year} in the interval, that debt will be above 90\% of GDP?} Mathematically, the output would be the \bold{vector}
			\deqn{\big[P(d_{2024} > 0.9),P(d_{2025} > 0.9),P(d_{2026} > 0.9),P(d_{2027} > 0.9),P(d_{2028} > 0.9)\big]}
		in case the interval was [2024-2028], for example.}

		\item{ID 2: \bold{future debt will be above} \code{threshold}\bold{, at \emph{every} period in the interval}. This probability answers questions like: \dQuote{what is the probability that debt will be above 90\% of GDP at \emph{all} periods in the interval, i.e. \emph{never below}?} Mathematically, the output would be the \bold{scalar}
			\deqn{P(d_{2024} > 0.9, d_{2025} > 0.9,d_{2026} > 0.9, d_{2027} > 0.9, d_{2028} > 0.9)}
		in case the interval was [2024-2028], for example.}

		\item{ID 3: \bold{future debt will be above} \code{threshold}\bold{, \emph{at least at one} period in the interval}. This probability answers questions like: \dQuote{what is the probability that debt will be above 90\% of GDP \emph{at least once} in the interval?} Mathematically, the output would be the \bold{scalar}
			\deqn{1 - P(d_{2024} \leq 0.9, d_{2025} \leq 0.9,d_{2026} \leq 0.9, d_{2027} \leq 0.9, d_{2028} \leq 0.9)}
		in case the interval was [2024-2028], for example.}

		\item{ID 4: \bold{future debt will cross the} \code{threshold} \bold{from below for the \emph{first time}, at \emph{each} period in the interval}. This probability answers questions like: \dQuote{what is the probability that debt will \emph{cross} (from below) 90\% of GDP, for the first time, in 2025?} Mathematically, the output would be the \bold{vector}
			\deqn{\big[p_1, p_2, p_3, p_4, p_5\big]}
		where
			\deqn{p_1 = P(d_{2024} > 0.9)}
			\deqn{p_2 = P(d_{2024} \leq 0.9, d_{2025} > 0.9)}
			\deqn{p_3 = P(d_{2024} \leq 0.9, d_{2025} \leq 0.9, d_{2026} > 0.9)}
			\deqn{p_4 = P(d_{2024} \leq 0.9, d_{2025} \leq 0.9, d_{2026} \leq 0.9, d_{2027} > 0.9)}
			\deqn{p_5 = P(d_{2024} \leq 0.9, d_{2025} \leq 0.9, d_{2026} \leq 0.9, d_{2027} \leq 0.9, d_{2028} > 0.9)}
		in case the interval was [2024-2028], for example.}
	}
}

\item{d}{\code{character(1)}. Name of debt variable as inputted to argument \code{realized_data} in function \code{\link{sim}}.}

\item{threshold}{\code{numeric(1)}. Debt as percentage of GDP. For example, for debt at 80\% of GDP, inform it as 80. Default is 100.}

\item{interval}{\code{list}. A list with at least one of two elements:
\itemize{
\item{\code{start}: \code{character(1)} or \code{Date(1)} in format \dQuote{YYYY-MM-DD}. \bold{First} date of the interval of interest.}
\item{\code{end}: \code{character(1)} or \code{Date(1)} in format \dQuote{YYYY-MM-DD}. \bold{Last} date of the interval of interest.}
}
If no interval is provided, the default for \code{start} is the first simulated period in argument \code{object}, and the default for \code{end} is the last simulated period.
}
}

\value{
A \code{list} named \code{p} (for \dQuote{probability}) with the estimated probability of each event requested by the user.
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

## Step 3) compute probabilities of events,
## focusing on the subinterval 2024-2026
e <- event_prob(
  object = s,
  d = "debt",
  threshold = 90,
  interval = list(
    start = "2024-12-01",
    end = "2026-12-01"
  )
)
}
