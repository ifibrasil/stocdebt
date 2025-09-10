\name{dde}
\alias{dde}
\title{Debt Dynamics Equation}
\description{
Computes debt paths using debt dynamics equations.
}
\usage{
dde(
  realized_data,
  stock_flow_adjustments = NULL,
  d,
  i,
  ti,
  tic = NULL,
  r,
  r_tic = NULL,
  g,
  p
)
}

\arguments{

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

\item{stock_flow_adjustments}{\code{xts}. Realized data (past observations) of stock-flow adjustments. This will simply cause the function to merge the time series in \code{stock_flow_adjustments} with the output. No calculations are done using this series.}

\item{d}{\code{character(1)}. Name of \bold{debt} variable in argument \code{realized_data}.}
\item{i}{\code{character(1)}. Name of \bold{nominal interest rate} variable in argument \code{realized_data}.}
\item{ti}{\code{character(1)}. Name of \bold{inflation rate} variable (inflation of the \bold{GDP deflator}) in argument \code{realized_data}.}
\item{tic}{\code{character(1)}. Name of \bold{inflation rate} variable (inflation of the \bold{consumer price index}) in argument \code{realized_data}. Defaults to NULL and, if informed, argument \code{r_tic} must also be informed.}
\item{r}{\code{character(1)}. Name of \bold{real interest rate} variable (computed with respect to the \bold{GDP deflator}) in argument \code{realized_data}.}
\item{r_tic}{\code{character(1)}. Name of \bold{real interest rate} variable (computed with respect to a \bold{consumer price index}) in argument \code{realized_data}. Defaults to NULL and, if informed, argument \code{tic} must also be informed.}
\item{g}{\code{character(1)}. Name of \bold{real GDP growth} variable in argument \code{realized_data}.}
\item{p}{\code{character(1)}. Name of \bold{primary balance} variable in argument \code{realized_data}.}
}

\details{

\bold{The formula for the real interest rate} is the usual one:
\deqn{r_t = \frac{(1+i_t)}{(1+\pi_t)}}
where \eqn{r_t} is real interest rate at period \eqn{t}; \eqn{i_t} is nominal interest rate at period \eqn{t}; and \eqn{\pi_t} is percent change in GDP deflator at period \eqn{t}.

\bold{The formula for debt} is the usual debt dynamics equation:
\deqn{d_t = \frac{(1+r_t)}{(1+g_t)}d_{t-1} - pb_t}
where \eqn{d_t} is debt in percent of GDP at period \eqn{t}; \eqn{g_t} is real GDP growth at period \eqn{t}; and \eqn{pb_t} is primary balance, in percent of GDP, at period \eqn{t}.

\bold{If arguments} \code{tic} \bold{and} \code{r_tic} \bold{are not NULL}, the \code{dde} function will
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

}

\value{
A \code{list} with an \code{xts} object named \code{paths}. Such \code{xts} object contains a copy of argument \code{realized data}, merged to newly computed values for debt and real interest rates. The computed series names and meanings are the following:
\enumerate{
\item{\code{d} plus \bold{\dQuote{(simulated, fixed init. cond.)}}: debt computed by means of the debt dynamics equation, with \emph{fixed} initial condition (realized debt at the first period). \bold{This is the main output of the function}.}

\item{\code{d} plus \bold{\dQuote{(simulated, moving init. cond.)}}: debt computed by means of the debt dynamics equation, with \emph{moving} initial condition (realized debt of the immediately previous period).
	\itemize{
		\item{Such debt series is \bold{auxiliary}, to help evaluate how much of the deviations observed between realized data, on the one hand, and the first series above (\dQuote{simulated, \emph{fixed} init. cond.}), on the other hand, can be attributed to the accumulation of errors committed by the debt dynamics equation.}
		\item{In other words, debt simulated with a \emph{moving} initial condition is useful to approximate the stock-flow adjustments each year, as measured by the difference between realized debt, on the one hand, and simulated debt with \emph{moving} initial condition, on the other hand.}
	}
}

\item{\code{r} plus \bold{\dQuote{(simulated, fixed init. cond.)}}: real interest rate computed from nominal interest rates and \bold{GDP deflator} (arguments \code{i} and \code{ti}, respectively).}

\item{\code{r_tic} plus \bold{\dQuote{(simulated, fixed init. cond.)}}: if arguments \code{tic} and \code{r_tic} are not NULL, this is the real interest rate computed from nominal interest rates and \bold{consumer price inflation} (arguments \code{i} and \code{tic}, respectively).}
}
}
\examples{
## EXAMPLE A: WITHOUT consumer price index (i.e. without arguments "tic" and "r_tic")
###################################################################################

x <- dde(
  realized_data = sample_series$realized_yearly_data,
  stock_flow_adjustments = sample_series$realized_yearly_sfa_data,
  d = "debt",
  i = "nominal interest rate",
  ti = "gdp deflator",
  r = "real interest rate (w.r.t. gdp deflator)",
  g = "real gdp growth",
  p = "primary balance"
)

## EXAMPLE B: WITH consumer price index (i.e. with arguments "tic" and "r_tic")
###################################################################################

y <- dde(
  realized_data = sample_series$realized_yearly_data,
  stock_flow_adjustments = sample_series$realized_yearly_sfa_data,
  d = "debt",
  i = "nominal interest rate",
  ti = "gdp deflator",
  tic = "consumer price index",                               # <-- consumer price index here
  r = "real interest rate (w.r.t. gdp deflator)",
  r_tic = "real interest rate (w.r.t. consumer price index)", # <-- real interest rate w.r.t. consumer price index here
  g = "real gdp growth",
  p = "primary balance"
)
}
