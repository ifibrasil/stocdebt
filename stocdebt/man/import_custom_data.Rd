\name{import_custom_data}
\alias{import_custom_data}
\title{Import of custom data}
\description{
Imports data from external files. The user can provide realized data, a baseline scenario and/or shocks.
}
\usage{
import_custom_data(
  realized_data = NULL,
  baseline_scenario = NULL,
  shocks = NULL
)
}
\arguments{

\item{realized_data}{\code{list(6)}. A list with elements named as follows.
\itemize{
\item{\code{file_type}: \code{character(1)}. Type of file. Currently, only type \dQuote{xlsx} is implemented.}

\item{\code{file_path}: \code{character(1)}. Path to the file. For example, \dQuote{my_data.xlsx}.}

\item{\code{sheet}: \code{character(1)}. Name of the worksheet, within the xlsx workbook informed in \code{file_path}, containing realized data.}

\item{\code{start}: \code{character(1)} or \code{Date(1)} in format \dQuote{YYYY-MM-DD}. First period of the time series of realized data.}

\item{\code{end}: \code{character(1)} or \code{Date(1)} in format \dQuote{YYYY-MM-DD}. Last period of the time series of realized data.}

\item{\code{frequency}: \code{character(1)}. Frequency of the time series of realized data. Set "year" for yearly, "quarter" for quarterly and "month" for monthly frequencies.}
}
}

\item{baseline_scenario}{\code{list(6)}. A list with elements named as follows.
\itemize{
\item{\code{file_type}: \code{character(1)}. Type of file. Currently, only type \dQuote{xlsx} is implemented.}

\item{\code{file_path}: \code{character(1)}. Path to the file. For example, \dQuote{my_data.xlsx}.}

\item{\code{sheet}: \code{character(1)}. Name of the worksheet, within the xlsx workbook informed in \code{file_path}, containing the baseline scenario.}

\item{\code{start}: \code{character(1)} or \code{Date(1)} in format \dQuote{YYYY-MM-DD}. First period of the time series of the baseline scenario.}

\item{\code{end}: \code{character(1)} or \code{Date(1)} in format \dQuote{YYYY-MM-DD}. Last period of the time series of the baseline scenario.}

\item{\code{frequency}: \code{character(1)}. Frequency of the time series of the baseline scenario. Set "year" for yearly, "quarter" for quarterly and "month" for monthly frequencies.}
}
}

\item{shocks}{\code{list(7)}. A list with elements named as follows.
\itemize{
\item{\code{file_type}: \code{character(1)}. Type of file. Currently, only type \dQuote{xlsx} is implemented.}

\item{\code{file_path}: \code{character(1)}. Path to the file. For example, \dQuote{my_data.xlsx}.}

\item{\code{sctructure}: \code{character(1)}. The way shocks are available in the file. Set \dQuote{all in one sheet} if all shock scenarios are in the same worksheet. Set \dQuote{one per sheet} if distinct shock scenarios are available in distinct worksheets.}

\item{\code{sheets}: \code{character(n)} or \code{integer(n)}. If \code{structure} is \dQuote{all in one sheet}, the name of the worksheet (cannot be its number), within the xlsx workbook informed in \code{file_path}, containing shock scenarios. If \code{structure} is \dQuote{one per sheet}, the names (or numbers) of the worksheets.
}
\item{\code{start}: \code{character(1)} or \code{Date(1)} in format \dQuote{YYYY-MM-DD}. First period of the time series of shocks.}

\item{\code{end}: \code{character(1)} or \code{Date(1)} in format \dQuote{YYYY-MM-DD}. Last period of the time series of shocks.}

\item{\code{frequency}: \code{character(1)}. Frequency of the time series of shocks. Set "year" for yearly, "quarter" for quarterly and "month" for monthly frequencies.}
}
}

}
\details{

The purpose of this function is to import data from external files and, most importantly, prepare it to be readily used as input in simulations. The function converts the imported data into objects of class \code{xts}, suitable for time series analysis.

Throughout this help page, by \dQuote{shock scenario} we mean a multivariate time series, where each variable occurs only once. Thus, each shock scenario contains \bold{one} time series of shocks on GDP growth, \bold{one} time series of shocks on primary balance, and so on.

Some users prefer to have one shock scenario per worksheet in their xlsx files, while other users prefer to store all variables, from all shock scenarios, in a single, large worksheet. The function supports both styles of organization, by means of its argument \code{structure}. In the first case, the argument must be set to \dQuote{all in one sheet}. In the second case, set to \dQuote{one per sheet}.

}
\value{
If the user informed all three arguments (\code{realized data}, \code{baseline_scenario} and \code{shocks}), the function will return:

\enumerate{
\item an \code{xts} object named \code{realized_data}, corresponding to the original realized data, but now prepared to be used by other functions of the package.

\item an \code{xts} object named \code{baseline_scenario}, corresponding to the original baseline scenario, but now prepared to be used by other functions of the package.

\item a \code{list} named \code{shocks} containing as many \code{xts} objects as the number shock scenarios provided by the user. Each \code{xts} object corresponds to one of the original shock scenarios, but now prepared to be used by other functions of the package.
}

If the user did not input a list for one or more of the arguments (\code{realized data}, \code{baseline_scenario} and \code{shocks}), NULL is returned for the corresponding argument.
}
\examples{
## Import a baseline scenario from an xlsx file
\dontrun{
i <- import_custom_data(
  baseline_scenario = list(
    file_type = "xlsx",
    file_path = "my_data.xlsx",
    sheet = "baseline_scenario",
    start="2024-12-01",
    end="2033-12-01",
    frequency="year"
  )
)

## Import (1) realized data, (2) baseline scenario and (3) shocks
## from an xlsx file, where six distinct shock scenarios are available
## across worksheets 3 through 8
i <- import_custom_data(
  realized_data = list(
    file_type = "xlsx",
    file_path = "my_data.xlsx",
    sheet = "realized_data",
    start="2013-12-01",
    end="2023-12-01",
    frequency="year"
  ),
  baseline_scenario = list(
    file_type = "xlsx",
    file_path = "my_data.xlsx",
    sheet = "baseline_scenario",
    start="2024-12-01",
    end="2033-12-01",
    frequency="year"
  ),
  shocks = list(
    file_type = "xlsx",
    file_path = "my_data.xlsx",
    structure = "one per sheet",
    sheets = 3:8,
    start="2024-12-01",
    end="2028-12-01",
    frequency="year"
  )
)
}
}
