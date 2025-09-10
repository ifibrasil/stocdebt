\name{sample_series}
\docType{data}
\alias{sample_series}
\title{Sample time series data}

\description{A \code{list} containing four objects with sample data, to be used in examples:
\itemize{
\item{\code{realized_yearly_data}: \code{xts} representing yearly realized (i.e. historic, past) values of relevant time series, like debt, real GDP growth, etc.}
\item{\code{baseline_yearly_scenario}: \code{xts} representing the baseline scenario (future), in yearly frequency, for relevant time series, like debt, real GDP growth, etc.}
\item{\code{realized_quarterly_data}: \code{xts} representing quarterly realized (i.e. historic, past) values of relevant time series, like debt, real GDP growth, etc.}
\item{\code{realized_yearly_sfa_data}: \code{xts} object representing yearly realized (i.e. historic, past) values of stock-flow adjustments for debt.}
}
The objects above have been created from data from three sources:
\itemize{
\item{hypothetical data}
\item{public data from the Central Bank of Brazil (Banco Central do Brasil - BCB)}
\item{public data from the Brazilian Institute of Geography and Statistics (Instituto Brasileiro de Geografia e Estatística - IBGE)}
}
}

\usage{sample_series}
\format{Object of class \code{list}, containing four \code{xts} objects.}
