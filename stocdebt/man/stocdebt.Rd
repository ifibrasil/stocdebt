\name{stocdebt}
\alias{stocdebt}
\title{Simulation of stochastic scenarios for public debt and its determinants}
\description{

Package \code{stocdebt} is useful to
\itemize{
	\item{simulate stochastic scenarios for public debt and its determinants (real GDP growth, inflation, primary balance and interest rates).}
	\item{estimate the probability of events related to public debt.}
}

Suggestions and bug reports can be submitted to ifi@senado.leg.br or to \url{https://github.com/ifibrasil/stocdebt}.

When the package loads after the user issues \code{library(stocdebt)}, the package looks for a Python installation in the computer (by means of its internal \code{.onLoad()} function, not visible to the user). Python is necessary only to export charts to xlsx files. Thus, even if there is no Python installation, all other features will work appropriately.

To see the source code of \code{stocdebt} with \bold{comments}, for easier understanding of the purpose of each chunk, users are encouraged to visit \url{https://github.com/ifibrasil/stocdebt}. While the R source code in the packaged version excludes comments, the version available on GitHub preserves them.

}
