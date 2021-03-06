% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/skewness_ret.R
\name{skewness_ret}
\alias{skewness_ret}
\title{A function that calculates skewness of the daily log-returns of the stock assuming method of Moments.}
\usage{
skewness_ret(
  symbol,
  start_date = Sys.Date() - 365,
  end_date = Sys.Date(),
  price_type = "close",
  rounding = 2
)
}
\arguments{
\item{symbol}{Stock symbol.}

\item{start_date}{Start date for gathering the stock prices - default set as one year ago.}

\item{end_date}{End date for gathering the stock prices - default set as the latest available day.}

\item{price_type}{Type of prices that are to be analyzed - decide between 'open' and 'close' (default 'close').}

\item{rounding}{The level of rounding for the output - default set as 2 decimal places.}
}
\value{
The skewness of daily log-returns for the stock is returned - it is a single numerical value.
}
\description{
A function that calculates skewness of the daily log-returns of the stock assuming method of Moments.
}
\examples{
skewness_ret('AAPL')
skewness_ret('MSFT', '2019-02-01', '2021-03-31', 'open', 3)
}
\author{
Kamil Golis
}
