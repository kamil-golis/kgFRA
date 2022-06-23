#' A function that calculates Risk Adjusted Performance Measures for a given stock, assuming a certain benchmark and risk-free rate.
#'
#' @author Kamil Golis
#' @param symbol Stock symbol.
#' @param rf Risk-free rate, default set as 0.03, it could be also passed as a vector of rates.
#' @param bench Benchmark used to calculate RAPM, default set as 'NDAQ'.
#' @param start_date Start date for gathering the stock prices - default set as one year ago.
#' @param end_date End date for gathering the stock prices - default set as the latest available day.
#' @param price_type Type of prices that are to be analyzed - decide between 'open' and 'close' (default 'close').
#' @param rounding The level of rounding for the output - default set as 2 decimal places.
#' @return A data frame informing about the Risk Adjusted Performance Measures (Treynor Ratio, Tracking Error, Information Ratio) is returned.
#' @examples
#' rapm('MSFT')
#' rapm('AAPL', 0.05, 'NDAQ', '2019-02-01', '2021-03-31', 'open', 3)
#' @export
#' @importFrom PerformanceAnalytics StdDev TreynorRatio TrackingError InformationRatio

 rapm <- function(symbol, rf = 0.03, bench = 'NDAQ', start_date=Sys.Date()-365, end_date=Sys.Date(), price_type = 'close',
                 rounding = 2) {

  ret_s <- get_returns(symbol, start_date, end_date, price_type)
  ret_b <- get_returns(bench, start_date, end_date, price_type)

  rf_scaled <- rf/sqrt(252)


  if (is.null(ret_s) | is.null(ret_b)) {
    print('Sorry, there is a problem - define what you want to achieve one more time!')
  } else {

    Treynor    <- round(TreynorRatio(ret_s, ret_b, rf_scaled, scale=252), rounding)
    Track_Err  <- round(TrackingError(ret_s, ret_b, scale=252), rounding)
    Info_Ratio <- round(InformationRatio(ret_s, ret_b, scale=252), rounding)

    rapm_table <- data.frame(Treynor, Track_Err, Info_Ratio)
    colnames(rapm_table) <- c("Treynor", "Track_Err", "Info_Ratio")

    return(rapm_table)
  }
}
