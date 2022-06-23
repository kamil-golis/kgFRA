#' A function that calculates summary statistics for a given stock.
#'
#' @author Kamil Golis
#' @param symbol Stock symbol.
#' @param start_date Start date for gathering the stock prices - default set as one year ago.
#' @param end_date End date for gathering the stock prices - default set as the latest available day.
#' @param price_type Type of prices that are to be analyzed - decide between 'open' and 'close' (default 'close').
#' @param rounding The level of rounding for the output - default set as 2 decimal places.
#' @param output_type States how the output is expressed - decide between 'percent' (result presented in percentage) and 'float' (result not modified) - default set as 'percent'.
#' @return A data frame informing about the Risk Adjusted Performance Measures (Treynor Ratio, Sharpe Ratio, Tracking Error, Information Ratio) is returned.
#' @examples
#' sum_stats_stock('MSFT')
#' sum_stats_stock('AAPL', '2019-02-01', '2021-03-31', 'open', 3, 'float')
#' @export


sum_stats_stock <- function(symbol, start_date=Sys.Date()-365, end_date=Sys.Date(), price_type = 'close',
                            rounding = 2, output_type) {

  ret <- get_returns(symbol, start_date, end_date, price_type)

  if (is.null(ret) ) {
    print('Sorry, there is a problem - define what you want to achieve one more time!')
  } else {

    annualized_return <- paste0(annual_return(symbol, start_date, end_date, price_type, output_type = 'percent', rounding),'%')
    factual_return <- paste0(factual_return(symbol, start_date, end_date, price_type, output_type = 'percent',  rounding),'%')
    annualized_standard_deviation <- paste0(annual_stand_dev(symbol, start_date, end_date, price_type, output_type = 'percent',  rounding),'%')
    maximal_daily_return <- paste0(round(max(ret)*100, rounding),'%')
    minimal_daily_return <- paste0(round(min(ret)*100, rounding),'%')
    skewness <- skewness_ret(symbol, start_date, end_date, price_type, rounding)
    kurtosis <- kurtosis_ret(symbol, start_date, end_date, price_type, rounding)

    sum_stats_table <- data.frame(annualized_return, factual_return, annualized_standard_deviation, maximal_daily_return,
                                  minimal_daily_return, skewness, kurtosis)
    colnames(sum_stats_table) <- c("annual_ret", "factual_ret", "annual_std", "max_ret", "min_ret", "skewness", "kurtosis")

    return(sum_stats_table)
  }
}
