#' A function that calculates skewness of the daily log-returns of the stock assuming method of Moments.
#'
#' @author Kamil Golis
#' @param symbol Stock symbol.
#' @param start_date Start date for gathering the stock prices - default set as one year ago.
#' @param end_date End date for gathering the stock prices - default set as the latest available day.
#' @param price_type Type of prices that are to be analyzed - decide between 'open' and 'close' (default 'close').
#' @param rounding The level of rounding for the output - default set as 2 decimal places.
#' @return The skewness of daily log-returns for the stock is returned - it is a single numerical value.
#' @examples
#' skewness_ret('AAPL')
#' skewness_ret('MSFT', '2019-02-01', '2021-03-31', 'open', 3)
#' @export

skewness_ret <-  function(symbol, start_date=Sys.Date()-365, end_date=Sys.Date(), price_type = 'close',
                          rounding = 2) {

  ret <- get_returns(symbol, start_date, end_date, price_type)

  if (is.null(ret)) {
    print('Sorry, there is a problem - define what you want to achieve one more time!')
  } else {
    n <- length(ret)

    skew_ret <- round(
      sum((ret-mean(ret))^3/stats::sd(ret)^(3))/n
      , rounding)
    return(skew_ret)
  }
}
