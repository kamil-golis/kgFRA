#' A function that calculates an annualized standard deviation of daily log-returns for a given stock
#'
#' @author Kamil Golis
#' @param symbol Stock symbol.
#' @param start_date Start date for gathering the stock prices - default set as one year ago.
#' @param end_date End date for gathering the stock prices - default set as the latest available day.
#' @param price_type Type of prices that are to be analyzed - decide between 'open' and 'close' (default 'close').
#' @param output_type States how the output is expressed - decide between 'percent' (result presented in percentage) and 'float' (result not modified) - default set as 'percent'.
#' @param rounding The level of rounding for the output - default set as 2 decimal places.
#' @return The annualized standard deviation of daily log-returns for the stock is returned - it is a single numerical value.
#' @examples
#' annual_stand_dev('AAPL')
#' annual_stand_dev('MSFT', '2019-02-01', '2021-03-31', 'open', 'float', 1)
#' @export

annual_stand_dev <-  function(symbol, start_date=Sys.Date()-365, end_date=Sys.Date(), price_type = 'close',
                              output_type = 'percent', rounding = 2) {

  ret <- get_returns(symbol, start_date, end_date, price_type)

  if (is.null(ret)) {
    print('Sorry, there is a problem - define what you want to achieve one more time!')
  } else {
    n <- length(ret)

    if (output_type == 'percent') {
      annual_std <- round(stats::sd(ret)*sqrt(365)*100, rounding)
      return(annual_std)
    }

    else if (output_type == 'float') {
      annual_std <- round(stats::sd(ret)*sqrt(365), rounding)
      return(annual_std)
    }
    else { return('Try one more time defining the output type properly! :)')}
  }
}
