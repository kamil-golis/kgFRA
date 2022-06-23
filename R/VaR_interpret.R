#' A function that shows an interpretation for Value at Risk defined for a given stock, time horizon, significance level and type of calculation.
#'
#' @author Kamil Golis
#' @param symbol Stock symbol.
#' @param start_date Start date for gathering the stock prices - default set as one year ago.
#' @param end_date End date for gathering the stock prices - default set as the latest available day.
#' @param price_type Type of prices that are to be analyzed - decide between 'open' and 'close' (default 'close').
#' @param rounding The level of rounding for the output - default set as 2 decimal places.
#' @param p Level of significance as decimal - default set as 0.05 (5%).
#' @param h Time horizon for the Expected Shortfall calculation - epxressed in days - default set as 1.
#' @param var_type Type of Value at Risk calculation - decide between 'hist' (historical simulation), 'norm' (assuming Normal Distribution), 'tstud' (assuming t-Student Distribution) - default set as 'hist'.
#' @return The interpretation in a form of string (message) is returned.
#' @examples
#' VaR_interpret('MSFT')
#' VaR_interpret('AAPL', '2019-02-01', '2021-03-31', 'open', 3, 0.01, 10, 'norm')
#' @export

VaR_interpret <- function(symbol, start_date=Sys.Date()-365, end_date=Sys.Date(), price_type = 'close',
                          rounding = 2, p = 0.05, h = 1, var_type = 'hist') {

  if (var_type == 'hist'){
    var <- paste0(VaR_hist(symbol, start_date, end_date, price_type, rounding, p, h, output='percent'),'%')
    name <- "Historical Simulation"
  } else if (var_type == 'norm') {
    var <- paste0(VaR_N(symbol, start_date, end_date, price_type, rounding, p, h, output='percent'),'%')
    name <- "Normal Distribution"
  } else if  (var_type == 'tstud') {
    var <- paste0(VaR_t(symbol, start_date, end_date, price_type, rounding, p, h, df=5, output='percent'),'%')
    name <- "t-Student Distribution"
  } else {return("Define the method of calculating VaR one more time!")}

  var_rdy <- unlist(var[1])

  var_mess <- paste0('We are ', as.character((1-p)*100), '% certain, that we are going to lose no more than ',
                     var_rdy, ' on stock ', symbol, ' within next ', as.character(h), ' day(s) (assuming a ', name,
                     ' method for Value at Risk calculation).')

  return(var_mess)
}
