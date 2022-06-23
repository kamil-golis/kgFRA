#' A function that shows Value at Risk defined for a given stock, time horizon, significance level and type of calculation (expressed in %).
#'
#' @author Kamil Golis
#' @param symbol Stock symbol.
#' @param start_date Start date for gathering the stock prices - default set as one year ago.
#' @param end_date End date for gathering the stock prices - default set as the latest available day.
#' @param price_type Type of prices that are to be analyzed - decide between 'open' and 'close' (default 'close').
#' @param rounding The level of rounding for the output - default set as 2 decimal places.
#' @param p Level of significance as decimal - default set as 0.05 (5%).
#' @param h Time horizon for the Expected Shortfall calculation - epxressed in days - default set as 1.
#' @param var_type Type of Expected Shortfall calculation - decide between 'hist' (historical simulation), 'norm' (assuming Normal Distribution), 'tstud' (assuming t-Student Distribution) - default set as 'hist'.
#' @return A 1-element data frame informing about the Value at Risk expressed in % is returned.
#' @examples
#' VaR_view('MSFT')
#' VaR_view('AAPL', '2019-02-01', '2021-03-31', 'open', 3, 0.01, 10, 'norm')
#' @export

VaR_view <- function(symbol, start_date=Sys.Date()-365, end_date=Sys.Date(), price_type = 'close',
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

  var_rdy <- data.frame(unlist(var[1]))

  colnames(var_rdy) <- paste0('Value at Risk - ', name, ', ', h, ' day(s), ', as.character(p*100), '% sig lvl:')
  rownames(var_rdy) <- ''

  return(var_rdy)
}
