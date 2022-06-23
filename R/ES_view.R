#' A function that shows Expected Shortfall defined for a given stock, time horizon, significance level and type of calculation (expressed in %).
#'
#' @author Kamil Golis
#' @param symbol Stock symbol.
#' @param start_date Start date for gathering the stock prices - default set as one year ago.
#' @param end_date End date for gathering the stock prices - default set as the latest available day.
#' @param price_type Type of prices that are to be analyzed - decide between 'open' and 'close' (default 'close').
#' @param rounding The level of rounding for the output - default set as 2 decimal places.
#' @param p Level of significance as decimal - default set as 0.05 (5%).
#' @param h Time horizon for the Expected Shortfall calculation - epxressed in days - default set as 1.
#' @param es_type Type of Expected Shortfall calculation - decide between 'hist' (historical simulation), 'norm' (assuming Normal Distribution), 'tstud' (assuming t-Student Distribution) - default set as 'hist'.
#' @return A 1-element data frame informing about the Expected Shortfall expressed in % is returned.
#' @examples
#' ES_view('MSFT')
#' ES_view('AAPL', '2019-02-01', '2021-03-31', 'open', 3, 0.05, 10, 'norm')
#' @export

ES_view <- function(symbol, start_date=Sys.Date()-365, end_date=Sys.Date(), price_type = 'close',
                    rounding = 2, p = 0.05, h = 1, es_type = 'hist') {

  if (es_type == 'hist'){
    es <- paste0(VaR_hist(symbol, start_date, end_date, price_type, rounding, p, h, output='percent'),'%')
    name <- "Historical Simulation"
  } else if (es_type == 'norm') {
    es <- paste0(VaR_N(symbol, start_date, end_date, price_type, rounding, p, h, output='percent'),'%')
    name <- "Normal Distribution"
  } else if  (es_type == 'tstud') {
    es <- paste0(VaR_t(symbol, start_date, end_date, price_type, rounding, p, h, df=5, output='percent'),'%')
    name <- "t-Student Distribution"
  } else {return("Define the method of calculating ES one more time!")}

  es_rdy <- data.frame(unlist(es[2]))

  colnames(es_rdy) <-  paste0('Expected Shortfall - ', name, ', ', h, ' day(s), ', as.character(p*100), '% sig lvl:')
  rownames(es_rdy) <- ''

  return(es_rdy)
}
