#' A function that calculates an factual return on a given stock for over a defined time horizon
#'
#' @author Kamil Golis
#' @param symbol Stock symbol.
#' @param start_date Start date for gathering the stock prices - default set as one year ago.
#' @param end_date End date for gathering the stock prices - default set as the latest available day.
#' @param price_type Type of prices that are to be analyzed - decide between 'open' and 'close' (default 'close').
#' @param output_type States how the output is expressed - decide between 'percent' (result presented in percentage) and 'float' (result not modified) - default set as 'percent'.
#' @param rounding The level of rounding for the output - default set as 2 decimal places.
#' @return The factual return on the stock is returned - it is a single numerical value.
#' @examples
#' factual_return('AAPL')
#' factual_return('MSFT', '2020-01-01', '2022-03-31', 'open', 'float', 4)
#' @export

factual_return <-  function(symbol, start_date=Sys.Date()-365, end_date=Sys.Date(), price_type = 'close',
                            output_type = 'percent', rounding = 2) {

  tryCatch(
    {
      as.Date(start_date)
      as.Date(end_date)
      message('Congrats, the dates are defined properly!')
    },
    error=function(d) {
      stop("Define the start and / or end date correctly!")
    },
    warning=function(d) {
      return("There is something not right with your dates, bro...")
    })

  quantmod::getSymbols(symbol,
             verbose = F,
             src = "yahoo",
             from=start_date,
             to=end_date)

  if (!exists(symbol)) {
    stop("Define correctly the symbol! :)")
  } else {
    stock <- get(symbol)
  }

  if (price_type == 'close') {
    prices <- stock[,4]
  } else if (price_type == 'open') {
    prices <- stock[,1]
  } else {
    stock <- NULL
    stop('Choose between close and open prices!')
  }

  prices <- as.numeric(prices)

  if (is.null(stock)) {
    print('Sorry, there is a problem - define what you want to achieve one more time!')
  } else {

    if (output_type == 'percent') {
      factual_ret <- round((prices[length(prices)]-prices[1])/prices[1]*100, rounding)
      return(factual_ret)
    }

    else if (output_type == 'float') {
      factual_ret <- round((prices[length(prices)]-prices[1])/prices[1], rounding)
      return(factual_ret)
    }
    else { return('Try one more time defining the output type properly! :)')}
  }
}
