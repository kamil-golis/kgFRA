#' A function that plots daily log-returns for a given stock.
#'
#' @author Kamil Golis
#' @param symbol Stock symbol.
#' @param start_date Start date for gathering the stock prices - default set as one year ago.
#' @param end_date End date for gathering the stock prices - default set as the latest available day.
#' @param price_type Type of prices that are to be analyzed - decide between 'open' and 'close' (default 'close').
#' @return A plot depicting daily log-returns of the stock is returned.
#' @examples
#' show_return('AAPL')
#' show_return('MSFT', '2020-01-01', '2022-03-31', 'open')
#' @export

show_return <- function(symbol, start_date=Sys.Date()-365, end_date=Sys.Date(), price_type = 'close') {

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

  if (is.null(stock)) {
    print('Sorry, no plot - define what you want to achieve one more time!')
  } else {
    returns <- diff(log(prices))
    plot(returns,
         main = paste0("Daily log-returns of ", symbol, " (%)"))
  }
}
