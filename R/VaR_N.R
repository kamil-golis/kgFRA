#' A function that calculates Value at Risk and Expected Shortfall assuming normal distribution - defined for a given stock, time horizon and significance level.
#'
#' @author Kamil Golis
#' @param symbol Stock symbol.
#' @param start_date Start date for gathering the stock prices - default set as one year ago.
#' @param end_date End date for gathering the stock prices - default set as the latest available day.
#' @param price_type Type of prices that are to be analyzed - decide between 'open' and 'close' (default 'close').
#' @param output_type States how the output is expressed - decide between 'percent' (result presented in percentage) and 'float' (result not modified) - default set as 'percent'.
#' @param rounding The level of rounding for the output - default set as 2 decimal places.
#' @param p Level of significance as decimal - default set as 0.05 (5%).
#' @param h Time horizon for the Expected Shortfall calculation - epxressed in days - default set as 1.
#' @return A 2-element list containing Value at Risk and Expected Shortfall is returned.
#' @examples
#' VaR_N('AAPL')
#' VaR_N('MSFT', '2019-02-01', '2021-03-31', 'open', 'float', 3, 0.03, 10)
#' @export

VaR_N <-  function(symbol, start_date=Sys.Date()-365, end_date=Sys.Date(), price_type = 'close',
                   output_type = 'percent', rounding = 2, p=0.05, h=1) {

  ret <- get_returns(symbol, start_date, end_date, price_type)

  if (is.null(ret) ) {
    print('Sorry, there is a problem - define what you want to achieve one more time!')
  } else {

    m <- mean(ret)                          # mean of returns
    s <- stats::sd(ret)                            # standard deviation of returns

    if (output_type == 'percent') {
      VaR <- round(
        (-m - s*stats::qnorm(p))*sqrt(h)*100
        , rounding)

      ES <- round(
        (-m + s*(stats::dnorm(stats::qnorm(p))/p))*sqrt(h)*100
        , rounding)
      return(list(VaR_N = VaR, ES_N = ES))
    }


    else if (output_type == 'float') {
      VaR <- round(
        (-m - s*stats::qnorm(p))*sqrt(h)
        , rounding)

      ES <- round(
        (-m + s*(stats::dnorm(stats::qnorm(p))/p))*sqrt(h)
        , rounding)
      return(list(VaR_N = VaR, ES_N = ES))
    }
    else { return('Try one more time defining the output type properly! :)')}
  }
}
