#' Parse exchange rates from cbr
#'
#' Parse exchange rates from cbr
#'
#' Parse exchange rates from cbr
#' Written by Maxim Alekseev
#'
#' @param access_date date of access is appended to every observation
#' 
#' @return tsibble
#' @export
#' @examples
#' # exch_rate <- parse_exchangerate()
parse_exchangerate <- function(access_date = Sys.Date()) {
  df <- cbr::cbr_currency(currency = 'R01235', 
                          from = '1991-01-01', to = access_date)
  colnames(df)[4] <- 'currency'
  df <- dplyr::mutate(df, exch_rate = R01235 * units)
  df <- dplyr::select(df, date, exch_rate)
  data_tsibble <- tsibble::as_tsibble(df, index = date) 
  data_tsibble = dplyr::mutate(data_tsibble, access_date = access_date)
  return(data_tsibble)
}

