#' Parse lending rates from cbr
#'
#' Parse lending rates from cbr
#'
#' Parse lending rates from cbr
#'
#' @param access_date date of access is appended to every observation
#' 
#' @return tsibble
#' @export
#' @examples
#' lend_rate <- parse_lendrate()
parse_lendrate <- function(access_date = Sys.Date()) {
  url = "http://www.cbr.ru/hd_base/mkr/mkr_monthes/"
  lendrate <- url %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = '//*[@id="content"]/table[1]') %>%
    rvest::html_table()
  
  # observations are stored in reverse chronological order :)
  lendrate <- lendrate[[1]] %>% dplyr::as_tibble() %>% dplyr::arrange(-dplyr::row_number())
  
  colnames(lendrate) = c("date", "dur_1_day", "dur_2_7_days", "dur_8_30_days", "dur_31_90_days", "dur_91_180_days", "dur_181_plus_days")
  
  first_date = lubridate::dmy(paste0("1 ", head(lendrate$date, 1)))
  
  lendrate = dplyr::mutate_at(lendrate, dplyr::vars(dplyr::starts_with("dur")), ~ as_numeric_cyrillic(.))
  
  # we convert "сентябрь 2001" to "2001-09-01"
  # but dmy wants "мая" and not "май"
  lendrate = dplyr::mutate(lendrate, date = tsibble::yearmonth(lubridate::dmy(paste0("1 ", stringr::str_replace(date, "май", "мая")))))
  
  lendrate_tsibble <- tsibble::as_tsibble(lendrate, index = "date") 
  return(lendrate_tsibble)
}