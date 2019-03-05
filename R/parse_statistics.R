#' Parse lending rates from cbr
#'
#' Parse lending rates from cbr
#'
#' Parse lending rates from cbr
#' Written by Nastya Jarkova
#'
#' @param access_date date of access is appended to every observation
#' 
#' @return tsibble
#' @export
#' @examples
#' # lend_rate <- parse_lendrate()
parse_lendrate <- function(access_date = Sys.Date()) {
  url = "http://www.cbr.ru/hd_base/mkr/mkr_monthes/"
  lendrate <- url %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = '//*[@id="content"]/table[1]') %>%
    rvest::html_table()
  
  # observations are stored in reverse chronological order :)
  lendrate <- lendrate[[1]] %>% dplyr::as_tibble() %>% dplyr::arrange(-dplyr::row_number())
  
  colnames(lendrate) = c("date", "dur_1_day", "dur_2_7_days", "dur_8_30_days", "dur_31_90_days", "dur_91_180_days", "dur_181_plus_days")
  
  first_date = lubridate::dmy(paste0("1 ", utils::head(lendrate$date, 1)))
  
  lendrate = dplyr::mutate_at(lendrate, dplyr::vars(dplyr::starts_with("dur")), ~ as_numeric_cyrillic(.))
  
  # we convert "сентябрь 2001" to "2001-09-01"
  # but dmy wants "мая" and not "май"
  lendrate = dplyr::mutate(lendrate, date = tsibble::yearmonth(lubridate::dmy(paste0("1 ", stringr::str_replace(date, "май", "мая")))))
  
  lendrate_tsibble <- tsibble::as_tsibble(lendrate, index = "date") 
  return(lendrate_tsibble)
}



#' Parse reserve rates from cbr
#'
#' Parse reserve rates from cbr
#'
#' Parse reserve rates from cbr
#' Written by Petr Garmider
#'
#' @param access_date date of access is appended to every observation
#' 
#' @return tsibble
#' @export
#' @examples
#' # res_rate <- parse_reserverate()
parse_reserverate <- function(access_date = Sys.Date()) {
  url <- "http://www.cbr.ru/hd_base/mrrf/mrrf_m/"

  nfa_cb <- url %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = '//*[@id="content"]/table') %>%
    rvest::html_table(fill = TRUE, head = NA)
  
  nfa_cb <- nfa_cb[[1]]
  
  colnames(nfa_cb) <- c("date", "in_res", "nfa_cb", "for_cur", "sdr", "mvf_pos", "mon_gold")
  
  nfa_cb <- nfa_cb[-(1:2), ]
  nfa_cb <- dplyr::mutate(nfa_cb, date = tsibble::yearmonth(lubridate::dmy(date)))
  nfa_cb <- dplyr::mutate_at(nfa_cb, 2:7, as_numeric_cyrillic)
    
  data_tsibble <- tsibble::as_tsibble(nfa_cb, index = date)
  data_tsibble <- dplyr::mutate(data_tsibble, access_date = access_date)
  return(data_tsibble)
}

