#' Converts strings into dates in Russian language
#'
#' Converts strings into dates in Russian language
#'
#' Converts strings into dates in Russian language
#'
#' @param date date in the following format: "2019-02-23"
#' @param freq equal to 4 for quarters, equal to 12 for months
#' @return date in the form of the string
#' @export
#' @examples
#' date <- c("2019-02-23", "2011-01-07")
#' date_to_string(date, freq = 12)
date_to_string <- function(date, freq = 12) {
  date_splited <- stringr::str_split(date, "-", simplify = TRUE)
  if (freq == 12) {
    date_month <- dplyr::case_when(
      date_splited[, 2] == "01" ~ "январь",
      date_splited[, 2] == "02" ~ "февраль",
      date_splited[, 2] == "03" ~ "март",
      date_splited[, 2] == "04" ~ "апрель",
      date_splited[, 2] == "05" ~ "май",
      date_splited[, 2] == "06" ~ "июнь",
      date_splited[, 2] == "07" ~ "июль",
      date_splited[, 2] == "08" ~ "август",
      date_splited[, 2] == "09" ~ "сентябрь",
      date_splited[, 2] == "10" ~ "октябрь",
      date_splited[, 2] == "11" ~ "ноябрь",
      date_splited[, 2] == "12" ~ "декабрь"
    )
    date_string <- stringr::str_c(date_month, date_splited[, 1], sep = " ")
  }
  if (freq == 4) {
    date_quarter <- dplyr::case_when(
      date_splited[, 2] == "01" ~ "I квартал",
      date_splited[, 2] == "04" ~ "II квартал",
      date_splited[, 2] == "07" ~ "III квартал",
      date_splited[, 2] == "10" ~ "IV квартал"
    )
    date_string <- stringr::str_c(date_quarter, date_splited[, 1], sep = " ")
  }
  return(date_string)
}
