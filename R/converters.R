#' Converts I_ipc file from rosstat to tibble
#'
#' Converts I_ipc file from rosstat to tibble
#'
#' Converts I_ipc file from rosstat to tibble
#'
#' @param path_to_source name of the original I_ipc.xls file
#' @param access_date date of access is appended to every observation
#' 
#' @return tibble
#' @export
#' @examples
#' # no yet
convert_i_ipc_xlsx = function(path_to_source, access_date = Sys.Date()) {
  data = rio::import(path_to_source)
  
  data <- data[5:16,-1]
  data <- tidyr::gather(data, year, value)
  data <- dplyr::select(data, -year)
  cpi_ts <- stats::ts(data, start = c(1991, 1), freq = 12)
  cpi_infl <- tsibble::as_tsibble(cpi_ts) %>% stats::na.omit() %>% dplyr::rename(date = index)
  
  data_tsibble = dplyr::mutate(cpi_infl, access_date = access_date)
  return(data_tsibble)
}



#' Converts tab5a file from rosstat to tibble
#'
#' Converts tab5a file from rosstat to tibble
#'
#' Converts tab5a file from rosstat to tibble.
#' The structure is similar to tab6b.
#'
#' @param path_to_source name of the original tab5a.xls file
#' @param access_date date of access is appended to every observation
#' 
#' @return tibble
#' @export
#' @examples
#' # no yet
convert_tab5a_xls = function(path_to_source, access_date = Sys.Date()) {
  data = rio::import(path_to_source)
  
  data_vector <- t(data[5, ]) %>% stats::na.omit() %>% as.numeric()
  
  data_ts <- stats::ts(data_vector, start = c(2011, 1), freq = 4)
  data_tsibble <- tsibble::as_tsibble(data_ts) %>% dplyr::rename(date = index)
  
  data_tsibble = dplyr::mutate(data_tsibble, access_date = access_date)
  return(data_tsibble)
}


#' Converts tab6b file from rosstat to tibble
#'
#' Converts tab6b file from rosstat to tibble
#'
#' Converts tab6b file from rosstat to tibble.
#' The structure is similar to tab5a.
#'
#' @param path_to_source name of the original tab6b.xls file
#' @param access_date date of access is appended to every observation
#' 
#' @return tibble
#' @export
#' @examples
#' # no yet
convert_tab6b_xls = function(path_to_source, access_date = Sys.Date()) {
  data_tsibble = convert_tab5a_xls(path_to_source, access_date)
  return(data_tsibble)
}


#' Converts tab9 file from rosstat to tibble
#'
#' Converts tab9 file from rosstat to tibble
#'
#' Converts tab9 file from rosstat to tibble.
#'
#' @param path_to_source name of the original tab9.xls file
#' @param access_date date of access is appended to every observation
#' 
#' @return tibble
#' @export
#' @examples
#' # no yet
convert_tab9_xls = function(path_to_source, access_date = Sys.Date()) {
  data = rio::import(path_to_source)
  
  data_vector <- t(data[4, ]) %>% stats::na.omit() %>% as.numeric()
  
  gdp_deflator <- stats::ts(data_vector, start = c(1996, 1), freq = 4)
  gdp_deflator <- tsibble::as_tsibble(gdp_deflator) %>% dplyr::rename(date = index)
  
  data_tsibble = dplyr::mutate(gdp_deflator, access_date = access_date)
  return(data_tsibble)
}



#' Converts tab9a file from rosstat to tibble
#'
#' Converts tab9a file from rosstat to tibble
#'
#' Converts tab9a file from rosstat to tibble.
#'
#' @param path_to_source name of the original tab9a.xls file
#' @param access_date date of access is appended to every observation
#' 
#' @return tibble
#' @export
#' @examples
#' # no yet
convert_tab9a_xls = function(path_to_source, access_date = Sys.Date()) {
  data = rio::import(path_to_source)
  
  data <- t(data[5, ]) %>% stats::na.omit() %>% as.numeric()
  
  gdp_deflator <- stats::ts(data, start = c(2012, 1), freq = 4)
  gdp_deflator <- tsibble::as_tsibble(gdp_deflator) %>% dplyr::rename(date = index)
  
  data_tsibble = dplyr::mutate(gdp_deflator, access_date = access_date)
  return(data_tsibble)
}



#' Converts urov_12kv file from rosstat to tibble
#'
#' Converts urov_12kv file from rosstat to tibble
#'
#' Converts urov_12kv file from rosstat to tibble.
#' Written by: Vladimir Omelyusik
#'
#' @param path_to_source name of the original urov_12kv.doc file
#' @param access_date date of access is appended to every observation
#' 
#' @return tibble
#' @export
#' @examples
#' # no yet
convert_urov_12kv_doc <- function(path_to_source, access_date = Sys.Date()) {
  real_world <- docxtractr::read_docx(path_to_source)
  table <- docxtractr::docx_extract_tbl(real_world, 2)
  table <- as.data.frame(table)
  table <- table[-c(1, 2, 74, 75), ] # две строки в начале и две пустые строки
  colnames(table) <- c('date', 'percent_to_period_last_year', 'percent_to_last_period')
  table <- table[!grepl("квартал", table$date),]
  table <- table[!grepl("Год", table$date),]
  table <- table[!grepl("год", table$date),]
  rownames(table) <- 1:nrow(table)
  
  table$date <- lubridate::ymd("2008-01-01") + months(0:(nrow(table) - 1)) 
  table$access_date <- access_date
  
  table <- table[,c(1, 4, 2, 3)]
  colnames(table) <- c('date', 'access_date', 'percent_to_period_last_year', 'percent_to_last_period')
  
  table$percent_to_period_last_year <- sub(",", ".", table$percent_to_period_last_year, fixed = TRUE)
  table$percent_to_last_period <- sub(",", ".", table$percent_to_last_period, fixed = TRUE)
  
  table <- table[!find_duplicates(table, index = date), ]
  ts_frame <- as_tsibble(table, index = date)
  return(ts_frame)
}



