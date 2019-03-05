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
  colnames(table) <- c("date", "percent_to_period_last_year", "percent_to_last_period")
  table <- dplyr::filter(table, !grepl("квартал", date), !grepl("Год", date), !grepl("год", date))
  
  table$date <- lubridate::ymd("2008-01-01") + months(0:(nrow(table) - 1)) 
  table$access_date <- access_date
  
  table <- mutate_at(table, vars(starts_with("percent")), as_numeric_cyrillic)

  ts_frame <- tsibble::as_tsibble(table, index = date)
  return(ts_frame)
}


#' Converts 1-nn file from rosstat to tibble
#'
#' Converts 1-nn file from rosstat to tibble
#'
#' Converts 1-nn file from rosstat to tibble.
#' Written by: Rifat Enileev
#'
#' @param path_to_source name of the original 1-nn.doc file
#' @param access_date date of access is appended to every observation
#' 
#' @return tibble
#' @export
#' @examples
#' # no yet
convert_1_nn_doc <- function(path_to_source, access_date = Sys.Date()) {
  tbl <- docxtractr::read_docx(path_to_source)
  table_1 <- docxtractr::docx_extract_tbl(tbl, tbl_number = 1, header = TRUE,
                                          preserve = FALSE, trim = FALSE)
  
  ncols <- ncol(table_1)
  if (ncols == 18) {
    colnames(table_1) <- c(
      "year", "yearly", "i", "ii", "iii", "iv", "jan", "feb", "mar", "apr",
      "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"
    )
  } else if (ncols == 13) {
    colnames(table_1) <- c(
      "year", "dec", "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug",
      "sep", "oct", "nov"
    )
    table_1 <- table_1[c(1, 3:13, 2)]
  } else {
    stop("Wrong number of columns in parsed doc: ", path_to_source)
  }
  
  table_1 <- subset(table_1, year >= 1999, select = c(
    "jan", "feb", "mar", "apr", "may", "jun",
    "jul", "aug", "sep", "oct", "nov", "dec"
  ))
  
  table_1$jan <- sub(",", ".", table_1$jan, fixed = TRUE)
  table_1$feb <- sub(",", ".", table_1$feb, fixed = TRUE)
  table_1$mar <- sub(",", ".", table_1$mar, fixed = TRUE)
  table_1$apr <- sub(",", ".", table_1$apr, fixed = TRUE)
  table_1$may <- sub(",", ".", table_1$may, fixed = TRUE)
  table_1$jun <- sub(",", ".", table_1$jun, fixed = TRUE)
  table_1$jul <- sub(",", ".", table_1$jul, fixed = TRUE)
  table_1$aug <- sub(",", ".", table_1$aug, fixed = TRUE)
  table_1$sep <- sub(",", ".", table_1$sep, fixed = TRUE)
  table_1$oct <- sub(",", ".", table_1$oct, fixed = TRUE)
  table_1$nov <- sub(",", ".", table_1$nov, fixed = TRUE)
  table_1$dec <- sub(",", ".", table_1$dec, fixed = TRUE)
  
  table <- dplyr::mutate_all(table_1, function(x) as.numeric(as.character(x))) %>% t() %>% as.data.frame()
  table <- tidyr::gather(table)[2]
  table[table == ""] <- NA
  table <- stats::na.omit(table)
  
  data_ts <- stats::ts(table, start = c(1999, 1), freq = 12)
  data_tsibble <- tsibble::as_tsibble(data_ts)
  data_tsibble <- dplyr::mutate(data_tsibble, access_date = access_date)
  
  return(data_tsibble)
}




#' Converts m2-m2_sa file from rosstat to tibble
#'
#' Converts m2-m2_sa file from rosstat to tibble
#'
#' Converts m2-m2_sa file from rosstat to tibble.
#' Written by: Petr Garmider
#'
#' @param path_to_source name of the original m2-m2_sa.xlsx file
#' @param access_date date of access is appended to every observation
#' 
#' @return tibble
#' @export
#' @examples
#' # no yet
convert_m2_m2_sa_xlsx <- function(path_to_source, access_date = Sys.Date()) {
  data <- rio::import(path_to_source)
  data <- data.frame(data)
  colnames(data)[3] <- "m2sa"
  
  data_vector <- as.numeric(stats::na.omit(t(data$m2sa[2:length(data$m2sa)])))
  
  data_ts <- stats::ts(data_vector, start = c(1995, 7), freq = 12)
  
  data_tsibble <- tsibble::as_tsibble(data_ts) %>% dplyr::rename(date = index)
  
  data_tsibble <- dplyr::mutate(data_tsibble, access_date = access_date)
  return(data_tsibble)
}




#' Converts ind_okved2 file from rosstat to tibble
#'
#' Converts ind_okved2 file from rosstat to tibble
#'
#' Converts ind_okved2 file from rosstat to tibble.
#' Written by: Nastya Jarkova
#'
#' @param path_to_source name of the original ind_okved2.xlsx file
#' @param access_date date of access is appended to every observation
#' 
#' @return tibble
#' @export
#' @examples
#' # no yet
convert_ind_okved2_xlsx <- function(path_to_source, access_date = Sys.Date()) {
  indprod <- rio::import(path_to_source, skip = 2, sheet = 1)
  indprod_vector <- t(indprod[2, 3:ncol(indprod)])
  
  indprod_ts <- stats::ts(indprod_vector, start = c(2013, 1), frequency = 12)
  indprod_tsibble <- tsibble::as_tsibble(indprod_ts)
  indprod_tsibble = dplyr::rename(indprod_tsibble, date = index, ind_prod = value)
  indprod_tsibble = dplyr::mutate(indprod_tsibble, access_date = access_date)
  
  return(indprod_tsibble)
}


#' Converts trade.xls file from cbr to tibble
#'
#' Converts trade.xls file from cbr to tibble
#'
#' Converts trade.xls file from cbr to tibble.
#' Written by: Maxim Alekseev
#'
#' @param path_to_source name of the original trade.xls file
#' @param access_date date of access is appended to every observation
#' 
#' @return tibble
#' @export
#' @examples
#' # no yet
convert_trade_xls <- function(path_to_source, access_date = Sys.Date()) {
  data <- rio::import(path_to_source)
  colnames(data)[c(1, 2, 8)] <- c('date', 'import', 'export')
  namelist <- c('январь', 'февраль', 'март', 'апрель', 'май', 'июнь', 'июль',
                'август', 'сентябрь', 'октябрь', 'ноябрь', 'декабрь')
  data <- dplyr::filter(data, date %in% namelist)
  data_ts <- dplyr::select(data, import, export) %>% stats::ts(start = c(1997, 1), freq = 12)
  data_tsibble <- tsibble::as_tsibble(data_ts, gather = FALSE)
  data_tsibble = dplyr::mutate(data_tsibble, access_date = access_date)
  return(data_tsibble)
}

#' Converts tab2.29.xls file from cbr to tibble
#'
#' Converts tab2.29.xls file from cbr to tibble
#'
#' Converts tab2.29.xls file from cbr to tibble.
#' Written by: Maxim Alekseev
#'
#' @param path_to_source name of the original tab2.29.xls file
#' @param access_date date of access is appended to every observation
#' 
#' @return tibble
#' @export
#' @examples
#' # no yet
convert_tab229_xls <- function(path_to_source, access_date = Sys.Date()) {
  data <- rio::import(path_to_source)
  data_vector <- data[4:24, 5] # WILL WORK IN THE FUTURE????
  data_ts <- stats::ts(data_vector, start = c(2017, 1), freq = 12)
  data_tsibble <- tsibble::as_tsibble(data_ts)
  data_tsibble = dplyr::mutate(data_tsibble, access_date = access_date)
  return(data_tsibble)
}



