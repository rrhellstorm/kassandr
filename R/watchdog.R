
get_watchdog_line = function(source_url, watchdog) {
  if (!(source_url %in% watchdog$url)) {
    stop("The file ", source_url, " is not guarded by watchdog :)")
  }
  watchdog_line = dplyr::filter(watchdog, url == source_url)
  return(watchdog_line)  
}


get_last_version_path = function(source_url, watchdog) {
  watchdog_line = get_watchdog_line(source_url, watchdog)
  download_date = watchdog_line$last_download
  file_name = watchdog_line$file
  last_version_path = paste0("../raw/", download_date, "/", file_name)
  return(last_version_path)
}

replace_extension = function(filename, new_ext = "_converted.csv") {
  new_filename = paste0(tools::file_path_sans_ext(filename), new_ext)
  return(new_filename)
}



get_last_version_download_date = function(source_url, watchdog) {
  watchdog_line = get_watchdog_line(source_url, watchdog)
  download_date = watchdog_line$last_download
  return(download_date)  
}

