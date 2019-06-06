#' @title transforms all numeric strings to numbers keeping non-numeric
#' @description transforms all numeric strings to numbers keeping non-numeric
#' @details transform all numeric strings to numbers keeping non-numeric, returns a list.
#' @param chr_vector character vector or list
#' @return list with numerics and character 1d objects
#' @export
#' @examples
#' gentle_as_numeric(c("5", "ggg", "'sss'"))
gentle_as_numeric = function(chr_vector) {
  num_vector = suppressWarnings(as.numeric(chr_vector)) # generally it's a bad idea to use suppressWarnings, but this function is ok!
  res_list = as.list(chr_vector)
  res_list[!is.na(num_vector)] = num_vector[!is.na(num_vector)]
  return(res_list)
}
# 

#' @title removes quotes around character vectors
#' @description removes quotes around character vectors
#' @details removes quotes around character vectors.
#' @param chr_vector character vector or list
#' @return character vector with quotes removed
#' @export
#' @examples
#' remove_quotes(c("aaa", "'bbb'"))
remove_quotes = function(chr_vector) {
  quoted = stringr::str_starts(chr_vector, "[']") & stringr::str_ends(chr_vector, "[']")
  chr_vector[quoted] = stringr::str_sub(chr_vector[quoted], start = 2, end = -2)
  return(chr_vector)
}

#' @title converts string with parameter values to tibble
#' @description converts string with parameter values to tibble
#' @details Converts string with parameter values to tibble. 
#' Parameters should be separated by comma. 
#' @param param_string character 
#' @return tibble with columns names equal to parameter names
#' @export
#' @examples
#' param_string_2_tibble("q=3, p=4, v='ml', qur=mlp")
param_string_2_tibble = function(param_string) {
  if (param_string == "") {
    params = tibble::as_tibble(list())
  } else {
    splitted = stringr::str_split(param_string, ",") %>% unlist() %>% stringr::str_trim()
    lhs_rhs = stringr::str_split(splitted, "=") %>% unlist()
    n_pars = length(lhs_rhs) / 2
    rhs = remove_quotes(lhs_rhs[2 * (1:n_pars)])
    lhs = lhs_rhs[2 * (1:n_pars) - 1]
    params = as.list(rhs)
    params = gentle_as_numeric(params)
    names(params) = lhs
    params = tibble::as_tibble(params)
  }
  return(params)
}



#' @title converts string with comma separated numbers to numeric vector
#' @description converts string with comma separated numbers to numeric vector
#' @details converts string with comma separated numbers to numeric vector. 
#' @param numbers_string character, something like '1,2,3,7.65'
#' @return numeric vector
#' @export
#' @examples
#' numbers_string_2_vector("1,2,4.56")
numbers_string_2_vector = function(numbers_string) {
  return(numbers_string %>% stringr::str_split(",") %>% unlist() %>% as.numeric() %>% unique())
}


#' @title unabbreviate vector
#' @description replaces acronyms in vector using dictionary of acronyms
#' @details replaces acronyms in character vector using dictionary of acronyms
#' @param original_vector character vector with acronyms
#' @param acronyms tibble with two character columns: acronym, meaning
#' @return character vector with acronyms replaced by their meaning
#' @export
#' @examples
#' acronyms = tibble::tribble(~acronym, ~meaning,
#'   "FOURIER_M", "s1_12+s2_12+s3_12+s4_12+s5_12+c1_12+c2_12+c3_12+c4_12+c5_12+c6_12",
#'   "FOURIER_Q", "s1_4+c1_4+c2_4",
#'   "TRENDS", "trend_lin+trend_root")
#' unabbreviate_vector(c("aaa", "bbb + FOURIER_M"), acronyms)
unabbreviate_vector = function(original_vector, acronyms) {
  full_vector = original_vector
  for (acro_no in 1:nrow(acronyms)) {
    full_vector = stringr::str_replace_all(full_vector, acronyms$acronym[acro_no], acronyms$meaning[acro_no])
  }
  return(full_vector)
}





#' @title splits comma or plus separated variable names into a character vector
#' @description splits comma or plus separated variable names into a character vector
#' @details splits comma or plus separated variable names into a character vector. 
#' Duplicate variable names are removed. For a vector input just collects all variable names.
#' @param predictors_vector character vector of comma or plus separated variable lists
#' @param acronyms tibble with two character columns: acronym, meaning
#' @param split_by regex expression, comma and plus by default
#' @return character vector with variable names
#' @export
#' @examples
#' split_variable_names(c("aaa+bbb", "ccc,bbb,ddd"))
split_variable_names = function(predictors_vector, acronyms = NULL, split_by = "[\\+,]") {
  if (!is.null(acronyms)) {
    predictors_vector = unabbreviate_vector(predictors_vector, acronyms)
  }
  variable_names = stringr::str_split(predictors_vector, split_by) %>% unlist() %>% unique()
  variable_names = variable_names[variable_names != ""]
  variable_names = stringr::str_trim(variable_names)
  return(variable_names)
}
