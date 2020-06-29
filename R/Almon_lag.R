#' Constructs Almon lag polynomial
#'
#' Constructs Almon lag polynomial
#'
#' Function constructs Almon lag polynomial as in the article by
#' Matteo Mogliani Bayesian MIDAS Penalized Regressions:
#' Estimation, Selection, and Prediction 2 Jan 2020
#' Enables to put endpoint restrictions on the value and on slope of the lag polynomial default R=c(1,1)
#' Meaning that restrictions jointly constrain the weighting structure to tail off slowly to zero
#'
#' @param polydegree degree of polynomial
#' @param C number of columns in Q matrix (???)
#' @param R two numbers (???)
#' @return Q matrix
#' @export
#' @examples

Almon_lag = function(polydegree, C, R) {

  names(R) = c('fC', 'dfC')

  base_seq = 0:(C - 1)

  if (polydegree == 2) {
    if (R['fC'] == 0 && R['dfC'] == 0) {
      # Unrestricted Almon lag polynomial
      Q = matrix(0, polydegree + 1, C)
      for (ii in 0:polydegree) {
        Q[ii + 1, ] = base_seq ^ ii
      }
    }
    else if(R['fC'] == 1 && R['dfC'] == 0) {
      # Almon lag polynomial with tail restriction (fC=0)
      Q = matrix(0, polydegree, C)
      for (ii in 0:polydegree) {
        Q[ii, ] = base_seq^ii - (C - 1) ^ ii
      }
    }
    else if (R['fC'] == 0 && R['dfC'] == 1) {
      # Almon lag polynomial with derivative restriction (dfC=0)
      Q = matrix(0, polydegree, C)
      for (ii in 0:polydegree) {
        Q[ii, ] = (base_seq ^ ii - ii * (C - 1) * base_seq) ^ (ii - 1)
      }
    }
    else if (R['fC'] == 1 && R['dfC'] == 1) {
      # Almon lag polynomial with tail and derivative restrictions (fC=0 and dfC=0)
      Q = matrix(0, polydegree - 1, C)
      for (ii in 0:polydegree - 1) { # CHECK THIS -1 MAYBE 0:(polydegree - 1) IS NEEDED
        Q[ii, ] = base_seq ^ (ii + 1) - (ii + 1) * (C - 1) * base_seq + (C - 1) ^ (ii + 1)
      }
    }
  }
  else if (polydegree == 3) {
    if (R['fC'] == 0 && R['dfC'] == 0) {
      # Unrestricted Almon lag polynomial
      Q = matrix(0, polydegree + 1, C)
      for (ii in 0:polydegree) {
        Q[ii + 1, ] = base_seq ^ ii
      }
    }
    else if(R['fC'] == 1 && R['dfC'] == 0){
      # Almon lag polynomial with derivative restriction (dfC=0)
      Q = matrix(0, polydegree, C)
      for (ii in 0:polydegree) {
        Q[ii, ] = base_seq ^ ii - (C - 1) ^ ii
      }
    }
    else if (R['fC'] == 0 && R['dfC'] == 1) {
      # Almon lag polynomial with derivative restriction (dfC=0)
      stop('Restrictions on the lag polynomial not allowed')
    }
    else if(R['fC'] == 1 && R['dfC'] == 1) {
      # Almon lag polynomial with tail and derivative restrictions (fC=0 and dfC=0)
      Q = matrix(0, polydegree - 1, C)
      for (ii in 0:polydegree - 1) { # CHECK THIS -1 MAYBE 0:(polydegree - 1) IS NEEDED
        Q[ii, ] = base_seq ^ (ii + 1) - (ii + 1) * (C - 1) ^ ii * base_seq + ii * (C - 1) ^ (ii + 1)
      }
    }
  }
  return(Q)
}
