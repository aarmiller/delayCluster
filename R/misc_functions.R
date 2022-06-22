

#' Compute euclidean distance
#'
#' Return the euclidean distance between two points
#'
#' @param a a vector of values
#' @param b a vector of values
#'
#' @export
euclidean <- function(a, b) sqrt(sum((a - b)^2))


#' Safe version of explain from the ICD package
#'
#' Return the name of code of NA
#'
#' @param code An icd-9 or icd-10 code
#'
#' @export
explain_code_safe <- function(code){

  tmp <- icd::explain_code(code)

  if (length(tmp)==0) {tmp <- NA}

  tmp
}

