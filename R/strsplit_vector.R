#' Splits A Vector Of Strings
#'
#' Splits a vector of strings by a character and returns elements within a position.
#' @param x vector with text strings.
#' @param sep character separating elements.
#' @param i index of elements to retain.
#' @keywords text
#' @keywords split
#' @export
#' @examples
#' x <- c("Fusarium_oxysporum_P1304", "Alternaria_sp._P1555", "Fusarium_oxysporum_f.sp._melonis")
#' strsplit_vector(x, sep = "_", i = 1)
# format_string ----------------------------------------------------------------
strsplit_vector <- function(x, sep, i) {

  x <- as.character(x)
  result <- vapply(strsplit(x, sep), '[', i, FUN.VALUE = character(1))
  return(result)

}

