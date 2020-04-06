#' Formats Text Strings For Plotting
#'
#' Formats differently words in a set of text strings for plotting.
#' @param s vector indicating the position of words in strings to show in italics.
#' @param b vector indicating the position of words in strings to show in bold-face.
#' @param spt character indicating the separator between words.
#' @param pattern regex pattern to select words to be omitted in formatting.
#' @param add_terms vector with words to be omitted from formatting.
#' @keywords text
#' @keywords format
#' @details At the moment, the maximum length os strings admitted is 10 words. Several terms are excluded by default from formatting: "sp", "sp.", "spp", "spp.", "unclassified", "unidentified", "others".
#' @export
#' @examples
#' x <- c("Fusarium_oxysporum_P1304", "Alternaria_sp._P1555", "Fusarium_oxysporum_f.sp._melonis")
#' format_string(x, i = 1:4, pattern = "P[0-9]", add_terms = "f.sp.")
# format_string ----------------------------------------------------------------
format_string <- function(s, i = NULL, b = NULL, spt = "_",
  sep = " ", pattern = NULL, add_terms = NULL) {

  # modify shorter elements
  for(z in s[stringr::str_count(s, spt) < max(stringr::str_count(s, spt))]) {
    n <- max(stringr::str_count(s, spt)) - stringr::str_count(z, spt)
    s[s == z] <- paste0(z, paste(rep(paste0(spt, "|"), n), collapse = ""))
  }

  # define text chunks
  chunks <- c("a", "b", "c", "d", "f", "g", "h", "i", "j", "k")
  s <- sapply(s, function(x) strsplit(x, spt)[[1]])
  s[s == "|"] <- ""
  n <- nrow(s)
  for(z in 1:n) assign(chunks[z], s[z, ])
 
  # define terms to exclude
  terms <- c("sp", "sp.", "spp", "spp.", "unclassified", "unidentified",
    "others")
  if(!is.null(add_terms)) terms <- c(terms, add_terms)
  if(!is.null(pattern)) {
    pattern_terms <- c()
    for(z in 1:n) {
      pattern_terms <- c(pattern_terms, s[z, ][grep(pattern, s[z, ])])
    }
    if(length(pattern_terms) > 0) terms <- c(terms, pattern_terms)
  }
  terms <- unique(terms)

  # run mixedFontLabel
  out <- switch(n,
    ape::mixedFontLabel(a, sep = sep, italic = i, bold = b,
      always.upright = terms),
    ape::mixedFontLabel(a, b, sep = sep, italic = i, bold = b,
      always.upright = terms),
    ape::mixedFontLabel(a, b, c, sep = sep, italic = i, bold = b,
      always.upright = terms),
    ape::mixedFontLabel(a, b, c, d, sep = sep, italic = i, bold = b,
      always.upright = terms),
    mixedFontLabel(a, b, c, d, e, sep = sep, italic = i, bold = b,
      always.upright = terms),
    ape::mixedFontLabel(a, b, c, d, e, f, sep = sep, italic = i,
      bold = b, always.upright = terms),
    ape::mixedFontLabel(a, b, c, d, e, f, g, sep = sep, italic = i,
      bold = b, always.upright = terms),
    ape::mixedFontLabel(a, b, c, d, e, f, g, h, sep = sep, italic = i,
      bold = b, always.upright = terms),
    ape::mixedFontLabel(a, b, c, d, e, f, g, h, i, sep = sep, italic = i,
      bold = b, always.upright = terms),
    ape::mixedFontLabel(a, b, c, d, e, f, g, h, i, j, sep = sep,
      italic = i, bold = b, always.upright = terms),
    ape::mixedFontLabel(a, b, c, d, e, f, g, h, i, j, k, sep = sep,
      italic = i, bold = b, always.upright = terms))
  return(out)

}
