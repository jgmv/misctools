#' Loop to retrieve tanonomic names using myTAI::taxonomy
#'
#' Retrieves taxon names at selected taxonomic ranks from a list of provided taxa
#' @param taxlist a vector of taxonomic names.
#' @param ranks a vector with the taxonomic ranks at which retrieve the names.
#' @param db the source database, as specified by myTAI::taxonomy.   
#' @return A data frame with the taxonomic names.
#' @keywords taxonomy
#' @export
#' @examples
#' get_tax_info(c("Holcus lanatus", "Leontodon hispidus"))
get_tax_info <- function(taxlist, ranks = c("genus", "family", "order",
   "species"), db = "ncbi") {
  for(i in 1:length(taxlist)) {
    message(paste("Retrieving", taxlist[i]))
    if(i == 1) {
      result <- data.frame(taxlist)
      for(r in ranks) result[, r] <- rep(NA, nrow(result))
    }
    try(tax <- myTAI::taxonomy(taxlist[i], db = db,
      output = "classification"), silent = T)
    if(is.na(tax[[1]])[1]) {
      tax <- data.frame(rank = ranks, name = rep(NA, length(ranks)))
    }
    for(r in ranks) result[i, r] <- tax[tax$rank == r, "name"]
  }
  return(result)
}
