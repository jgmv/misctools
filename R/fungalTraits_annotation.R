#' Assigns Fungal Guilds using FungalTraits database
#'
#' Assigns fungal guilds using the FungalTraits database (https://doi.org/10.1007/s13225-020-00466-2) by comparing genus names. In the case of Glomeromycota, assignment to arbuscular mycorrhizal lifestyles can be optionally done at the phylum level.
#' @param x a data frame with taxonomy data.
#' @param print_missing whether taxa not found in the database should be displayed. 
#' @param amf whether Glomeromycota are assigned to arbuscular mycorrhizas at the phylum level.
#' @param coltaxa names of columns with genus and phylum, respectively.
#' @return data frame
#' @keywords fungal guild
#' @export
#' @examples
#' fungalTraits_annotation(example_taxonomy)

fungalTraits_annotation <- function(x, print_missing = F, amf = T,
  coltaxa = c("genus", "phylum")) {
  for(i in colnames(x)) {
    x[, i] <- base::enc2utf8(x[, i])
  }
  ft <- fungaltraits
  ft <- ft[!base::duplicated(ft$genus), ]
  if(print_missing) {
    nf <- unique(x[!(x$genus %in% ft$genus), ])
    if(length(nf) > 0) {
      message("Genera not found:")
      print(nf)
    }    
  }
  lf1 <- ft$primary_lifestyle[match(x[, coltaxa[1]], ft$genus)]
  lf2 <- ft$secondary_lifestyle[match(x[, coltaxa[1]], ft$genus)]
  lf1[lf1 == ""] <- NA
  lf2[lf2 == ""] <- NA
  if(amf) {
    lf1[x[, coltaxa[2]] == "Glomeromycota" & is.na(lf2)] <-
        "arbuscular_mycorrhizal"
    lf2[x[, coltaxa[2], ] == "Glomeromycota" & is.na(lf2)] <-
        "root-associated"  
  }
  result <- cbind(x, primary_lifestyle = lf1, secondary_lifestyle = lf2)
  return(result)
}
