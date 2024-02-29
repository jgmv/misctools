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
#' tax <- read.csv(system.file("extdata", "example_taxonomy.csv",
#'   package = "misctools"), h = TRUE, sep = ";")
#' fungalTraits_annotation(tax)

fungalTraits_annotation <- function(x, print_missing = F, amf = T,
  coltaxa = c("genus", "phylum")) {
  ft <- utils::read.csv(system.file("extdata", "fungaltraits.csv",
    package = "misctools"), h = T, sep = ";")
  ft <- ft[!duplicated(ft$genus), ]
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
