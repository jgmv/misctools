#' Plot Krona chart
#'
#' Plots a Krona chart from a taxonomy data set, with the option of including abundance data.
#' Requires a local install, with system-wide access, of KronaTools (https://github.com/marbl/Krona/tree/master/KronaTools).
#' @param tax data frame with taxonomy data
#' @param cdm community data matrix, or vector with total taxon abundance
#' @param group a groupping vector for samples
#' @param outfile name of the output file
#' @return An html file with a Krona char.
#' @keywords taxonomy
#' @export
#' @example
#' plot_krona(example_taxonomy)

plot_krona <- function(tax, cdm = NULL, group = NULL,
  outfile = "krona_plot.html") {

  if(is.null(cdm)) {
    abund <- cbind(abund = rep(1, nrow(tax)), tax)
  } else if (is.vector(cdm)) {
    abund <- cbind(abund = cdm, tax)
  } else if (is.matrix(cdm)) {
    abund <- cbind(abund = colSums(cdm), tax)
  } else {
    stop("cdm must be a vector or a matrix")
  }
  utils::write.table(abund, file = "temp_taxonomy_total.tsv", sep = "\t",
    quote = F, col.names = F, row.names = F)

  if(!is.null(group) & !is.null(cdm)) {
    f <- 0
    files <- c()
    for(i in unique(group)) {
      f <- f + 1
      cdm_sub <- cdm[group == i, ]
      cdm_sub <- cdm_sub[, colSums(cdm_sub) > 0]
      tax_sub <- tax[colnames(cdm_sub), ]
      abund <- cbind(abund = colSums(cdm_sub), tax_sub)
      utils::write.table(abund, file = paste0("temp_taxonomy", f, ".tsv"),
        sep = "\t", quote = F, col.names = F, row.names = F)
      files <- c(files, paste0("temp_taxonomy", f, ".tsv,", i))
    }
    system(paste("ktImportText temp_taxonomy_total.tsv,total",
      paste(files, collapse = " ")))
  } else {
    system("ktImportText temp_taxonomy_total.tsv")
  }
  system(paste("mv text.krona.html", outfile))
  system("rm temp_taxonomy*.tsv")

}
