# update misctools package
update_misctools <- function() {
  
  ### load all functions
  devtools::load_all()

  ### add data
  # example_taxonomy
  example_taxonomy <- utils::read.csv("data-raw/example_taxonomy.csv", h = T,
    sep = ";", row.names = 1)
  usethis::use_data(example_taxonomy, overwrite = TRUE)

  # fungaltraits
  fungaltraits <- utils::read.csv("data-raw/fungaltraits.csv", h = T, sep = ";")
  for(i in colnames(fungaltraits)) {
    fungaltraits[, i] <- base::enc2utf8(fungaltraits[, i])
  }
  usethis::use_data(fungaltraits, overwrite = TRUE)

  ### build documentation
  devtools::document()

  ### run checks
  chck <- devtools::check()
  print(chck)
  if(sum(length(chck[["errors"]]), length(chck[["warnings"]]),
    length(chck[["notes"]])) > 0) {
    stop("Update aborted.")
  }

  ### update DESCRIPTION file and git with version number
  dsc <- base::readLines("DESCRIPTION")
  v0 <- dsc[base::which(grepl("Version: ", dsc))]
  v <- base::strsplit(v0, ".", fixed = TRUE)[[1]]
  v[length(v)] <- as.numeric(v[length(v)]) + 1
  v <- paste(v, collapse = ".")
  base::system(paste0("sed -i 's/", v0, "/", v, "/g' DESCRIPTION"))
  base::system(paste0("git tag -a ", gsub("^Version: ", "v", v), " -m '", v, "'"))

  ### build local binary and manual
  devtools::build()
  #devtools::build_manual()

  ### install locally
  devtools::install()
  
}
