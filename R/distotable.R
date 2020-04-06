#' Distance Matrix To Table
#'
#' Converts a distance matrix into a data frame.
#' @param dm Distance matrix.
#' @keywords distance matrix
#' @export
#' @examples
#' dm <- dist(runif(10))
#' print(dm)
#' distotable(dm)
distotable <- function(dm){
  dm <- as.dist(dm, diag = T, upper = T)
  dm <- as.matrix(dm)
  index <- expand.grid(rownames(dm), colnames(dm))
  index <- index[as.vector(lower.tri(dm, diag = F)), ]
  tab <- cbind(index, dm[lower.tri(dm, diag = F)])
  colnames(tab) <- c("Var1", "Var2", "dist")  
  return(tab)
}
