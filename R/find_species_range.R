#' Find species distribution range
#'
#' Calculates the range of a species, in Km.
#' @param x data frame with the coordinates of the species occurrence.
#' @param lon name of data frame column with the longitude data.
#' @param lat of data frame column with the latitude data.
#' @keywords distance
#' @export
#' @examples
#' x <- data.frame(longitude = runif(10, min = -180, max = 180),
#'   latitude = runif(10, min = -90, max = 90))
#' find_species_range(x)
find_species_range <- function(x, lon = "longitude", lat = "latitude") {
  xy <- as.matrix(x[, c(lon, lat)])
  xy <- stats::na.omit(xy)
  xy <- xy[xy[, lon] %in% range(xy[, lon]) | xy[, lat] %in% range(xy[, lat]), ]
  if(nrow(xy) > 0) {
    xy_dist <- sp::spDists(xy, longlat = T)
    result <- max(xy_dist)
  } else {
    result <- NA
  }
  return(result)  
}
