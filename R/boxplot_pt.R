#' Boxplots With Data Points
#'
#' Draws boxplots with individual data points
#' @param y response variable.
#' @param x factor.
#' @param jitter_amount value to control the random horizontal spread of data points.
#' @param bp_col color of boxplots.
#' @param bp_lty line type for boxplots.
#' @param notch see ?bxp.
#' @param width see ?bxp.
#' @param varwidth see ?bxp.
#' @param notch.frac see ?bxp.
#' @param border see ?bxp.
#' @param staplewex see ?bxp.
#' @param ... further graphical arguments.
#' @details Returns warnings because argumments are jointly passed to 'boxplot' and 'points'. Will solve later...
#' @keywords boxplot
#' @export
#' @examples
#' y <- runif(10)
#' x <- c(rep("a", 5), rep("b", 5))
#' boxplot_pt(y, x, jitter_amount = 0.1)
boxplot_pt <- function(y, x, jitter_amount = NULL, bp_col = 0, bp_lty = 1,
  notch = F, width = NULL, varwidth = F, notch.frac = 0.5, border = par("fg"),
  staplewex = 0, range = 1.5, ...) {

  options(warn = -1)

  names(y) <- x 
  tab <- data.frame(x = x, y = y)
  x_id <- 1:length(unique(x))
  names(x_id) <- unique(x)
  tab$z <- x_id[as.character(tab$x)]
  tab <- tab[with(tab, order(z)), ]
  y <- tab$y
  names(y) <- tab$x
  x <- tab$z
  names(x) <- tab$x

  plot(y ~ x, type = "n", xaxt = "n", ...)
  axis(1, at = x_id, labels = names(x_id))
  if(!is.null(jitter_amount)) {
    x <- jitter(x, amount = jitter_amount)
  }
  for(i in names(x_id)) {
    boxplot(y[names(y) == i], at = x_id[i], add = T, axes = F,
      col = bp_col, lty = bp_lty, outline = F, notch = notch, width = width,
      varwidth = varwidth, notch.frac = notch.frac, border = border,
      staplewex = staplewex, range = range)
  }
  points(y ~ x, ...)

  options(warn = 0)

}

