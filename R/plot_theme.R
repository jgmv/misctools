#' Default plotting theme for ggplot2
#'
#' Defines default graphical parameters for ggplot2 plots. 
#' @param ... elements from 'theme'
#' @keywords plotting
#' @export
plot_theme <- function(...) {
  require(ggplot2)
  mytheme <- theme_classic() +
    theme(axis.text = element_text(colour = "black", size = 10),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.ticks = element_line(colour = "black"),
      axis.ticks.length = unit(0.1, "cm"),
      strip.background = element_blank(),
      panel.spacing = unit(1, "lines"),
      strip.text.x = element_text(size = 12),
      legend.key.height = unit(0.35, "cm"),
      ...)
  return(mytheme)
}
