#' Default plotting theme for ggplot2
#'
#' Defines default graphical parameters for ggplot2 plots. 
#' @param ... elements from 'theme'
#' @keywords plotting
#' @export
plot_theme <- function(...) {
  mytheme <- ggplot2::theme_classic() +
    ggplot2::theme(axis.text = ggplot2::element_text(colour = "black", size = 10),
      axis.title.x = ggplot2::element_text(size = 10),
      axis.title.y = ggplot2::element_text(size = 10),
      axis.ticks = ggplot2::element_line(colour = "black"),
      axis.ticks.length = ggplot2::unit(0.1, "cm"),
      strip.background = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(1, "lines"),
      strip.text.x = ggplot2::element_text(size = 12),
      legend.key.height = ggplot2::unit(0.35, "cm"),
      ...)
  return(mytheme)
}
