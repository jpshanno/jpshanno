#' Export a png from base graphics
#'
#' By default Liberation Narrow is used as the font family, and the axis text
#' title and labels are scaled to match the output resolution. Axis text is
#' rotated to always be horizontal (las = 1)
#'
#' @param file.name The output location
#' @param plot.code A function or code chunk to create the figure
#' @param res Figure resolution
#' @param width Figure width in pixels
#' @param height Figure height in pixels
#' @param ... Additional par() settings
#'
#' @return
#' @export
#'
#' @examples
pretty_png <-
  function(file.name, plot.code, res = 300, width = 3000, height = 2000,...){
    png(file.name,
        width = width,
        height = height,
        res = res,
        type = "cairo-png",
        antialias = "subpixel")
    jpshanno::load_liberation_fonts()
    par(cex.axis = res/72,
        cex.main = 1.2 * res/72,
        cex.lab = 1.1 * res/72,
        las = 1,
        family = "Liberation",
        mar = c(5.1, 5.1, 2.1, 2.1),
        ...)

    if(is.function(plot.code)){
      do.call(plot_function, args = list())
    } else {
      eval(plot.code)
    }

    dev.off()
  }

#' Auto scale text in ggsave
#'
#' This function autoscales the fonts from X11() windows display at 96ppi to the
#' output resolution specified with dpi.
#'
#' @param filename
#' @param plot
#' @param dpi
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
save_ggplot <-
  function(filename, plot = last_plot(), dpi = 300, ...){

    plot <- plot + theme(text = element_text(size = 36*(300/96)))

    ggplot2::ggsave(filename, plot, dpi, ...)
  }
